import gc
from math import floor, log
import machine
import neopixel
import sys
import uasyncio as asyncio
import utime as time

import geopixel
import region
import state
import stats

N_SIDE = 108
N_TOP = 48

strips = [
  geopixel.GeoPixel(machine.Pin(0, machine.Pin.OUT), n=N_SIDE),
  geopixel.GeoPixel(machine.Pin(13, machine.Pin.OUT), n=N_TOP + N_SIDE),
]

colour = geopixel.colour

# Offsets of each shelf from the top, in pixels.
SHELVES = [13, 30, 47, 67, 88]


TOP = region.ContiguousRegion(strips[1], 0, N_TOP)
LEFT = region.ContiguousRegion(strips[0], 0, N_SIDE)
RIGHT = region.ContiguousRegion(strips[1], N_TOP, N_TOP + N_SIDE)


def stop():
  for strip in strips:
    strip.fill(colour(0, 0, 0, 0))


def test_regions():
  TOP[:] = colour(0, 0, 0, 10)
  TOP[0] = TOP[-1] = colour(0, 0, 80, 0)
  LEFT[:] = colour(20, 0, 0, 0)
  RIGHT[:] = colour(0, 20, 0, 0)


red = colour(255, 0, 0, 0)
green = colour(0, 255, 0, 0)
blue = colour(0, 0, 255, 0)
white = colour(0, 0, 0, 255)

def chasers(strip, frame):
  n = frame
  sn = len(strip)
  strip[n % sn] = white
  strip[(n + sn//4) % sn] = red
  strip[(n + sn//2) % sn] = green
  strip[(n + 3*(sn//4)) % sn] = blue


def test_pattern():
  frame = 0
  while True:
    n = frame
    for strip in LEFT, RIGHT, TOP:
      sn = strip.stop - strip.start
      strip[n % sn] = white
      strip[(n + sn//4) % sn] = red
      strip[(n + sn//2) % sn] = green
      strip[(n + 3*(sn//4)) % sn] = blue

      strip[0] = green
      strip[-1] = green
    frame = frame + 1
    yield


def ruler():
  red = colour(255, 0, 0, 0)

  c1 = None
  c2 = None
  while True:
    if c1 == state.colour1 and c2 == state.colour2:
      yield
      continue
    c1 = state.colour1
    c2 = state.colour2

    c1_ = colour(*c1)
    c2_ = colour(*c2)

    for strip in LEFT, RIGHT:
      b = False
      for n in range(0, len(strip), 10):
        strip[n:n+10] = c1_ if b else c2_
        b = not b
      for n in SHELVES:
        strip[n] = red
    TOP[:] = c2_


def rainbow():
  w = None
  while True:
    if w == state.brightness:
      yield
      continue
    w = state.brightness

    red = colour(255, 0, 0, w)
    orange = colour(255, 127, 0, w)
    yellow = colour(255, 255, 0, w)
    green = colour(0, 255, 0, w)
    blue = colour(0, 127, 255, w)
    indigo = colour(0, 0, 255, w)
    violet = colour(63, 0, 255, w)

    TOP[:] = red
    side_colours = [orange, yellow, green, blue, indigo, violet]
    for strip in LEFT, RIGHT:
      for start, stop, c in zip([0] + SHELVES,
                                SHELVES + [len(strip)],
                                side_colours):
        strip[start:stop] = c


def just_red():
  # Paula's fantasy
  w = state.brightness
  red = colour(255, 0, 0, w)
  orange = colour(255, 127, 0, w)
  yellow = colour(255, 255, 0, w)
  TOP[:] = red
  LEFT[:] = red
  RIGHT[:] = red


def colombia():
  w = None
  while True:
    if w == state.brightness:
      yield
      continue
    w = state.brightness

    yellow = colour(255, 255, 0, w)
    blue = colour(0, 0, 255, w)
    red = colour(255, 0, 0, w)
    TOP[:] = yellow
    for side in LEFT, RIGHT:
      boundaries = SHELVES[2], (SHELVES[3]+SHELVES[4])//2
      side[:boundaries[0]] = yellow
      side[boundaries[0]:boundaries[1]] = blue
      side[boundaries[1]:] = red


def lerp(a, b, n):
  l = b - a
  for i in range(n):
    yield a + (l * i) // (n-1)


def clerp(a, b, n):
  g0, r0, b0, w0 = a
  g1, r1, b1, w1 = b
  for r, g, b, w in zip(
      lerp(r0, r1, n),
      lerp(g0, g1, n),
      lerp(b0, b1, n),
      lerp(w0, w1, n)):
    yield colour(r, g, b, w)


def sunset():
  w = 0  #state.brightness

  c1 = None
  c2 = None
  while True:
    if c1 == state.colour1 and c2 == state.colour2:
      yield
      continue
    c1 = state.colour1
    c2 = state.colour2
    TOP[:] = colour(*c1)
    for side in LEFT, RIGHT:
      #side[:] = clerp(red, yellow, len(side))
      for n, c in enumerate(clerp(colour(*c1), colour(*c2), len(side))):
        side[n] = c


def christmas():
  for strip in [LEFT, RIGHT]:
    for n in SHELVES:
      strip[n-10:n-1] = colour(0, 255, 0, 0)
      strip[n] = colour(255, 0, 0, 0)



oops = None


async def run():
  _this_module = sys.modules[__name__]

  mode = None  # Name of the current mode
  mode_gen = None  # Generator of current mode behaviour

  frame = 0
  prev_tick = None
  log2 = log(2)
  while True:
    try:
      cur_tick = time.ticks_us()
      if prev_tick:
        duration_ticks = time.ticks_diff(cur_tick, prev_tick)
        stats.last_loop_micros[frame %
                               len(stats.last_loop_micros)] = duration_ticks
        stats.log2_loop_micros[floor(log(duration_ticks) // log2)] += 1
      prev_tick = cur_tick

      if mode != state.mode:
        mode = state.mode
        new_mode_gen = getattr(_this_module, mode, None)
        mode_gen = new_mode_gen() if new_mode_gen else None
        del new_mode_gen
        gc.collect()

      if mode_gen:
        try:
          next(mode_gen)
        except StopIteration:
          mode_gen = None
          gc.collect()

      for strip in strips:
        strip.write()
      frame = frame + 1
      await asyncio.sleep(0)
    except Exception as e:
      global oops
      oops = e
      raise
