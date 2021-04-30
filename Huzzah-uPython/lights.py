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

SHELVES = [13, 30, 47, 67, 88]


TOP = region.ContiguousRegion(strips[1], 0, N_TOP)
LEFT = region.ContiguousRegion(strips[0], 0, N_SIDE)
RIGHT = region.ContiguousRegion(strips[1], N_TOP, N_TOP + N_SIDE)


def stop():
  for strip in strips:
    strip.fill(colour(0, 0, 0, 0))

  while True:
    yield


def test_regions(frame):
  gc.collect()
  TOP[:] = colour(0, 0, 0, 10)
  TOP[0] = TOP[-1] = colour(0, 0, 80, 0)
  LEFT[:] = colour(20, 0, 0, 0)
  RIGHT[:] = colour(0, 20, 0, 0)


def test_pattern():
  red = colour(255, 0, 0, 0)
  green = colour(0, 255, 0, 0)
  blue = colour(0, 0, 255, 0)
  white = colour(0, 0, 0, 255)

  #strip.fill((state.brightness,) * 4)

  def chasers(strip, frame):
    n = frame
    sn = len(strip)
    strip[n % sn] = white
    strip[(n + sn//4) % sn] = red
    strip[(n + sn//2) % sn] = green
    strip[(n + 3*(sn//4)) % sn] = blue

  frame = 0
  while True:
    for strip in LEFT, RIGHT, TOP:
      chasers(strip, frame)
      strip[0] = green
      strip[-1] = green
    frame += 1
    yield


def ruler(_):
  red = colour(255, 0, 0, 0)
  blue = colour(0, 0, 127, 0)
  white = colour(0, 0, 0, 79)
  for strip in LEFT, RIGHT:
    b = False
    for n in range(0, len(strip), 10):
      strip[n:n+10] = blue if b else white
      b = not b
    for n in SHELVES:
      strip[n] = red
  TOP[:] = white


def rainbow(frame):
  red = colour(255, 0, 0, 0)
  orange = colour(255, 127, 0, 0)
  yellow = colour(255, 255, 0, 0)
  green = colour(0, 255, 0, 0)
  blue = colour(0, 127, 255, 0)
  indigo = colour(0, 0, 255, 0)
  violet = colour(63, 0, 255, 0)

  TOP[:] = red
  side_colours = [orange, yellow, green, blue, indigo, violet]
  for strip in LEFT, RIGHT:
    for start, stop, c in zip([0] + SHELVES,
                              SHELVES + [len(strip)],
                              side_colours):
      strip[start:stop] = c


oops = None


async def run():
  _this_module = sys.modules[__name__]

  mode = None
  mode_gen = stop()

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
