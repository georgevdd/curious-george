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


def stop(frame):
  for strip in strips:
    strip.fill(colour(0, 0, 0, 0))


def test_regions(frame):
  gc.collect()
  TOP[:] = colour(0, 0, 0, 10)
  TOP[0] = TOP[-1] = colour(0, 0, 80, 0)
  LEFT[:] = colour(20, 0, 0, 0)
  RIGHT[:] = colour(0, 20, 0, 0)


def chasers(strip, frame):
  n = frame
  strip[n % strip.n] = colour(0, 0, 0, 255)
  strip[(n + strip.n//4) % strip.n] = colour(255, 0, 0, 0)
  strip[(n + strip.n//2) % strip.n] = colour(0, 255, 0, 0)
  strip[(n + 3*(strip.n//4)) % strip.n] = colour(0, 0, 255, 0)


def test_pattern(frame):

  for strip in strips:
#    strip.fill((state.brightness,) * 4)
    chasers(strip, frame)

  strip[0] = colour(0, 255, 0, 0)
  strip[strip.n - 1] = colour(0, 255, 0, 0)

  for strip in strips:
    strips[1][N_TOP - 1] = colour(0, 255, 0, 0)
    strips[1][N_TOP] = colour(0, 255, 0, 0)


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
  mode_fn = None
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
      new_mode_fn = getattr(_this_module, state.mode, None)
      prev_tick = cur_tick
      if new_mode_fn and new_mode_fn is not mode_fn:
        mode_fn = new_mode_fn
      mode_fn(frame)
      for strip in strips:
        strip.write()
      frame = frame + 1
      await asyncio.sleep(0)
    except Exception as e:
      global oops
      oops = e
      raise
