from math import floor, log
import machine
import neopixel
import sys
import uasyncio as asyncio
import utime as time

import region
import state
import stats

N_SIDE = 108
N_TOP = 48

strips = [
  neopixel.NeoPixel(machine.Pin(0, machine.Pin.OUT), n=N_SIDE, bpp=4),
  neopixel.NeoPixel(machine.Pin(13, machine.Pin.OUT), n=N_TOP + N_SIDE, bpp=4),
]


SHELVES = [13, 30, 47, 67, 88]


TOP = region.ContiguousRegion(strips[1], 0, N_TOP)
LEFT = region.ContiguousRegion(strips[0], 0, N_SIDE)
RIGHT = region.ContiguousRegion(strips[1], N_TOP, N_TOP + N_SIDE)


def stop(frame):
  for strip in strips:
    strip.fill((0,) * 4)


def test_regions(frame):
  TOP[:] = (0, 0, 0, 10)
  TOP[0] = TOP[-1] = (0, 0, 80, 0)
  LEFT[:] = (20, 0, 0, 0)
  RIGHT[:] = (0, 20, 0, 0)


def chasers(strip, frame):
  n = frame
  strip[n % strip.n] = (0, 0, 0, 255)
  strip[(n + strip.n//4) % strip.n] = (255, 0, 0, 0)
  strip[(n + strip.n//2) % strip.n] = (0, 255, 0, 0)
  strip[(n + 3*(strip.n//4)) % strip.n] = (0, 0, 255, 0)


def test_pattern(frame):

  for strip in strips:
#    strip.fill((state.brightness,) * 4)
    chasers(strip, frame)

  strip[0] = (0, 255, 0, 0)
  strip[strip.n - 1] = (0, 255, 0, 0)

  for strip in strips:
    strips[1][N_TOP - 1] = (0, 255, 0, 0)
    strips[1][N_TOP] = (0, 255, 0, 0)


def ruler(_):
  for strip in LEFT, RIGHT:
    for n in range(len(strip)):
      strip[n] = (0, 0, 127, 0) if ((n // 10) % 2) else (0, 0, 0, 79)
    for n in SHELVES:
      strip[n] = (255, 0, 0, 0)
  TOP[:] = (0, 0, 0, 79)


def rainbow(frame):
  red = (255, 0, 0, 0)
  orange = (255, 127, 0, 0)
  yellow = (255, 255, 0, 0)
  green = (0, 255, 0, 0)
  blue = (0, 127, 255, 0)
  indigo = (0, 0, 255, 0)
  violet = (63, 0, 255, 0)

  TOP[:] = red
  side_colours = [orange, yellow, green, blue, indigo, violet]
  for strip in LEFT, RIGHT:
    for colour, start, stop in zip(side_colours,
                                   [0] + SHELVES,
                                   SHELVES + [len(strip)]):
      strip[start:stop] = colour


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
