import machine
import neopixel
import sys
import uasyncio as asyncio

import state

N_SIDE = 108
N_TOP = 48

strips = [
  neopixel.NeoPixel(machine.Pin(0, machine.Pin.OUT), n=N_SIDE, bpp=4),
  neopixel.NeoPixel(machine.Pin(13, machine.Pin.OUT), n=N_TOP + N_SIDE, bpp=4),
]


SHELVES = [13, 30, 47, 67, 88]


def stop(frame):
  for strip in strips:
    strip.fill((0,) * 4)


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
  for strip in strips:
    for n in range(strip.n):
      strip[n] = (0, 0, 127, 0) if ((n // 10) % 2) else (0, 0, 0, 79)
  for n in SHELVES:
    strips[0][n] = (255, 0, 0, 0)


oops = None


async def run():
  _this_module = sys.modules[__name__]
  mode_fn = None
  frame = 0
  while True:
    try:
      new_mode_fn = getattr(_this_module, state.mode, None)
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
