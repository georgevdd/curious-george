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


class Region:
  """An indexable sequence of pixels in a single strip.

  See https://docs.micropython.org/en/latest/genrst/builtin_types.html
  for some limitations of slicing in MicroPython.
  """

  def __init__(self, strip, indices):
    self.strip = strip
    self.indices = list(indices)

  def __getitem__(self, key):
    if isinstance(key, slice):
      return Region(self.strip, [
        self.indices[n] for n in range(key.start or 0,
                                       key.stop or len(self.indices),
                                       key.step or 1)])
    else:
      return self.strip[self.indices[key]]

  def __setitem__(self, key, value):
    if isinstance(key, slice):
      for n in range(key.start or 0,
                     key.stop or len(self.indices),
                     key.step or 1):
        self.strip[self.indices[n]] = value
    else:
      self.strip[self.indices[key]] = value


TOP = Region(strips[1], range(N_TOP))
LEFT = Region(strips[0], range(N_SIDE))
RIGHT = Region(strips[1], range(N_TOP, N_TOP + N_SIDE))


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
