import errno
import gc
from math import floor, log
import machine
import neopixel
import sys
import uasyncio as asyncio
import usocket as socket
import utime as time

import geopixel
import lights
import state
import stats


strips = lights.strips = [
  geopixel.GeoPixel(machine.Pin(pin, machine.Pin.OUT), n=len(strip))
  for pin, strip in zip([0, 13], lights.strips)
]


sock = None


def datagram():
  global sock
  if sock is None:
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('', 5678))
    sock.settimeout(0)

  bytes_per_frame = sum(len(strip.buf) for strip in strips)

  buf = bytearray(bytes_per_frame)
  mv = memoryview(buf)

  while True:
    cursor = 0
    while cursor < bytes_per_frame:
      bytes_read = sock.readinto(mv[cursor:])
      if bytes_read is None:
        yield False
      else:
        cursor += bytes_read

    cursor = 0
    for strip in strips:
      n = strip.n << 2
      geopixel.neopixel_write(strip.pin, mv[cursor:cursor+n], 1)
      cursor += n
    yield False

oops = None


async def run():
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
        new_mode_gen = getattr(lights, mode, None)
        mode_gen = new_mode_gen() if new_mode_gen else iter([])
        del new_mode_gen
        gc.collect()

      try:
        should_write = next(mode_gen)
      except StopIteration:
        mode_gen = None
        should_write = False
        gc.collect()

      if should_write is not False:
        for strip in strips:
          strip.write()
      frame = frame + 1
      await asyncio.sleep(0)
    except Exception as e:
      global oops
      oops = e
      raise


def test_loop():
  loop = asyncio.get_event_loop()
  loop.create_task(run())
  loop.run_forever()
