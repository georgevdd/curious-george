import machine
import neopixel
import uasyncio as asyncio

strips = [
  neopixel.NeoPixel(machine.Pin(0, machine.Pin.OUT), n=108, bpp=4),
]

async def blink(led, period_ms):
    while True:
        led.on()
        await asyncio.sleep_ms(period_ms)
        led.off()
        await asyncio.sleep_ms(period_ms * 2)

async def run():
  strip = strips[0]
  while True:
    for n in range(strip.n):
      await asyncio.sleep_ms(20)
      strip.fill((0, 0, 0, 0))
      strip[n] = (255, 0, 0, 255)
      strip[(n + strip.n//2) % strip.n] = (255, 0, 0, 0)
      strip.write()
