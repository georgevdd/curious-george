import esp8266


import uasyncio

async def blink(led, period_ms):
    while True:
        led.on()
        await uasyncio.sleep_ms(period_ms)
        led.off()
        await uasyncio.sleep_ms(period_ms)


import machine

uasyncio.create_task(blink(machine.Pin(0, machine.Pin.OUT), 700))

esp8266.run()
