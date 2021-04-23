import esp8266
import lights

import uasyncio
uasyncio.create_task(lights.run())

esp8266.run()
