import api
import lights

import uasyncio
uasyncio.create_task(lights.run())

api.run()
