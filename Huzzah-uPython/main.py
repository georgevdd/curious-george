import api
import run_lights

import uasyncio
uasyncio.create_task(run_lights.run())

api.run()
