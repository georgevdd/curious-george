# This file is executed on every boot (including wake-boot from deepsleep)
#import esp
#esp.osdebug(None)
import uos, machine
#uos.dupterm(None, 1) # disable REPL on UART(0)
import gc
#import webrepl
#webrepl.start()
gc.collect()

import net_config

def do_connect():
  import network
  sta_if = network.WLAN(network.STA_IF)
  if not sta_if.isconnected():
    print('connecting to network...')
    sta_if.active(True)
    sta_if.connect(net_config.ssid, net_config.password)
    while not sta_if.isconnected():
      pass
    maybe_static_ip = getattr(net_config, 'static_ip', None)
    if maybe_static_ip:
      ip_config = sta_if.ifconfig()
      sta_if.ifconfig((maybe_static_ip,) + ip_config[1:])
    print('network config:', sta_if.ifconfig())

do_connect()
