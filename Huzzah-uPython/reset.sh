#! /usr/local/bin/python3
import os
import websocket

conn = websocket.create_connection('ws://192.168.1.234:8266')
conn.send(os.environ['WEBREPL_PASSWORD'] + '\r')
conn.send('\x03')  # Ctrl-C
conn.send('import machine\r')
conn.send('machine.soft_reset()\r')

try:
  while True:
    print(conn.recv(), end='')
except websocket.WebSocketConnectionClosedException:
  pass
