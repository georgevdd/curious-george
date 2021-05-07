#! /usr/bin/python3

import os
import http.server
import socketserver
import ssl

class MyHttpRequestHandler(http.server.SimpleHTTPRequestHandler):

  def translate_path(self, path):
    path = super().translate_path(path)
    if os.path.exists(path):
      return path
    elif os.path.exists(path + '.gz'):
      return path + '.gz'
    else:
      return path

  def end_headers(self):
    base, ext = os.path.splitext(self.translate_path(self.path))
    if ext == '.gz':
      self.send_header('Content-Encoding', 'gzip')
    super().end_headers()

  def guess_type(self, path):
    base, ext = os.path.splitext(self.translate_path(self.path))
    if ext == '.gz':
      return super().guess_type(base)
    else:
      return super().guess_type(path)


if __name__ == '__main__':
  PORT = 4443
  os.chdir('static')
  server_address = ('0.0.0.0', PORT)
  httpd = http.server.HTTPServer(server_address, MyHttpRequestHandler)
  ssl_context = ssl.SSLContext()
  ssl_context.load_cert_chain(
      certfile='/etc/letsencrypt/live/bookcase.updog.net/fullchain.pem')
      keyfile='/dev/stdin')
  httpd.socket = ssl_context.wrap_socket(
    httpd.socket,
    server_side=True)
  httpd.serve_forever()
