#! /usr/bin/python3
import functools
import os
import http.server
import socketserver
import ssl
import sys
import urllib.request


class MyHttpRequestHandler(http.server.SimpleHTTPRequestHandler):

  def __init__(self, *args, api_server_url: str = '', **kwargs):
    self.api_server_url = api_server_url
    super().__init__(*args, **kwargs)

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

  def do_GET(self):
    if self.path.startswith('/api'):
      self.send_response(200)
      self.end_headers()
      self.copyfile(
        urllib.request.urlopen(self.api_server_url + self.path),
        self.wfile)
    else:
      super().do_GET()

  def do_PUT(self):
    if self.path.startswith('/api'):
      print('proxying...', self.path)
      self.send_response(200)
      self.end_headers()
      data_length = int(self.headers.get('content-length'))
      data=self.rfile.read(data_length)
      print(data)
      self.copyfile(
        urllib.request.urlopen(urllib.request.Request(
            self.api_server_url + self.path,
            method='PUT',
            data=data
          )),
        self.wfile)
    else:
      self.send_response(404)
      self.end_headers()


if __name__ == '__main__':
  api_server_url = sys.argv[1]

  if len(sys.argv) > 2:
    PORT = int(sys.argv[2])
  else:
    PORT = 4443

  secure = PORT not in (80, 8080)

  server_address = ('0.0.0.0', PORT)
  httpd = http.server.HTTPServer(
    server_address,
    functools.partial(
      MyHttpRequestHandler,
      directory='static',
      api_server_url=api_server_url,
      ))
  if secure:
    ssl_context = ssl.SSLContext()
    ssl_context.load_cert_chain(
        certfile='/etc/letsencrypt/live/bookcase.updog.net/fullchain.pem',
        keyfile='/dev/stdin')
    httpd.socket = ssl_context.wrap_socket(
      httpd.socket,
      server_side=True)
  httpd.serve_forever()
