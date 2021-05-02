#! /usr/local/bin/python3

import os
import http.server
import socketserver

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
  PORT = 8000
  os.chdir('static')
  my_server = socketserver.TCPServer(("", PORT), MyHttpRequestHandler)
  my_server.serve_forever()
