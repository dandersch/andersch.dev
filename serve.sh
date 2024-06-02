#!/usr/bin/python

import http.server
import socketserver

PORT = 1337

class HttpRequestHandler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        '':      'text/html',
        '.':     'text/html',
        '.html': 'text/html',
        '.png':  'image/png',
        '.jpg':  'image/jpg',
        '.svg':	 'image/svg+xml',
        '.css':	 'text/css',
    }

httpd = socketserver.TCPServer(("localhost", PORT), HttpRequestHandler)

try:
    print(f"serving at http://localhost:{PORT}")
    httpd.serve_forever()
except KeyboardInterrupt:
    httpd.shutdown()
    pass
