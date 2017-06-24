"""
Dump WebSocket messages to stdout.

Usage: python dump_websocket.py <host> <port>
"""

import tornado.websocket
import tornado.ioloop
import sys

ioloop = tornado.ioloop.IOLoop.current()

if len(sys.argv) != 3:
    print(__doc__)
    sys.exit(1)

host = sys.argv[1]
port = sys.argv[2]
url = 'ws://{}:{}'.format(host, port)

async def main():
    conn = await tornado.websocket.websocket_connect(url, io_loop=ioloop)
    while True:
        msg = await conn.read_message()
        print("MESSAGE:", msg)

ioloop.run_sync(main)