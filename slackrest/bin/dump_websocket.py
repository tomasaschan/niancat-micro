"""
Dump WebSocket messages to stdout.

Usage: python dump_websocket.py <url>
"""

from tornado.websocket import websocket_connect
import tornado.ioloop
import sys

if len(sys.argv) != 2:
    print(__doc__)
    sys.exit(1)

url = sys.argv[1]

def on_message_callback(message):
    print("MESSAGE:", message)

if __name__ == "__main__":
    websocket_connect(url, on_message_callback=on_message_callback)
    tornado.ioloop.IOLoop.instance().start()