"""
Usage: python3 dump_websocket.py <url>
"""

import websocket
import sys

def on_message(ws, message):
    print("MESSAGE:", message)

def on_error(ws, error):
    print("ERROR:", error)

def on_close(ws):
    print("### CLOSED ###")

def on_open(ws):
    print("### OPENED ###")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(__doc__)
        sys.exit(1)

    websocket.enableTrace(True)
    ws = websocket.WebSocketApp(sys.argv[1],
                              on_message=on_message,
                              on_error=on_error,
                              on_close=on_close)
    ws.on_open = on_open
    ws.run_forever()
