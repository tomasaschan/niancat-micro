from queue import Queue, Empty
import tornado.websocket as ws
from tornado.ioloop import IOLoop
import json

loop = IOLoop.current()


class NoSuchEvent(RuntimeError):
    def __init__(self, *args, **kwargs):
        RuntimeError(*args, **kwargs)


class EventHandler(object):
    def __init__(self, ws_url):
        self.queue = Queue()
        self.unread_messages = []
        self.ws_connection = ws.websocket_connect(ws_url, on_message_callback=self.on_message_callback)

    def on_message_callback(self, msg):
        self.queue.put_nowait(json.loads(msg))

    def find_unread_message(self, type):
        for i in range(0, len(self.unread_messages)):
            msg = self.unread_messages[i]
            if msg['event'] == type:
                self.unread_messages = self.unread_messages[:i] + self.unread_messages[i+1:]
                return msg
        return None

    def send_message(self, msg):
        loop.add_callback(self.ws_connection.write_message, msg)

    def await(self, event_type, timeout_ms=5000):
        print("EventHandler.await, type={}, timeout_ms={}", event_type, timeout_ms)
        unread_message = self.find_unread_message(event_type)
        if unread_message:
            return unread_message

        wait_ms = 500
        slept = 0
        while slept < timeout_ms:
            try:
                msg = self.queue.get(block=True, timeout=wait_ms / 1000)
                print("EventHandler.await: msg = '{}'".format(msg))
                if msg['event'] == event_type:
                    return msg
                else:
                    self.unread_messages.append(msg)
            except Empty:
                pass
            finally:
                slept += wait_ms
        assert False, "No event of type {} received!".format(event_type)
