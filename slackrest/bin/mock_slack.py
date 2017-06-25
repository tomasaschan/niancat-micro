"""
Acts as a fake Slack server, allowing for Slack bots to be tested for acceptance.
"""
import tornado.web
import tornado.websocket
import tornado.httpserver
import tornado.ioloop
import json
from queue import Queue
from threading import RLock, Thread

loop = tornado.ioloop.IOLoop.current()
producers_lock = RLock()
test_event_producers = []
event_queue = Queue()

def send_event_thread():
    while True:
        new_event = event_queue.get(block=True)
        with producers_lock:
            for producer in test_event_producers:
                loop.add_callback(producer.write_message, new_event)


class TestEventHandler(tornado.websocket.WebSocketHandler):
    def __init__(self, *args, **kwargs):
        tornado.websocket.WebSocketHandler.__init__(self, *args, **kwargs)

    def open(self):
        with producers_lock:
            test_event_producers.append(self)
        self.write_message({'event': 'hello'})

    def on_message(self, message):
        """Ignore any message sent to the event producer."""
        pass

    def on_close(self):
        with producers_lock:
            test_event_producers.remove(self)




class TestEventApp(tornado.web.Application):
    def __init__(self, *args, **kwargs):
        handlers = [
            (r'/', TestEventHandler)
        ]

        settings = {
            'template_path': 'templates'
        }
        tornado.web.Application.__init__(self, handlers, **settings)


class MockSlackHandler(tornado.websocket.WebSocketHandler):
    def __init__(self, *args, **kwargs):
        tornado.websocket.WebSocketHandler.__init__(self, *args, **kwargs)

    def open(self):
        """A Slack RTM WebSocket connection starts with a hello message."""
        print("Slack: Opened RTM WebSocket connection. Sending Hello message.")
        self.write_message('{ "type": "hello" }')
        login_event = {'event': 'login'}
        event_queue.put_nowait(login_event)

    def on_message(self, message):
        if 'type' in message and message['type'] == 'ping':
            pong = {'type': 'pong', 'reply_to': message['id']}
            loop.add_callback(self.write_message, json.dumps(pong))
        elif 'type' in message and message['type'] == 'message':
            json_message = json.loads(message)
            message_event = {'event': 'message', 'message': json_message}
            event_queue.put_nowait(json.dumps(message_event))

    def on_close(self):
        pass


class RtmHandler(tornado.web.RequestHandler):
    def get(self):
        self.write({'ok': True, 'url': 'http://slack.com/websocket'})
        self.finish()


class MockSlackApp(tornado.web.Application):
    def __init__(self, *args, **kwargs):
        handlers = [
            (r'/api/rtm.start', RtmHandler),
            (r'/websocket', MockSlackHandler),
        ]
 
        settings = {
            'template_path': 'templates'
        }
        tornado.web.Application.__init__(self, handlers, **settings)
 
 
if __name__ == '__main__':
    event_thread = Thread(target=send_event_thread)
    event_thread.start()

    test_event_app = TestEventApp()
    server = tornado.httpserver.HTTPServer(test_event_app)
    server.listen(8080)

    mock_slack_app = MockSlackApp()
    server = tornado.httpserver.HTTPServer(mock_slack_app)
    server.listen(80)

    tornado.ioloop.IOLoop.instance().start()
    event_thread.stop()