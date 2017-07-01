"""
Acts as a fake Slack server, allowing for Slack bots to be tested for acceptance.
"""
import tornado.web
import tornado.websocket
import tornado.httpserver
import tornado.ioloop
from tornado import gen
from tornado.queues import Queue
import json
import os
import sys

loop = tornado.ioloop.IOLoop.current()
event_handlers = []
event_queue = Queue()

slack_handler = None


@gen.coroutine
def send_events():
    while True:
        event = yield event_queue.get()
        print("Sending event {}".format(event), file=sys.stderr)
        for event_handler in event_handlers:
            event_handler.write_message(event)
        yield gen.sleep(0.01)


class TestEventHandler(tornado.websocket.WebSocketHandler):
    def __init__(self, *args, **kwargs):
        tornado.websocket.WebSocketHandler.__init__(self, *args, **kwargs)

    def open(self):
        event_handlers.append(self)
        self.write_message({'event': 'hello'})

    def on_message(self, message):
        loop.add_callback(slack_handler.write_message, message)

    def on_close(self):
        event_handlers.remove(self)


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
        print("Slack RTM WebSocket Handler created", file=sys.stderr)
        global slack_handler
        slack_handler = self
        tornado.websocket.WebSocketHandler.__init__(self, *args, **kwargs)

    def open(self):
        """A Slack RTM WebSocket connection starts with a hello message."""
        print("Slack: Opened RTM WebSocket connection. Sending Hello message.", file=sys.stderr)
        self.write_message({"type": "hello"})
        login_event = {'event': 'login'}
        event_queue.put_nowait(login_event)

    def on_message(self, message_string):
        message = json.loads(message_string)
        print("Slack RTM on_message: {}".format(message))
        if 'type' in message and message['type'] == 'ping':
            pong = {'type': 'pong', 'reply_to': message['id']}
            loop.add_callback(self.write_message, json.dumps(pong))
        elif 'type' in message and message['type'] == 'message':
            message_event = {'event': 'message', 'message': message}
            event_queue.put_nowait(json.dumps(message_event))

    def on_close(self):
        print("Slack RTM WebSocket closed!")


class RtmHandler(tornado.web.RequestHandler):
    def post(self):
        print("MockSlack.RtmHandler: POST rtm.start")
        login_data = {
            'ok': True,
            'url': 'wss://slack.com/websocket',
            'team': {
                'domain': 'somedomain'
            },
            'self': {
                'name': 'yourbotname'
            },
            'channels': [{'id': 'C012345'}, {'id': 'C456789'}],
            'groups': [{'id': 'G012345'}],
            'ims': [{'id': 'I012345'}],
            'users': [{'name': 'yourbotname', 'id': 'U012345'}]
        }
        self.write(login_data)
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
    try:
        cert = os.environ["MOCK_SSL_CERT"]
        cert_key = os.environ["MOCK_SSL_KEY"]
    except KeyError:
        cert = '/var/cert/cert.pem'
        cert_key = '/var/cert/key.pem'

    try:
        ssl_port = int(os.environ["MOCK_SLACK_SSL_PORT"])
    except KeyError:
        ssl_port = 443

    test_event_app = TestEventApp()
    server = tornado.httpserver.HTTPServer(test_event_app)
    server.listen(8080)
    print("Listening to event WebSocket connections on port 8080...")

    mock_slack_app = MockSlackApp()
    server = tornado.httpserver.HTTPServer(mock_slack_app, ssl_options={
        "certfile": cert,
        "keyfile": cert_key,
    })
    server.listen(ssl_port)
    print("Listening to Slack connections on port {}".format(ssl_port))

    loop.add_callback(send_events)

    tornado.ioloop.IOLoop.instance().start()
