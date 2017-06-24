"""
Acts as a fake Slack server, allowing for Slack bots to be tested for acceptance.
"""
import tornado.web
import tornado.websocket
import tornado.httpserver
import tornado.ioloop
import json

loop = tornado.ioloop.IOLoop.current()

class TestEventHandler(tornado.websocket.WebSocketHandler):
    def open(self):
        pass
 
    def on_message(self, message):
        """Ignore any message sent to the event producer."""
        pass

    def on_close(self):
        pass




class TestEventApp(tornado.web.Application):
    def __init__(self, test_event_producer):
        test_event_producer = TestEventHandler()
        handlers = [
            (r'/', lambda: test_event_producer)
        ]

        settings = {
            'template_path': 'templates'
        }
        tornado.web.Application.__init__(self, handlers, **settings)

class MockSlackHandler(tornado.websocket.WebSocketHandler):
    def __init__(self, test_event_producer):
        self.test_event_producer = test_event_producer

    def open(self):
        """A Slack RTM WebSocket connection starts with a hello message."""
        self.write_message('{ "type": "hello" }')
        login_event = {'event': 'login'}
        loop.add_callback(self.test_event_producer.write_message, json.dumps(login_event))

    def on_message(self, message):
        if 'type' in message and message['type'] == 'ping':
            pong = {'type': 'pong', 'reply_to': message['id']}
            loop.add_callback(self.write_message, json.dumps(pong))
        elif 'type' in message and message['type'] == 'message':
            json_message = json.loads(message)
            message_event = {'event': 'message', 'message': json_message}
            loop.add_callback(self.test_event_producer.write_message, json.dumps(message_event))

    def on_close(self):
        pass


class RtmHandler(tornado.web.RequestHandler):
    def get(self):
        return json.dumps({'ok': True, 'url': 'http://slack.com/websocket'})


class MockSlackApp(tornado.web.Application):
    def __init__(self, test_event_producer):
        mock_slack_handler = MockSlackHandler(test_event_producer)
        handlers = [
            (r'/api/rtm.start', RtmHandler)
            (r'/websocket', lambda: mock_slack_handler),
        ]
 
        settings = {
            'template_path': 'templates'
        }
        tornado.web.Application.__init__(self, handlers, **settings)
 
 
if __name__ == '__main__':
    test_event_producer = TestEventHandler()
    test_event_app = TestEventApp(test_event_producer)
    server = tornado.httpserver.HTTPServer(test_event_app)
    server.listen(8080)

    mock_slack_app = MockSlackApp()
    server = tornado.httpserver.HTTPServer(mock_slack_app)
    server.listen(80)

    tornado.ioloop.IOLoop.instance().start()