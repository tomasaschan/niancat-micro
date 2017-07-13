import threading
from slackclient import SlackClient
from slackrest.routing import IncomingMessage, RouteContext
from slackrest.command import Visibility, CommandParser, Method
import os
from tornado.ioloop import  IOLoop
from tornado import gen
from tornado.httpclient import AsyncHTTPClient, HTTPRequest
import json


class SlackException(RuntimeError):
    def __init__(self, *args, **kwargs):
        RuntimeError.__init__(*args, **kwargs)


def create_slack_client():
    try:
        token = os.environ["SLACK_API_TOKEN"]
    except KeyError:
        print("You must specify a Slack API token in the 'SLACK_API_TOKEN' environment variable")
        return
    return SlackClient(token)


class SlackrestApp(object):
    def __init__(self, base_url, commands, notification_channel_id):
        self.base_url = base_url
        self.command_parser = CommandParser(commands)
        self.notification_channel_id = notification_channel_id
        self._async_thread = None
        self.sc = None

    def read_slack_messages(self, sc):
        msgs = sc.rtm_read()
        for m in msgs:
            print("MESSAGE: {}".format(m))
            self.handle_if_message(m)
        IOLoop.current().call_later(0.5, self.read_slack_messages, sc)

    def run_async(self):
        print("Starting SlackrestApp asynchronously")
        self._async_thread = threading.Thread(target=self.run_forever)
        self._async_thread.start()

    def handle_if_message(self, m):
        if 'type' in m and m['type'] == 'message':
            channel_id = m['channel']
            user_id = m['user']
            try:
                user_name = self.sc.server.users[user_id]
            except KeyError:
                user_name = '<unknown user name>'
            incoming_message = IncomingMessage(m, channel_id, user_id, user_name)
            route_context = RouteContext(self, incoming_message, self.notification_channel_id)
            visibility = Visibility.parse(channel_id)
            self.handle_command(incoming_message, route_context, visibility)

    def handle_command(self, incoming_message, route_context, visibility):
        print("Handling command for incoming message {}".format(incoming_message.message))
        message_text = incoming_message.message['text']
        request = self.command_parser.parse(message_text,
                                            incoming_message.channel_id,
                                            incoming_message.user_id,
                                            incoming_message.user_name,
                                            visibility)
        if request:
            print("Request will be for URL {}".format(request.url))
            self.make_request(request, route_context)
        else:
            print("Ignoring message")

    @gen.coroutine
    def make_request(self, request, route_context):
        def response_callback(http_response):
            if http_response.error:
                print("HTTP Response Error: {}".format(http_response.error))
            else:
                response = json.loads(http_response.buffer.getvalue().decode("utf-8"))
                print("Response: {}".format(response))
                for r in response:
                    route_context.route(r)

        final_url = self.base_url + request.url
        client = AsyncHTTPClient()
        http_request = HTTPRequest(final_url, method=Method.serialize(request.method), body=request.body)
        client.fetch(http_request, response_callback)

    def enqueue(self, message, channel_id):
        print("Replying to Slack with message {} and channel id {}".format(message, channel_id))
        IOLoop.current().add_callback(self.sc.rtm_send_message, channel_id, message)

    def connect_to_slack(self):
        print("Connecting to Slack...")
        self.sc = create_slack_client()
        if not self.sc:
            raise SlackException("Failed to create Slack client!")
        self.sc.server.rtm_connect()

        IOLoop.current().add_callback(self.read_slack_messages, self.sc)

    def run_forever(self):
        print("Starting SlackrestApp...")
        self.connect_to_slack()
        IOLoop.current().start()
