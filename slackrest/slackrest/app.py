import threading
from slackclient import SlackClient
import os
from tornado.ioloop import IOLoop, PeriodicCallback
from tornado import gen
from tornado.httpclient import AsyncHTTPClient, HTTPRequest
import json
from slackrest.handler import MessageHandler
from slackrest.command import Method, CommandParser
import sys

class SlackException(RuntimeError):
    def __init__(self, *args, **kwargs):
        RuntimeError.__init__(*args, **kwargs)


def create_slack_client():
    try:
        token = os.environ["SLACK_API_TOKEN"].strip()
    except KeyError:
        print("You must specify a Slack API token in the 'SLACK_API_TOKEN' environment variable")
        return
    return SlackClient(token)


class SlackrestApp(object):
    def __init__(self, base_url, commands, notification_channel_id):
        self.base_url = base_url
        self._async_thread = None
        self.sc = None
        self.read_msg_callback = None
        self.handler = MessageHandler(CommandParser(commands), self, notification_channel_id)

    def run_async(self):
        print("Starting SlackrestApp asynchronously")
        self._async_thread = threading.Thread(target=self.run_forever)
        self._async_thread.start()

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
        print("Connected to Slack. Bot user name is '{}'".format(self.sc.server.username))
        self.sc.server.rtm_connect()
        self.handler.set_self_name(self.sc.server.username)

    def read_slack_messages(self):
        msgs = self.sc.rtm_read()
        requests_and_route_contexts = self.handler.handle_messages(msgs, self.sc.server.users)
        for rarc in requests_and_route_contexts:
            self.make_request(rarc.request, rarc.route_context)

    def run_forever(self):
        print("Starting SlackrestApp...")
        self.connect_to_slack()
        self.read_msg_callback = PeriodicCallback(self.read_slack_messages, 500)
        self.read_msg_callback.start()
        IOLoop.current().start()
