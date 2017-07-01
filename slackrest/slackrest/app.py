import threading
from slackclient import SlackClient
import os
from tornado.ioloop import  IOLoop


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
        self.commands = commands
        self.notification_channel_id = notification_channel_id
        self._async_thread = None

    def read_slack_messages(self, sc):
        print("WebSocket URL", sc.server.ws_url)
        msgs = sc.rtm_read()
        print("Raw MESSAGE'", msgs)
        for m in msgs:
            print("MESSAGE: {}".format(m))

    def run_async(self):
        print("Starting SlackrestApp asynchronously")
        self._async_thread = threading.Thread(target=self.run_forever)
        self._async_thread.start()

    def connect_to_slack(self):
        print("Connecting to Slack...")
        sc = create_slack_client()
        if not sc:
            raise SlackException("Failed to create Slack client!")
        sc.server.rtm_connect()

        IOLoop.current().add_callback(self.read_slack_messages, sc)

    def run_forever(self):
        print("Starting SlackrestApp...")
        self.connect_to_slack()
        IOLoop.current().start()
