import threading
from tornado.platform.asyncio import AsyncIOMainLoop
import asyncio
from slackclient import SlackClient
import os


class SlackException(RuntimeError):
    pass


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

    async def read_slack_messages(self):
        msgs = self.sc.rtm_read()
        for m in msgs:
            print("MESSAGE: {}".format(m))

    def run_async(self):
        print("Starting SlackrestApp asynchronously")
        self._async_thread = threading.Thread(target=self.run_forever)
        self._async_thread.start()

    def connect_to_slack(self):
        # Connect to Slack
        print("Connecting to Slack...")
        sc = create_slack_client()
        if not sc.rtm_connect():
            raise SlackException()

        asyncio.get_event_loop().call_soon(self.read_slack_messages)

    def run_forever(self):
        print("Starting SlackrestApp...")
        asyncio.get_event_loop()
        AsyncIOMainLoop().install()
        asyncio.get_event_loop().call_soon(self.connect_to_slack)
        asyncio.get_event_loop().run_forever()