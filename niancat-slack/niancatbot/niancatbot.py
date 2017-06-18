"""
NiancatBot is the Slack bot front-end for the Niancat micro services.

Usage: python niancatbot.py <token name>

"""

import os
from slackclient import SlackClient
import time
import tornado
from tornado.httpclient import AsyncHTTPClient
from tornado.ioloop import IOLoop
from threading import Thread


http_client = AsyncHTTPClient()
loop = IOLoop.current()


def slack_loop():
    try:
        token = os.environ["SLACK_API_TOKEN"]
    except KeyError:
        print("You must specify a Slack API token in the 'SLACK_API_TOKEN' environment variable")
        return
    sc = SlackClient(token)

    if sc.rtm_connect():
        print("NiancatBot connected to Slack RTM.")
        while True:
            msgs = sc.rtm_read()
            if msgs:
                for msg in msgs:
                    print("MESSAGE: {}".format(msg))
                    if 'type' in msg and msg['type'] == 'message':
                        if 'text' in msg and msg['text'] == '!nian':
                            loop.add_callback(get_puzzle, sc)
            time.sleep(1)
    else:
        print("Connection failed!")


def main():
    slack_thread = Thread(target=slack_loop)
    slack_thread.start()
    tornado.ioloop.IOLoop.current().start()


def get_puzzle(sc):
    def handle_response(response):
        if response.error:
            print("Error: %s" % response.error)
            sc.rtm_send_message("general", "ERROR: {}".format(response.error))
        else:
            print("RESPONSE:", response.body)
            msg = str(response.body).strip('"')
            sc.rtm_send_message("general", msg)

    print("Making GET request to v1/puzzle")
    http_client.fetch("http://niancat:8081/v1/puzzle", handle_response)


if __name__ == "__main__":
    main()
