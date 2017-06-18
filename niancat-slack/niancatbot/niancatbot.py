"""
NiancatBot is the Slack bot front-end for the Niancat micro services.

Usage: python niancatbot.py <token name>

"""

import os
from slackclient import SlackClient
import time


def main():
    try:
        token = os.environ["SLACK_API_TOKEN"]
    except KeyError:
        print("You must specify a Slack API token in the 'SLACK_API_TOKEN' environment variable".format(token_name))
        return
    sc = SlackClient(token)

    if sc.rtm_connect():
        print("NiancatBot connected to Slack RTM.")
        while True:
            time.sleep(120)
    else:
        print("Connection failed!")

if __name__ == "__main__":
    main()
