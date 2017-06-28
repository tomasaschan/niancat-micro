import tornado
import tornado.ioloop
from threading import Thread
from steps.event_handler import EventHandler
from slackrest.command import CommandParser


def before_all(context):
    context.slack_events = EventHandler('ws://slack.com:8080/')
    #context.chat_events = EventHandler('ws://nianchat-chat:80')
    context.tornado_thread = Thread(target=tornado.ioloop.IOLoop.current().start)


def after_all(context):
    tornado.ioloop.IOLoop.current().stop()


def before_feature(context, feature):
    context.commands = []
    context.command_attributes = {}
    context.command_parser = CommandParser()