import tornado
import tornado.ioloop
from threading import Thread
from steps.event_handler import EventHandler
from slackrest.command import CommandParser


def before_all(context):
    loop = tornado.ioloop.IOLoop.instance()
    context.slack_events = EventHandler('ws://slack.com:8080/', loop)
    #context.chat_events = EventHandler('ws://nianchat-chat:80')
    context.tornado_thread = Thread(target=loop.start)
    context.tornado_thread.start()
    context.slack_events.await_connected()

def after_all(context):
    tornado.ioloop.IOLoop.instance().stop()


def before_scenario(context, scenario):
    context.commands = []
    context.command_attributes = {}
    context.command_parser = CommandParser()
