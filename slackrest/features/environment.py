import tornado
import tornado.ioloop
from threading import Thread
from steps.event_handler import EventHandler
from slackrest.command import CommandParser, Visibility


def before_feature(context, feature):
    if 'integration' in feature.tags:
        loop = tornado.ioloop.IOLoop.instance()
        context.slack_events = EventHandler('ws://slack.com:8080/', loop)
        context.tornado_thread = Thread(target=loop.start)
        context.tornado_thread.start()
        context.slack_events.await_connected()


def after_feature(context, feature):
    if 'integration' in feature.tags:
        tornado.ioloop.IOLoop.instance().stop()


def before_scenario(context, scenario):
    context.commands = []
    context.command_attributes = {'visibility': Visibility.Any, 'body': None}
    context.command_parser = CommandParser()
    context.self_name = 'mybotname'
    context.request = None
