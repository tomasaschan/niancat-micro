from behave import given, when, then
import tornado
from threading import Thread
from .event_handler import EventHandler


def before_all(context):
    context.slack_events = EventHandler('ws://slack.com:8080/')
    context.chat_events = EventHandler('ws://nianchat-chat:80')
    context.tornado_thread = Thread(target=tornado.ioloop.IOLoop.current().start)


@given(u'Slackrest is connected to Slack')
def step_impl(context):
    context.slack_events.await(type='login')


@given(u'that /reply returns some reply')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given that /reply returns some reply')


@given(u'I map "!foo" to /reply')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given I map "!foo" to /reply')


@when(u'I send "!foo" from Slack channel "C012345"')
def step_impl(context):
    raise NotImplementedError(u'STEP: When I send "!foo" from Slack channel "C012345"')


@then(u'I should get that reply back in Slack channel "C012345"')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then I should get that reply back in Slack channel "C012345"')


@given(u'that /notify returns some notification')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given that /notify returns some notification')


@given(u'I map "!bar" to /notify')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given I map "!bar" to /notify')


@given(u'I set the notification channel to "C456789"')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given I set the notification channel to "C456789"')


@when(u'I send "!bar" from Slack channel "C012345"')
def step_impl(context):
    raise NotImplementedError(u'STEP: When I send "!bar" from Slack channel "C012345"')


@then(u'I should get a notification in channel "C456789"')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then I should get a notification in channel "C456789"')

