from behave import given, when, then
from slackrest.app import SlackrestApp
from slackrest.command import Visibility


class GiveMeAReply:
    pattern = '!givemeareply'
    url_format = '/reply'
    visibility = Visibility.Any
    body = None


class GiveMeANotification:
    pattern = '!givemeanotification'
    url_format = '/notify'
    visibility = Visibility.Any
    body = None


commands = [GiveMeAReply, GiveMeANotification]


@given(u'Slackrest is connected to Slack')
def step_impl(context):
    context.app = SlackrestApp(context.chat_url, commands, context.notification_channel_id)
    context.app.run_async()
    context.slack_events.await(event_type='login')


@when(u'I send "{message}" from channel "{channel_id}"')
def step_impl(context, message, channel_id):
    msg = {'type': 'message', 'text': message, 'channel': channel_id}
    context.slack_events.send_message(msg)


@then(u'I should get a {type} in channel "{channel_id}"')
def step_impl(context, event_type, channel_id):
    event = context.slack_events.await(event_type=event_type)
    assert event['channel_id'] == channel_id


@given(u'I set the notification channel to "{notification_channel_id}"')
def step_impl(context, notification_channel_id):
    context.notification_channel_id = notification_channel_id


@given(u'I map "!givemeareply" to /reply')
def step_impl(context):
    pass


@given(u'I map "!givemeanotification" to /notify')
def step_impl(context):
    pass


@given(u'the chat bot is at {url}')
def step_impl(context, url):
    context.chat_url = url