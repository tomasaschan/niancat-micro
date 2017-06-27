from behave import given, when, then
from slackrest.app import SlackrestApp
from slackrest.routing import RouteContext

def before_feature(context):
    context.app = SlackrestApp()


@given(u'Slackrest is connected to Slack')
def step_impl(context):
    context.slack_events.await(type='login')


@given(u'I map "{command}" to {url}')
def step_impl(context, command, url):
    pass

@when(u'I send "{message}" from channel "{channel_id}"')
def step_impl(context, message, channel_id):
    msg = {'type': 'message', 'text': message, 'channel': channel_id}
    context.slack_events.send_message(msg)


@then(u'I should get a {type} in channel "{channel_id}"')
def step_impl(context, type, channel_id):
    event = context.slack_events.await(type=type)
    assert event['channel_id'] == channel_id


@given(u'I set the notification channel to "{notification_channel_id}"')
def step_impl(context, notification_channel_id):
    context.app.set_notification_channel(notification_channel_id)

