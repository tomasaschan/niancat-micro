from behave import given, when, then


@given(u'Slackrest is connected to Slack')
def step_impl(context):
    context.slack_events.await(type='login')


@given(u'that /reply returns some reply')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given that /reply returns some reply')


@given(u'I map "{command}" to /reply')
def step_impl(context, command):
    raise NotImplementedError(u'STEP: Given I map "<command>" to /reply')


@when(u'I send "{message}" from channel "{channel_id}"')
def step_impl(context, message, channel_id):
    raise NotImplementedError(u'STEP: When I send "<message>" from Slack channel "<channel>"')


@then(u'I should get a {type} in channel "{channel_id}"')
def step_impl(context, type, channel_id):
    raise NotImplementedError(u'STEP: Then I should get that reply back in Slack channel "{channel_id}"')


@given(u'that /notify returns some notification')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given that /notify returns some notification')


@given(u'I map "{command}" to /notify')
def step_impl(context, command):
    raise NotImplementedError(u'STEP: Given I map "<command>" to /notify')


@given(u'I set the notification channel to "{notification_channel_id}"')
def step_impl(context, notification_channel_id):
    raise NotImplementedError(u'STEP: Given I set the notification channel to "C456789"')


