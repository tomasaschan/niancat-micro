from behave import *
import requests
from requests.compat import urljoin
import json

niancat = 'http://niancat/v1'


def get_responses_of_type(response_type, request_text):
    messages = json.loads(request_text)
    if response_type:
        return [m for m in messages if m['response_type'] == response_type]
    else:
        return messages


def get_responses_containing(text, request_text, response_type=None):
    replies = get_responses_of_type(response_type, request_text)
    return [r for r in replies
            if text in r['message']]


def has_reply_containing(text, request_text):
    return get_responses_containing(text, request_text, response_type='reply') is not []


def has_notification_containing(text, request_text):
    return get_responses_containing(text, request_text, response_type='notification') is not []


@given(u'that no puzzle is set')
def step_impl(context):
    r = requests.delete(urljoin(niancat, '/puzzle'))
    assert r.status_code == requests.codes.ok


@when(u'I get the puzzle')
def step_impl(context):
    context.response = requests.get(urljoin(niancat, '/puzzle'))


@then(u'I get a reply that the puzzle is not set')
def step_impl(context):
    assert has_reply_containing('är inte satt', context.request.text)


@given(u'I set the puzzle {puzzle}')
def step_impl(context, puzzle):
    context.response = requests.post(urljoin(niancat, '/puzzle'), json=puzzle)


@then(u'I get a reply containing {reply_text}')
def step_impl(context, reply_text):
    assert has_reply_containing(reply_text, context.request.text)


@then(u'I get a reply that it is correct')
def step_impl(context):
    assert has_reply_containing('är korrekt', context.request.text)


@then(u'there is a notification that I solved the puzzle')
def step_impl(context):
    notifications = get_responses_containing('löste nian')
    assert notifications is not []
    assert len(notifications) == 1
    assert context.user_id in notifications[0]


@when(u'I test the solution {maybesolution}')
def step_impl(context, maybesolution):
    context.user_id = 'U012345'
    context.response = requests.post(urljoin(niancat, '/solution/{}'.format(maybesolution)), json=context.user_id)


@then(u'I get a reply that it is incorrect')
def step_impl(context):
    assert has_reply_containing('finns inte med i')


@then(u'my name is listed as solving {solution} in the notification channel')
def step_impl(context, solution):
    notifications = get_responses_containing(solution, context.request.text)
    notifications_with_my_name = [mine for mine in notifications if context.user_id in mine]
    assert notifications_with_my_name is not  []


@then(u'there is a notification that there are two solutions')
def step_impl(context):
    assert has_notification_containing('2 lösningar', context.request.text)


@given(u'I store an unsolution "{unsolution}"')
def step_impl(context, unsolution):
    user_id = "U012345"
    context.response = requests.post(
        urljoin(niancat, '/unsolution/{}'.format(user_id)),
        json=unsolution)


@when(u'I list my unsolutions')
def step_impl(context):
    user_id = "U012345"
    context.response = requests.get(urljoin(niancat, '/unsolution/{}'.format(user_id)))


@then(u'the unsolutions contain "{unsolution}"')
def step_impl(context, unsolution):
    assert unsolution in context.response.text


@then(u'the unsolution "{unsolution}" is listed with my name')
def step_impl(context, unsolution):
    user_id = "U012345"
    assert user_id in context.response.text
    assert unsolution in context.response.text


@given(u'I test the solution {maybesolution}')
def step_impl(context, maybesolution):
    context.user_id = 'U012345'
    context.response = requests.post(urljoin(niancat, '/solution/{}'.format(maybesolution)), json=context.user_id)


@when(u'I set the puzzle {puzzle}')
def step_impl(context, puzzle):
    context.response = requests.post(urljoin(niancat, '/puzzle'), json=puzzle)


@then(u'I get a reply that there are too many {toomany} and too few {toofew}')
def step_impl(context, toomany, toofew):
    assert has_reply_containing(toomany, context.request.text)
    assert has_reply_containing(toofew, context.request.text)
