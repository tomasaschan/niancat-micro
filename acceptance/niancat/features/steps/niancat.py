from behave import *
import requests
from requests.compat import urljoin

niancat = 'http://niancat/v1'


@given(u'that no puzzle is set')
def step_impl(context):
    r = requests.delete(urljoin(niancat, '/puzzle'))
    assert r.status_code == requests.codes.ok


@when(u'I get the puzzle')
def step_impl(context):
    context.response = requests.get(urljoin(niancat, '/puzzle'))


@then(u'I get a response that the puzzle is not set')
def step_impl(context):
    assert context.response.status_code == requests.code.not_found


@given(u'I set the puzzle {puzzle}')
def step_impl(context, puzzle):
    context.response = requests.post(urljoin(niancat, '/puzzle'), json=puzzle)


@then(u'I get back {response_text}')
def step_impl(context, response_text):
    assert context.text == response_text


@then(u'the response is that it is correct')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the response is that it is correct')


@then(u'a notification that I solved the puzzle')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then a notification that I solved the puzzle')


@when(u'I test the solution {maybesolution}')
def step_impl(context, maybesolution):
    raise NotImplementedError(u'STEP: When I test the solution DATORLESP')


@then(u'the response is that it is incorrect')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the response is that it is incorrect')


@then(u'my name is listed as solving {solution} in the notification channel')
def step_impl(context, solution):
    raise NotImplementedError(u'STEP: Then my name is listed as solving DATORSPEL in the notification channel')


@then(u'there is a notification that there are two solutions')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then there is a notification that there are two solutions')


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
    raise NotImplementedError(u'STEP: Given I test the solution DATORSPEL')


@when(u'I set the puzzle {puzzle}')
def step_impl(context, puzzle):
    context.response = requests.post(urljoin(niancat, '/puzzle'), json=puzzle)


