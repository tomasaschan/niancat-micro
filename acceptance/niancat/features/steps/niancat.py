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


@then(u'I get a 404 Not Found response')
def step_impl(context):
    assert context.response.status_code == requests.code.not_found


@given(u'that I set the puzzle {puzzle}')
def step_impl(context, puzzle):
    context.response = requests.post(urljoin(niancat, '/puzzle'), json=puzzle)


@then(u'I get back {response_text}')
def step_impl(context, response_text):
    assert context.text == response_text

