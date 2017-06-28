from behave import *
from slackrest.command import Visibility

# class NoParamCommand:
#     pattern = '!noparam'
#     url_format = '/noparam'
#     visibility = Visibility.Any
#     body = None
#
# class FooCommand:
#     pattern = '!foo {bar}'
#     url_format = '/foo/{bar}'
#     visibility = Visibility.Any
#     body = None
#
# class BarCommand:
#     pattern = '!bar {baz} {qux}'
#     url_format = '/bar'
#     visibility = Visibility.Any
#
#     @classmethod
#     def body(cls, baz, qux):
#         return json.dumps({'argument1': baz, 'argument2': qux})
#
# class BazCommand:
#     pattern = '{freetext}'
#     url_format = '/baz'
#     visibility = Visibility.Private
#
#     @classmethod
#     def body(cls, freetext):
#         return json.dumps(freetext)


@given(u'a command with pattern \'{pattern}\'')
def step_impl(context, pattern):
    context.command_attributes['pattern'] = pattern


@given(u'URL format \'{url_format}\'')
def step_impl(context, url_format):
    context.command_attributes['url_format'] = url_format


@given(u'for any visibility')
def step_impl(context):
    context.command_attributes['visibility'] = Visibility.Public


@given(u'with no request body')
def step_impl(context):
    context.command_attributes['body'] = None


@when(u'I send a message \'{msg}\'')
def step_impl(context, msg):
    channel_id = "C0123456"
    try:
        context.request = context.command_parser.parse(msg, channel_id, Visibility.Public)
    except Exception as e:
        context.parse_exception = e


@then(u'the request URL is \'{url}\'')
def step_impl(context, url):
    assert context.request.url == url


@then(u'the request body is empty')
def step_impl(context):
    assert context.request.body is None


@given(u'for private channels')
def step_impl(context):
    context.command_attributes['visibility'] = Visibility.Private


@then(u'the command is ignored')
def step_impl(context):
    assert context.request is None


@when(u'I send a message \'{msg}\' in public')
def step_impl(context, msg):
    channel_id = "C0123456"
    try:
        context.request = context.command_parser.parse(msg, channel_id, Visibility.Public)
    except Exception as e:
        context.parse_exception = e


@when(u'I send a message \'{msg}\' in private')
def step_impl(context, msg):
    channel_id = "C0123456"
    try:
        context.request = context.command_parser.parse(msg, channel_id, Visibility.Private)
    except Exception as e:
        context.parse_exception = e
