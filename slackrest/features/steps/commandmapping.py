from behave import *

from slackrest.command import FixedParameterCommand, FreeTextCommand
from slackrest.command import CommandParser

def before_feature(context):
    context.commands = []
    context.command_parser = CommandParser()

@given(u'a fixed parameter command mapping with pattern "{pattern}", and URL "{url}"')
def step_impl(context, pattern, url):
    command = FixedParameterCommand(pattern, url)
    context.latest_command = command
    context.commands.append(command)

@then(u'the target URL is "{url}"')
def step_impl(context, url):
    assert context.request.url == url

@given(u'a free text command mapping with pattern "{pattern}", and URL "{url}"')
def step_impl(context, pattern, url):
    command = FreeTextCommand(pattern, url)
    context.latest_command = command
    context.commands.append(command)

@given(u'required visibility is {visibility}')
def step_impl(context, visibility):
    context.latest_command.required_visibility = visibility

@then(u'the command is ignored')
def step_impl(context):
    assert context.request is None

@when(u'I send a command "{command}" in public')
def step_impl(context, command):
    context.request = context.command_parser.parse(command, CommandParser.PUBLIC)

@when(u'I send a command "{command}" in private')
def step_impl(context, command):
    context.request = context.command_parser.parse(command, CommandParser.PRIVATE)

@then(u'a response is sent noting that the command is unknown')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then a response is sent noting that the command is unknown')
