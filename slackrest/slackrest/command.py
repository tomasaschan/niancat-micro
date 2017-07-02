from parse import *


class Request:
    def __init__(self, url, body):
        self.url = url
        self.body = body


class CommandParser:
    def __init__(self, commands=[]):
        self._commands = commands

    def parse(self, msg, channel_id, visibility):
        for command in self._commands:
            command_has_visibility = not command.visibility == Visibility.Any
            visibility_matches_message = command.visibility == visibility
            if command_has_visibility and not visibility_matches_message:
                continue

            match = parse(command.pattern, msg)
            if match:
                arguments = match.named
                url = command.url_format.format(**arguments)
                if command.body:
                    request_body = command.body(**arguments)
                else:
                    request_body = None
                return Request(url, request_body)

    def add_command(self, command):
        self._commands.append(command)


class Visibility:
    Any = 0
    Public = 1
    Private = 2

    @classmethod
    def parse(cls, channel_id):
        if channel_id.startswith('C'):
            return Visibility.Public
        elif channel_id.startswith('D'):
            return Visibility.Private


class Method:
    GET = 0
    POST = 1
    PUT = 2
    UPDATE = 3
    DELETE = 4
    HEAD = 5
