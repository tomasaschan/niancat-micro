from slackrest.command import Visibility, CommandParser
from slackrest.routing import IncomingMessage, RouteContext


class RequestAndRouteContext:
    def __init__(self, request, route_context):
        self.request = request
        self.route_context = route_context


class MessageHandler:
    def __init__(self, command_parser, outbound_message_queue, notification_channel_id):
        self._notification_channel_id = notification_channel_id
        self._outbound_message_queue = outbound_message_queue
        self._command_parser = command_parser
        self.self_name = None

    def handle_messages(self, msgs, users):
        requests_and_route_contexts = []
        for m in msgs:
            print("MESSAGE: {}".format(m))
            request_and_route = self.handle_if_message(m, users)
            if request_and_route:
                requests_and_route_contexts.append(request_and_route)
        return requests_and_route_contexts

    def handle_if_message(self, m, users):
        if 'type' in m and m['type'] == 'message' and 'channel' in m and 'user' in m:
            channel_id = m['channel']
            user_id = m['user']
            try:
                user_name = users[user_id].name
            except KeyError:
                user_name = '<unknown user name>'
            incoming_message = IncomingMessage(m, channel_id, user_id, user_name)
            route_context = RouteContext(self._outbound_message_queue, incoming_message, self._notification_channel_id)
            visibility = Visibility.parse(channel_id)
            return self.handle_command(incoming_message, route_context, visibility)

    def handle_command(self, incoming_message, route_context, visibility):
        print("Handling command for incoming message {}".format(incoming_message.message))
        print("  Bot user name is '{}' and sender user name is '{}'".format(self.self_name, incoming_message.user_name))
        message_text = incoming_message.message['text']
        request = self._command_parser.parse(message_text,
                                             incoming_message.channel_id,
                                             incoming_message.user_id,
                                             incoming_message.user_name,
                                             self.self_name,
                                             visibility)
        if request:
            print("Request will be for URL {}".format(request.url))
            return RequestAndRouteContext(request, route_context)
        else:
            print("Ignoring message")
            return None

    def set_self_name(self, self_name):
        self.self_name = self_name
