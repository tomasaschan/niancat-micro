from slackrest.command import Visibility, CommandParser, Method
from slackrest.routing import IncomingMessage, RouteContext

class MessageHandler:
    def __init__(self, commands, make_request, notification_channel_id):
        self.make_request = make_request
        self.notification_channel_id = notification_channel_id
        self.command_parser = CommandParser(commands)

    def read_slack_messages(self, sc):
        msgs = sc.rtm_read()
        for m in msgs:
            print("MESSAGE: {}".format(m))
            self.handle_if_message(sc, m)

    def handle_if_message(self, sc, m):
        if 'type' in m and m['type'] == 'message' and 'channel' in m and 'user' in m:
            channel_id = m['channel']
            user_id = m['user']
            try:
                user_name = sc.server.users[user_id].name
            except KeyError:
                user_name = '<unknown user name>'
            incoming_message = IncomingMessage(m, channel_id, user_id, user_name)
            route_context = RouteContext(self, incoming_message, self.notification_channel_id)
            visibility = Visibility.parse(channel_id)
            self.handle_command(incoming_message, route_context, visibility)

    def handle_command(self, incoming_message, route_context, visibility):
        print("Handling command for incoming message {}".format(incoming_message.message))
        message_text = incoming_message.message['text']
        request = self.command_parser.parse(message_text,
                                            incoming_message.channel_id,
                                            incoming_message.user_id,
                                            incoming_message.user_name,
                                            visibility)
        if request:
            print("Request will be for URL {}".format(request.url))
            self.make_request(request, route_context)
        else:
            print("Ignoring message")
