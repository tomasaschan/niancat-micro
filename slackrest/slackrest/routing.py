"""
The `routing` module deals with sending outgoing messages (to Slack) to the right channel or user.

# Terminology
A response is a message sent back to the Slack user or channel. A response can be of three types

- Reply: send the response to the same channel or user that the incoming message originated from.
- Notification: send the response to a defined "main" channel.
- Direct: send the response to a user or channel directly set in the message.
"""

class InvalidResponseType(Exception):
    def __init__(self):
        Exception.__init__(self)

class RouteContext(object):
    """
    A RouteContext is a class which encapsulates all the context needed to route a response to the right channel/user.
    """
    def __init__(self, outbound_message_queue, incoming_message, notification_channel_id):
        self.outbound_message_queue = outbound_message_queue
        self.incoming_message = incoming_message
        self.notification_channel_id = notification_channel_id

    def route(self, response):
        """
        Route a response according to its `response_type` value.
        `response_type` must be one of `reply`, `notification`, or `direct`. If it is `direct`, then the key
        `channel_id` must also be present.

        :param response: a dict with keys `response_type`, `message`, and optionally `channel_id`.
        """
        response_type = response['response_type']
        if response_type == 'reply':
            response_channel = self.incoming_message.user_id
        elif response_type == 'notification':
            response_channel = self.notification_channel_id
        else:
            raise InvalidResponseType()

        self.outbound_message_queue.enqueue(response['message'], response_channel)