import unittest
from unittest.mock import Mock, create_autospec
import slackrest.routing as routing

class TestMessageRoute(unittest.TestCase):
    def setUp(self):
        self.message_queue = Mock()
        self.channel_id = 'C012345'
        self.user_id = 'U012345'
        self.notification_channel_id = 'C456789'

    def tearDown(self):
        pass

    def expect_message(self, message, user_id):
        self.message_queue.enqueue.assert_called_with(message, user_id)

    def test_RouteContext_Reply_MessageIsReturnedToSender(self):
        reply_msg = {'response_type': 'reply', 'message': 'Lorem Ipsum'}
        incoming_msg = routing.IncomingMessage("Some message text", self.channel_id, self.user_id)
        route_context = routing.RouteContext(self.message_queue, incoming_msg, self.notification_channel_id)

        route_context.route(reply_msg)

        self.expect_message('Lorem Ipsum', self.user_id)

    def test_RouteContext_Notification_MessageIsSentToNotificationChannel(self):
        notification_msg = {'response_type': 'notification', 'message': 'Ipsum Lorem'}
        incoming_msg = routing.IncomingMessage("Some message text", self.channel_id, self.user_id)
        route_context = routing.RouteContext(self.message_queue, incoming_msg, self.notification_channel_id)

        route_context.route(notification_msg)

        self.expect_message('Ipsum Lorem', self.notification_channel_id)

    def test_RouteContext_InvalidResponseType_InvalidMessageException(self):
        invalid_response_type_msg = {'response_type': 'invalidresponsetype', 'message': 'ABCDEFGHI'}

        incoming_msg = routing.IncomingMessage("Some message text", self.channel_id, self.user_id)
        route_context = routing.RouteContext(self.message_queue, incoming_msg, self.notification_channel_id)

        with self.assertRaises(routing.InvalidResponseType):
            route_context.route(invalid_response_type_msg)