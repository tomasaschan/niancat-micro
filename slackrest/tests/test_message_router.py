import unittest
from unittest.mock import Mock, create_autospec
import slackrest

class TestMessageRoute(unittest.TestCase):
    def setUp(self):
        self.message_queue = Mock()
        self.channel_id = 'C012345'
        self.user_id = 'U012345'

    def tearDown(self):
        pass

    def expect_message(self, message, user_id):
        self.message_queue.enqueue.assert_called_with(message, user_id)

    def test_MessageRouter_Reply_MessageIsReturnedToSender(self):
        reply_msg = {'type': 'reply', 'message': 'Lorem Ipsum'}
        slack_msg = slackrest.SlackMessage("Some message text", self.channel_id, self.user_id)

        router = slackrest.MessageRouter(self.message_queue)
        router.route(reply_msg)

        self.expect_message('Lorem Ipsum', self.user_id)
