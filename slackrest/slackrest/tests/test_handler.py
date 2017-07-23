import unittest
from unittest.mock import Mock
from slackrest.handler import MessageHandler


class FakeOutboundQueue:
    pass


def make_handler(notification_channel_id=None):
    mock_command_parser = Mock()
    return MessageHandler(mock_command_parser, FakeOutboundQueue(), notification_channel_id or "C123")


class TestHandler(unittest.TestCase):
    def test_HandleSlackMessage_EditMessage_NoRequest(self):
        handler = make_handler()
        users = {}
        requests = handler.handle_messages([{'type': 'message', 'edit': {}}], users)
        self.assertEqual([], requests)
