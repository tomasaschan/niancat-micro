import unittest

from slackrest.command import Visibility


class TestCommand(unittest.TestCase):
    def test_ChannelVisibility_ChannelStartingWithC_ChannelIsPublic(self):
        self.assertEqual(Visibility.Public, Visibility.parse("C0123456"))

    def test_ChannelVisibility_ChannelStartingWithD_ChannelIsPrivate(self):
        self.assertEqual(Visibility.Private, Visibility.parse("D0123456"))
