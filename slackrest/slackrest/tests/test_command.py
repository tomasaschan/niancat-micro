import unittest

from slackrest.command import Visibility, Method, UnknownMethodException, CommandParser


class FooCommand:
    pattern = '!foo'
    url_format = '/foo'
    visibility = Visibility.Any
    body = None
    method = Method.GET


class TestCommand(unittest.TestCase):
    def test_ChannelVisibility_ChannelStartingWithC_ChannelIsPublic(self):
        self.assertEqual(Visibility.Public, Visibility.parse("C0123456"))

    def test_ChannelVisibility_ChannelStartingWithD_ChannelIsPrivate(self):
        self.assertEqual(Visibility.Private, Visibility.parse("D0123456"))

    def test_ParseMethod_GET_MethodIsGET(self):
        self.assertEqual(Method.GET, Method.parse("GET"))

    def test_ParseMethod_POST_MethodIsPOST(self):
        self.assertEqual(Method.POST, Method.parse("POST"))

    def test_ParseMethod_PUT_MethodIsPUT(self):
        self.assertEqual(Method.PUT, Method.parse("PUT"))

    def test_ParseMethod_DELETE_MethodIsDELETE(self):
        self.assertEqual(Method.DELETE, Method.parse("DELETE"))

    def test_ParseMethod_UnknownMethod_RaisesException(self):
        self.assertRaises(UnknownMethodException, Method.parse, "NOTAMETHOD")

    def test_SerializeMethod_GET_ReturnsString(self):
        self.assertEqual('GET', Method.serialize(Method.GET))

    def test_SerializeMethod_POST_ReturnsString(self):
        self.assertEqual('POST', Method.serialize(Method.POST))

    def test_ParseCommand_CommandHasTrailingWhitespace_CommandIsFound(self):
        command_parser = CommandParser([FooCommand])
        text_with_trailing_whitespace = "!foo "
        channel_id = "C1"
        user_id = "U1"
        user_name = "user"
        command = command_parser.parse(text_with_trailing_whitespace, channel_id, user_id, user_name, Visibility.Public)
        self.assertIsNotNone(command)