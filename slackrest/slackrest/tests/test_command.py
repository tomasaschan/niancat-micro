import unittest

from slackrest.command import Visibility, Method, UnknownMethodException


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