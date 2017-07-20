# Slack-mock
Slack-mock is a simple fake Slack service, for use in acceptance tests.

A Slack bot can connect to it, thinking it is the real Slack service, and
send and receive message to/from it. For each interaction with a Slack bot,
the Slack-mock services produces an event on a separate WebSocket connection.
This allows a test framework to automatically validate that the Slack bot
acts appropriately.

## License
This software is licensed under GPL version 3. See LICENSE for the full license text.