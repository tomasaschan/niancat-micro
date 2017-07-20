Feature: Events are sent to and from the mock side
  In order to test that a Slack bot acts appropriately
  As a test running slack-mock
  I want events to be sent for each Slack message
  So that I can see the Slack bot send messages

  Scenario: A Slack user sends a message
    When a user sends a message to Slack
    Then an event of type "message" is sent
     And it contains the message
     And it contains the user id
     And it contains the channel id
     And it contains the team name

  Scenario: An RTM session is started
    When a Slack RTM session is started
    Then an event of type "rtm_start" is sent

  Scenario: An RTM WebSocket connection is made
    When an RTM WebSocket connection is made
    Then an event of type "login" is sent

  Scenario: An RTM WebSocket connection is closed
    When an RTM WebSocket connection is closed
    Then an event of type "websocket_closed" is sent