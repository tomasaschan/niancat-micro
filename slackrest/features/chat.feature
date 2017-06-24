Feature: A Slackrest app acts as a middleman between Slack and a RESTful chat service.
  In order to separate a chat bot from its Slack integration
  As a developer
  I want to easily map Slack messages to RESTful calls
  So that I can implement my chat bot as a micro service independent of Slack

  Background: Slackrest is connected to Slack
    Given Slackrest is connected to Slack


  Scenario: Sending a message and getting a reply
    Given that /reply returns some reply
      And I map "!foo" to /reply
     When I send "!foo" from Slack channel "C012345"
     Then I should get that reply back in Slack channel "C012345"

  Scenario: Sending a message and getting a notification
    Given that /notify returns some notification
      And I map "!bar" to /notify
      And I set the notification channel to "C456789"
     When I send "!bar" from Slack channel "C012345"
     Then I should get a notification in channel "C456789"

