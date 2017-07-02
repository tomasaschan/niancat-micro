@integration
Feature: A Slackrest app acts as a middleman between Slack and a RESTful chat service.
  In order to separate a chat bot from its Slack integration
  As a developer
  I want to easily map Slack messages to RESTful calls
  So that I can implement my chat bot as a micro service independent of Slack

  Background: Slackrest is connected to Slack
    Given I map "!givemeareply" to /reply
      And I map "!givemeanotification" to /notify
      And I set the notification channel to "C456789"
      And the chat bot is at http://niancat-chat
      And Slackrest is connected to Slack


  Scenario: Sending a message and getting a reply
     When I send "!givemeareply" from channel "C012345"
     Then I should get a message in channel "C012345"

  Scenario: Sending a message and getting a notification
     When I send "!givemeanotification" from channel "C012345"
     Then I should get a message in channel "C456789"

