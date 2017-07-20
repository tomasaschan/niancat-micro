Feature: Arranging teams and users
  In order to provide a realistic environment for the Slack bot
  As a test running slack-mock
  I want to be able to arrange a Slack environment
  So that the Slack bot has reasonable channels and users

  Scenario: Specifying channels
    Given that we arrange the channel #foo with id C123
     When a Slack bot starts an RTM session
     Then the channel list contains channel id C123

  Scenario: Arranging users
    Given that we arrange the user baz with id U123
     When a Slack bot starts an RTM session
     Then the user list contains user baz with id U123

  Scenario: Arranging self name
    Given that we arrange the self name foo
     When a Slack bot starts an RTM session
     Then the self name is foo

