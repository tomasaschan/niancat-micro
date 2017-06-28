Feature: Command mappings define how Slack events are mapped to a chat bot REST call
  As a user of Slackrest
  I want to define commands
  So that they are properly mapped to the chat bots REST interface

  Scenario: Command with no arguments
    Given a fixed parameter command mapping with pattern "!foo", and URL "/bar"
     When I send a command "!foo" in public
     Then the target URL is "/bar"

  Scenario: Command with a single required argument
    Given a fixed parameter command mapping with pattern "!foo <qux>", and URL "/bar/<qux>"
     When I send a command "!foo baz" in public
     Then the target URL is "/bar/baz"

  Scenario: Command with a free text argument
    Given a free text command mapping with pattern "!foo <freetext>", and URL "/bar/{freetext}"
     When I send a command "!foo baz qux" in public
     Then the target URL is "/bar/baz%2Fqux"

  Scenario: Private command mappings are ignored for public channels
    Given a fixed parameter command mapping with pattern "!foo", and URL "/bar"
      And required visibility is private
     When I send a command "!foo" in public
     Then the command is ignored

  Scenario: Unknown commands in public are ignored
    Given a fixed parameter command mapping with pattern "!foo", and URL "/bar"
     When I send a command "!baz" in public
     Then the command is ignored

  Scenario: Unknown commands in private are reported to the user
    Given a fixed parameter command mapping with pattern "!foo", and URL "/bar"
     When I send a command "!baz" in private
     Then a response is sent noting that the command is unknown
