Feature: Command mappings define how Slack events are mapped to a chat bot REST call
  As a user of Slackrest
  I want to define commands
  So that they are properly mapped to the chat bots REST interface

  Scenario: Command mapping with no parameters
    Given a command with pattern '!noparam'
      And URL format '/noparam' with method GET
      And for any visibility
      And with no request body
     When I send a message '!noparam'
     Then the request URL is '/noparam'
      And the request body is empty

  Scenario: Command mapping with a single required argument
    Given a command with pattern '!oneparam {param1}'
      And URL format '/oneparam/{param1}' with method GET
      And for any visibility
      And with no request body
     When I send a message '!oneparam foo'
     Then the request URL is '/oneparam/foo'
      And the request body is empty

  Scenario: Private command mappings are ignored for public channels
    Given a command with pattern '!privatecommand'
      And URL format '/privatecommand' with method GET
      And for private channels
      And with no request body
     When I send a message '!privatecommand' in public
     Then the command is ignored

  Scenario: Private command mappings are processed for private channels
    Given a command with pattern '!privatecommand'
      And URL format '/privatecommand' with method GET
      And for private channels
      And with no request body
     When I send a message '!privatecommand' in private
     Then the request URL is '/privatecommand'


  Scenario: Unknown commands in public are ignored
    Given a command with pattern '!somecommand'
      And URL format '/somecommand' with method GET
      And for any visibility
      And with no request body
     When I send a message '!nosuchcommand' in public
     Then the command is ignored

  Scenario: Commands can specify a body
    Given a command with pattern '!bodycommand {param}'
      And URL format '/bodycommand' with method GET
      And for any visibility
      And with a body that writes the param value as JSON
     When I send a message '!bodycommand foo'
     Then the request body contains 'foo'

  Scenario: Commands can also use POST
    Given a command with pattern '!command'
      And URL format '/command' with method POST
      And for any visibility
      And with no request body
     When I send a message '!command'
     Then the request method is POST

  Scenario: User id is always implicitly available in URLs
    Given a command with pattern '!command'
      And URL format '/command/{user_id}' with method POST
     When a message '!command' is sent by user id 'U012345'
     Then the request URL is '/command/U012345'

  Scenario: User id is always implicitly available in the body
    Given a command with pattern '!command'
      And URL format '/command'
      And with a body that contains the user id
     When a message '!command' is sent by user id 'U012345'
     Then the request body contains 'U012345'

  Scenario: Channel id is always implicitly available in URLs
    Given a command with pattern '!command'
      And URL format '/command/{channel_id}'
     When a message '!command' is sent from channel 'C012345'
     Then the request URL is '/command/C012345'

  Scenario: Channel id is always implicitly available in the body
    Given a command with pattern '!command'
      And URL format '/command'
      And with a body that contains the channel id
     When a message '!command' is sent from channel 'C012345'
     Then the request body contains 'C012345'
