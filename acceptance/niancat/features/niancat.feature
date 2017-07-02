Feature: Niancat as a REST service
  In order to solve the Dagens Nian puzzle
  As a user
  I want to be able to check solutions and store unsolutions
  So that we have tons of fun in our Slack channel

  Scenario: No puzzle is set
    Given that no puzzle is set
     When I get the puzzle
     Then I get a 404 Not Found response

  Scenario: Get a puzzle
    Given that I set the puzzle ABCDEFGHI
     When I get the puzzle
     Then I get back "ABCDEFGHI"

