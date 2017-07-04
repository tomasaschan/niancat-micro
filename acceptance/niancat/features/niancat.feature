Feature: Niancat as a service
  In order to solve the Dagens Nian puzzle
  As a user
  I want to be able to check solutions and store unsolutions
  So that we have tons of fun in our Slack channel

  Scenario: No puzzle is set
    Given that no puzzle is set
     When I get the puzzle
     Then I get a reply that the puzzle is not set

  Scenario: Get a puzzle
    Given I set the puzzle DATORSPLE
     When I get the puzzle
     Then I get a reply containing "DATORSPLE"

  Scenario: Solve a puzzle
    Given I set the puzzle DATORSPLE
     When I test the solution DATORSPEL
     Then I get a reply that it is correct
      And there is a notification that I solved the puzzle

  Scenario: Check an incorrect solution
    Given I set the puzzle DATORSPLE
     When I test the solution DATORLESP
     Then I get a reply that it is incorrect

  Scenario: Solutions are reported on next puzzle
    Given I set the puzzle DATORSPLE
      And I test the solution DATORSPEL
     When I set the puzzle VANTRIVSA
     Then my name is listed as solving DATORSPEL in the notification channel

  Scenario: A notification is sent when a puzzle with multiple solutions is set
     When I set the puzzle DATORSPLE
     Then there is a notification that there are two solutions

  Scenario: Setting an unsolution
    Given I store an unsolution "Unsolution 1"
     When I list my unsolutions
     Then the unsolutions contain "Unsolution 1"

  Scenario: Unsolutions are listed on next puzzle
    Given I set the puzzle DATORSPLE
      And I store an unsolution "ABC DEF GHI"
     When I set the puzzle VANTRIVSA
     Then the unsolution "ABC DEF GHI" is listed with my name

  Scenario: Showing mismatches for potential solutions
    Given I set the puzzle DATORSPLE
     When I test the solution DATORSPEXY
     Then I get a reply that there are too many XY and too few L
