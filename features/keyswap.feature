Feature: Keys Bindings are swapped
  In order to swap key bindings
  As a keyswap-mode user
  I want to enable keyswap-mode

  Scenario: Turn on keyswap-mode
    Given I am in buffer "*keyswap-mode-tests*"
    And I turn on keyswap-mode
    Then keyswap-mode should be active
    When I turn off keyswap-mode
    Then keyswap-mode should not be active

  Scenario: Type with and without keyswap-mode
    Given I am in buffer "*keyswap-mode-tests*"
    Given the buffer is empty
    When I press "hello"
    Then I should see "hello"
    When I swap only keys "h" and "j"
    And I turn on keyswap-mode
    And I update the keyswap map
    Given I clear the buffer
    When I press "hello"
    Then I should see "jello"
