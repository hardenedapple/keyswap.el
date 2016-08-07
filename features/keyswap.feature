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
