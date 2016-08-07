Feature: Keys Bindings are swapped
  In order to swap key bindings
  I want to use keyswap-mode

  Scenario: Turn on keyswap-mode
    When I turn on keyswap-mode
    Then keyswap-mode should be active
    When I turn off keyswap-mode
    Then keyswap-mode should not be active

  Scenario: Type with and without keyswap-mode
    When I type "hello"
    Then I should see "hello"
    When I turn on keyswap-mode
    And I swap only keys "h" and "j"
    Given I clear the buffer
    When I type "hello"
    Then I should see "jello"
    When I turn off keyswap-mode
    Given I clear the buffer
    When I type "hello"
    Then I should see "hello"


Feature: Provided exceptions for certain symbols
  In order to account for different major modes
  I want to use the provided exceptions

  Scenario: I add braces to the keyswap pairs
    Given I am in buffer "*keyswap-test-hooks*"
    Given I clear the buffer
    And I turn on keyswap-mode
    When I type "[]{}"
    Then I should see "[]{}"
    Given I call the braces swap hook
    And I clear the buffer
    When I type "[]{}"
    Then I should see "{}[]"

  Scenario: I add braces to the keyswap pairs
    Given I clear the buffer
    And I turn on keyswap-mode
    When I type "\'\""
    Then I should see "\'\""
    Given I call the quotes swap hook
    And I clear the buffer
    When I type "\'\""
    Then I should see "\"\'"

  Scenario: I add braces to the keyswap pairs
    Given I clear the buffer
    And I turn on keyswap-mode
    When I type "-_"
    Then I should see "_-"
    Given I call the underscore swap hook
    And I clear the buffer
    When I type "-_"
    Then I should see "-_"

  Scenario: I add braces to the keyswap pairs
    Given I clear the buffer
    And I turn on keyswap-mode
    When I type ";:"
    Then I should see ";:"
    Given I call the colon swap hook
    And I clear the buffer
    When I type ";:"
    Then I should see ":;"



Feature: Key bindings propagate to isearch-mode
  In order to search with swapped key bindings
  I use the keyswap-isearch hooks

  Scenario: Keys are swapped in isearch-mode
    Given I insert:
    """
    First line
    Second line
    Third line
    lone
    done
    """
    When I turn on keyswap-mode
    And I swap only keys "d" and "l"
    When I go to beginning of buffer
    Given Mode integration is on
    And I isearch for "lone"
    Then the cursor should be after "done"
    When I go to beginning of buffer
    And Mode integration is off
    And I isearch for "lone"
    Then the cursor should be after "lone"

  Scenario: Keys are not swapped unless the hook is on
    Given I insert:
    """
    First line
    Second line
    Third line
    done
    """
    Given Mode integration is off
    And I isearch for "done"
    Then the cursor should be after "done"


Feature Key bindings are used by jump-char
  In order to jump to a swapped key character
  I use the keyswap-isearch hooks

  Scenario: Keys are swapped in jump-char
    Given I clear the buffer
    Given I insert:
    """
    Hello there this is a line
    """
    When I turn on keyswap-mode
    And I swap only keys "t" and "e"
    When I go to beginning of buffer
    Given Mode integration is off
    And I jump-char to "t"
    Then the cursor should be before "there this is a line"
    When I go to beginning of buffer
    Given Mode integration is on
    And I jump-char to "t"
    Then the cursor should be before "ello there this is a line"