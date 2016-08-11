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


Feature Key swaps are propagated to avy
  In order to jump to a word using avy
  I can specify the start of the word with swapped keys

  # I only check avy-goto-word-1. Functions in avy.el *currently* all either use
  # `read-char' or aren't relevant to using keyswap-mode.
  # Testing every function in avy.el is just as brittle (because the function
  # names are very likely to change) and much more work than just checking one
  # function and assuming it's representative of how all functions in the
  # library work.
  Scenario: Keys are not swapped in avy unless we do something
    Given I clear the buffer
    # When printing out the problem steps, having a '%' in the text below causes
    # problems, it doesn't cause any problems with running the test though.
    Given I insert:
    """
    ! @ # $ % ^ & * ( )
    1 2 3 4 5 6 7 8 9 0
    ! @ # $ % ^ & * ( )
    1 2 3 4 5 6 7 8 9 0
    """
    Given I go to beginning of buffer
    And I jump to the first occurance of "1"
    Then the cursor should be at point "21"

  Scenario: Keyswap mode can swap keys in avy-goto-word-1
    When I turn on keyswap-mode
    And I start avy-integration
    And I swap only keys "a" and "s"
    And I jump to the first occurance of "1"
    Then the cursor should be at point "61"
    When I swap keys "1" and "!"
    And I jump to the first occurance of "1"
    Then the cursor should be at point "41"

  Scenario: Keyswap avy integration may be removed cleanly
    When I remove avy-integration
    And I jump to the first occurance of "1"
    Then the cursor should be at point "21"
