Scenario: Echo area should indicate opponent ran out of time.
  Given ics session
  When new game
  And opponent forfeits on time
  Then I should see message "Your opponent has forfeited the game on time"

Scenario: Let me know when opponent aborts
  Given ics session
  When new game
  And opponent aborts
  Then I should see message "Your offer to abort was accepted"

Scenario: Let me know when opponent forfeits by disconnection
  Given ics session
  When new game
  And opponent forfeits by disconnection
  Then I should see message "Your opponent has resigned"
