Scenario: Echo area should indicate opponent ran out of time.
  Given ics session
  When new game
  And opponent forfeits on time
  Then I should see message "Your opponent has forfeited the game on time"
