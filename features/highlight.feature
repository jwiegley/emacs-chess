Scenario: legal highlights should not persist across piece selection
  Given I start server and client
  Then I am ready to play
  When white selects "d2"
  And white selects "d2"
  And white selects "e2"
  Given I switch to buffer "*Chessboard*"
  Then the square at "d3" is unhighlighted

Scenario: preserve last-move highlight after changing my mind
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And black moves "e5"
  And white selects "d4"
  And white selects "d4"
  Then the square at "e5" is highlighted last-move
  When black selects "e5"
  And black selects "e5"
  Then the square at "e5" is highlighted last-move

Scenario: preserve last-move highlight after pre-move invalidated
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And black moves "e5"
  And white moves "e4"
  And white moves "d5"
  Then the square at "d5" is highlighted pre-move
  When black moves "ex"
  Given I switch to buffer "*Chessboard*"
  Then the square at "d4" is highlighted last-move

Scenario: Legal highlight locus must needs reduce after opponent moves
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And black moves "d5"
  And black selects "e7"
  Then the square at "e5" is highlighted legal
  Then the square at "f6" is highlighted legal
  And white moves "e4"
  Given I switch to buffer "*Chessboard*<2>"
  Then the square at "e5" is highlighted legal
  Then the square at "f6" is unhighlighted

Scenario: My opponent invalidates my pre-move.  I should not need to click twice for next selection.
  Given I start server and client
  Then I am ready to play
  And white moves "d4"
  And black moves "e5"
  When black selects "e5"
  Then the square at "e5" is highlighted selected
  Then the square at "e4" is highlighted legal
  And white moves "dx"
  Given I switch to buffer "*Chessboard*<2>"
  Then the square at "e5" is highlighted last-move
  Then the square at "e4" is unhighlighted
  When black selects "c7"
  Then the square at "c7" is highlighted selected

Scenario: paint-move had contained a redraw that was 2000 microseconds (still 1/200 of blink of an eye)
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And black moves "c5"
  And paint-move last 2 plies less than 300 microseconds (individually)
