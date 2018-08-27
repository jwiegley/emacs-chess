Scenario: en-passant logic assumed no pre-moves (white)
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And white selects "d4"
  Then the square at "d3" is unhighlighted

Scenario: en-passant logic assumed no pre-moves (black)
  Given I start server and client
  Then I am ready to play
  When white moves "d4"
  And black moves "e5"
  And black selects "e5"
  Then the square at "e6" is unhighlighted

Scenario: pre-move can leave king in check, assuming checking piece is captured
  Given I start server and client
  Then I am ready to play
  And I set position of "*chess-network*<1>" to fen "3rk3/8/8/3R4/8/8/PPPPPPPP/1NBQKBNR w - -"
  And I send position from "*chess-network*<1>"
  When black selects "e8"
  And black selects "d8"
  Then the square at "d8" is highlighted pre-move


Scenario: pre-move promotion should not ask yet
  Given I start server and client
  Then I am ready to play
  And I set position of "*chess-network*<1>" to fen "rnbqkbn1/pppppppP/8/8/8/8/PPPPPPP1/RNBQKBNR w - -"
  And I send position from "*chess-network*<1>"
  And white moves "d4"
  And white selects "h7"
  And white selects "h8"
  Then the square at "h8" is highlighted pre-move
