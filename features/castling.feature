Scenario: e1-b1 is misinterpreted as O-O-O when it should be illegal
  Given game with fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w KQkq -"
  Then the move "e1-b1" is illegal

Scenario: e8-b8 is misinterpreted as O-O-O when it should be illegal
  Given game with fen "r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq -"
  Then the move "e8-b8" is illegal
