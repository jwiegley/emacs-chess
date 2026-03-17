# Emacs Chess

I started writing chess.el back in early 2001, right after I beat my father
at chess for the very first time in thirty years of trying. I'd looked up a
few simple strategies on the Web -- things like building a strong center --
and suddenly everything clicked. That same week, Emacs Chess was born.

I was visiting my father at his home in upstate New Jersey at the time, and I
remember spending days and nights just hacking away on this code. What you'll
find here is pure passion -- an obsession that took me by storm for over two
years until I moved on to other things.

## Getting started

Type `M-x chess` and play. Emacs Chess automatically detects whatever engine
you have installed -- Stockfish, GNU Chess, Crafty, Phalanx, Sjeng --
provided it's on your PATH. If you don't have any of those, there's a pure
Elisp AI built in. It's not very strong, but it'll play.

Use `C-u M-x chess` to pick a specific engine.

## Playing against humans

For a direct Emacs-to-Emacs game over TCP:

    C-u M-x chess RET network RET

To play on an Internet Chess Server like freechess.org:

    M-x chess-ics

## Other features

- **PGN editing**: `chess-pgn-mode` provides a major mode for browsing and
  editing Portable Game Notation files, displaying the board position at
  point.

- **Puzzles**: `M-x chess-puzzle` lets you work through puzzle collections in
  EPD or PGN format against any installed engine.

- **Tutorial**: `M-x chess-tutorial` is a simple knight movement exercise to
  get you started.

## Display options

There are a few ways to see the board:

- **chess-images** -- graphical display with piece image sets (the default in
  GUI Emacs)
- **chess-ics1** -- verbose ASCII chessboard (fallback for terminal use)
- **chess-plain** -- minimal character display

Customize `chess-default-display` to choose.

## Architecture

The whole design is modular. Every display renderer, engine wrapper, utility
module (sound, clock, autosave), and database backend is a self-contained
module -- a named buffer with a handler function that responds to game events
through the game's event hook system.

This loose coupling is what made it possible for Mario Lang to add braille
display support as just another module. Mario can't see, but Emacs Chess's
architecture made it the only chess client that works with his braille viewer.
He's been playing with it ever since, and he's contributed the German
translations, `chess-eco`, and a fair amount of code and testing over the
years.

## Installation

Emacs Chess is available through GNU ELPA:

    M-x package-install RET chess RET

## Development

With Nix:

```bash
nix develop                  # enter dev shell with all dependencies
lefthook install             # set up pre-commit hooks
nix flake check              # run all checks (build, test, lint, format)
nix build                    # build the package
```

Without Nix:

```bash
emacs -batch -L . -f batch-byte-compile *.el          # byte-compile
emacs -batch -L . -l chess-perft -f ert-run-tests-batch  # run perft tests
```

The pre-commit hooks (via lefthook) run byte-compilation, indentation checks,
linting, and the perft test suite automatically. See `lefthook.yml` for
details.

## License

See [COPYING](COPYING) for license details.
