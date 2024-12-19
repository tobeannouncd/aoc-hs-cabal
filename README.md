# Advent of Code Solutions

*Migrated [previous project](https://github.com/tobeannouncd/aoc-hs) to use Cabal instead of Stack.*

This is a repository of my solutions to the annual [Advent of Code](https://adventofcode.com) puzzles using Haskell. Previous years have been solved in various languages, and will eventually be moved to this repo.

## Running solutions

Solutions can be run in various ways:

- Running with no command line arguments will download the day's input from the website.
    * Your session cookie is read from the `AOC_SESSION` environment variable.
    * You can set the `AOC_CACHE` environment variable to manually set the directory where inputs are cached.
- If given a filename argument, reads the given file.
- Hyphen `-` reads from stdin.
- Plus `+` reads input from the next command line argument interpreted as a string literal.