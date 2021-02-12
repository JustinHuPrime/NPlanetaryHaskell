# NPlanetary

![GitHub Workflow Status](https://img.shields.io/github/workflow/status/JustinHuPrime/NPlanetary/Haskell%20CI) ![GitHub milestone](https://img.shields.io/github/milestones/issues-open/JustinHuPrime/NPlanetary/1) ![GitHub pull requests](https://img.shields.io/github/issues-pr/JustinHuPrime/NPlanetary) ![GitHub](https://img.shields.io/github/license/JustinHuPrime/NPlanetary)

NPlanetary is an electronic board game loosely based off of the pen-and-paper board game Triplanetary.

The game simulates combat and economics in a two-dimensional recreation of the solar system, and features Newtonian movement rules.

## Installation

This package depends on:

- cabal
- libGL
- libGLU

To install, run `cabal install`

## Usage

The game is split into a client, which views the state of the game board and sends your moves to the server, and the server, which resolves everyone's moves.

Start the client with `nplanetary-client <server-url>`, where `<server-url>` is the URL of the server to connect to.

Start the server with `nplanetary-server 2`.

For more information on game mechanics, see [the wiki](https://github.com/JustinHuPrime/NPlanetary/wiki).
