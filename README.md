[![CI Status](https://github.com/kritzcreek/pscid/workflows/CI/badge.svg)](https://github.com/kritzcreek/pscid/actions)
[![npm version](https://badge.fury.io/js/pscid.svg)](https://badge.fury.io/js/pscid)

pscid
===

An editor agnostic minimal IDE for your shell. Think `spago -w build` on steroids.

### Installation

`npm i -g pscid`

### Usage

Start `pscid` in a terminal in the root folder of your project.

pscid will show you errors and warnings (one at a time) whenever you save a PureScript source file. This makes for a nice iterative workflow.

Type `b` inside `pscid`'s terminal window to build your project. This looks up the `pscid:build` script inside your package.json, then falls back to the `build` script, and then finally tries `spago build`.

Type `t` inside `pscid`'s terminal window to test your project. As with building this looks up the `pscid:test` script first, then `test`, then falls back to `spago test` as a last resort.

Type `q` to quit pscid.

### Suggestions

Some warnings carry a suggestion from the compiler (for example redundant
imports). `pscid` will prompt you to press `s` inside the terminal window when
it encounters such a warning, and automatically apply the suggestion for you.

### Demo

![Demo GIF](http://i.imgur.com/ssBtu6w.gif)

### Options
  - `-p` The port to use. Defaults to 4243
  - `--include -I <dir;dir;...>`  Additional directories for PureScript source files, separated by `;`
  - `--censor-codes <UnusedTypeVar,...>` Warning codes to ignore, seperated by `,` (just like in purescript-psa)
  - `--test` Runs your tests after every successful rebuild
  - `-O/--output` Specifies what output directory to use to load the externs for the server

### Attribution

pscid utilizes https://github.com/natefaubion/purescript-psa to format and enrich the errors and warnings emitted by the compiler.

It's inspired by https://github.com/ndmitchell/ghcid and https://github.com/anttih/psc-pane.

### LICENSE

Copyright 2023 Christoph Hegemann and Contributors

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the LICENSE file for further details.
