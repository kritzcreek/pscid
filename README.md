[![Build Status](https://dev.azure.com/christophhegemann1337/pscid/_apis/build/status/kRITZCREEK.pscid?branchName=master)](https://dev.azure.com/christophhegemann1337/pscid/_build/latest?definitionId=1&branchName=master)
[![npm version](https://badge.fury.io/js/pscid.svg)](https://badge.fury.io/js/pscid)

pscid
===

An editor agnostic minimal IDE for your shell. Think `pulp -w build` on steroids.

### Installation

`npm i -g pscid`

### Usage

Start `pscid` in a terminal in the root folder of your project.

pscid will show you errors and warnings (one at a time) whenever you save a PureScript source file. This makes for a nice iterative workflow.

Type `b` inside `pscid`'s terminal window to build your project. This looks up the `pscid:build` script inside your package.json, then falls back to the `build` script, and then finally tries `spago/pulp build`.

Type `t` inside `pscid`'s terminal window to test your project. As with building this looks up the `pscid:test` script first, then `test`, then falls back to `spago/pulp test` as a last resort.

Type `q` to quit pscid.

### Suggestions

Some warnings carry a suggestion from the compiler (for example redundant
imports). `pscid` will prompt you to press `s` inside the terminal window when
it encounters such a warning, and automatically apply the suggestion for you.

#### CAREFUL: This modifies the file in place.

If something goes horribly wrong you might lose your uncommited changes. Commit
often and trust in the types I guess...

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

Copyright 2018 Christoph Hegemann and Contributors

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the LICENSE file for further details.
