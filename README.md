[![npm version](https://badge.fury.io/js/pscid.svg)](https://badge.fury.io/js/pscid) 
[![Dependency Status](https://www.versioneye.com/user/projects/5714bac7fcd19a004544136d/badge.svg?style=flat)](https://www.versioneye.com/user/projects/5714bac7fcd19a004544136d)

pscid
===

An editor agnostic minimal IDE for your shell. Think `pulp -w build` on steroids.

### Installation

Requires purescript >= v0.8.5 to be installed.

`npm i -g pscid`

### Usage

Start `pscid` in a terminal in the root folder of your project.

pscid will show you errors and warnings (one at a time) whenever you save a PureScript source file. This makes for a nice iterative workflow.

Type `b` inside `pscid`'s terminal window to build your project. This looks up the `build` script inside your package.json and failing to find that will run `pulp build`.

Type `q` to quit pscid.

### Demo

![Demo GIF](http://i.imgur.com/ssBtu6w.gif)

### Options
  - `-p` The port to use. Defaults to 4243

### Attribution

pscid utilizes https://github.com/natefaubion/purescript-psa to format and enrich the errors and warnings emitted by the compiler.

It's inspired by https://github.com/ndmitchell/ghcid and https://github.com/anttih/psc-pane.
