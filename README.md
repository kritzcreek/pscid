[![npm version](https://badge.fury.io/js/pscid.svg)](https://badge.fury.io/js/pscid) 
[![Dependency Status](https://www.versioneye.com/user/projects/5714bac7fcd19a004544136d/badge.svg?style=flat)](https://www.versioneye.com/user/projects/5714bac7fcd19a004544136d)

pscid
===

An unintrusive, lightweight tool to make writing PureScript nicer.

Currently needs the unreleased master branch of the purescript compiler to be on
your path.

pscid uses @natefaubion's excellent `purescript-psa` to format the
errors/warnings and look up source files.

### Installation

`npm i -g pscid`

### Usage

Your project needs to have been built at least once. Then call `pscid` in your
projects root folder.

pscid will show you errors and warnings (one at a time) in whatever file you
edit+save.

If you hit `b` inside pscid's console window it will try to either run `npm run
build` or `pulp build`.

Pressing `q` quits pscid.

### Options
  - `-p` The port to use. Defaults to 4243

### Demo

![Demo GIF](http://i.imgur.com/ssBtu6w.gif)
