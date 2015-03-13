# RobberyRL
RobberyRL is a roguelike made in 7 days using [Purescript](http://www.purescript.org/).

The game is a turn-based platformer roguelike with sneaking and thievery.

The game works in Chrome and Internet Explorer 11. It does not work in Firefox (tested with version 36.0.1). For some reason the max recursion limit is reached and the program stops working.

[You can play the (currently unfinished) game here.](https://dl.dropboxusercontent.com/u/51067730/7drl2015/index.html)

#### Development blog

[You can read daily development reports here.](https://teamkalamakkara.wordpress.com/)

#### How to build from the sources

Install [Node.js](https://nodejs.org/), [Haskell Platform 2014.2.0.0](https://www.haskell.org/platform/) and [Purescript](http://www.purescript.org/) and then run the following commands:

```
npm install -g grunt-cli bower
npm install
bower update
grunt
```
