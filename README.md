<p align="center"><img src="https://github.com/BeardedPlatypus/elm-node-editor/blob/master/node_editor.png?raw=true" alt="PacMan" title="PacMan" width=40% /></p>

# elm-node-editor
[![Build Status](https://dev.azure.com/mwtegelaers/elm-node-editor/_apis/build/status/BeardedPlatypus.elm-node-editor?branchName=master)](https://dev.azure.com/mwtegelaers/elm-node-editor/_build/latest?definitionId=18&branchName=master)

A (very) basic node editor implementation written in elm. A live demo 
of the latest revision can be found [here](https://beardedplatypus.github.io/elm-node-editor/).

## Motivation

As part of a personal project, I am developing a simple locally hosted
website in which I can connect inputs with outputs. In order to display
this intuitively I opted to represent this as a node editor. Unfortunately,
I could not find any elm libraries that provide such an interface out of
the box. Thus this little bit of code was born.

## Implementation

The `elm-node-editor` leverages the Elm SVG library to represent the 
different nodes. The current implementation is more of a proof-of-concept.
It fits the needs I have for my specific website. Ideally, I will extend
this into a separate library if time allows, but for the time-being I have
made the source code available.