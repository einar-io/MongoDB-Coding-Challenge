# MongoDB-Coding-Challenge


## Tool chain
The solution is implemented in Haskell and uses the stack project manager.
`stack test` builds and runs the unit tests.

## Installation
$ sudo apt-get install haskell-platform
`git clone git@github.com:einar-io/MongoDB-Coding-Challenge.git`


## Assumption
During the challenge will state any further assumptions explicitly here:

0. You want me to be productive, so I use the tools I use most.
1. YOu want me document my thought process.


## Analysis
We essentially want a JSON object flattening operation.
This can be implemented by a recursive `eval` function
like the ones used for interpreters where we pass
a the current path as state to serve as a prefix.
We can the start the process with a helper function
`evalH` calling the main function with an empty prefix.


## Testing
`stack test`

### Timeline
12:00 Sent start mail
12:08 Repo in place
12:38 Written control-flow outline
      Simple echo service works
      Found a suitable JSON lib (Aeson)
12:55 Resolved issue with JSON lib
15:00 Finally steared clear of some conversion issues between standard Strings
      and the internal text representation that Aeson uses.
