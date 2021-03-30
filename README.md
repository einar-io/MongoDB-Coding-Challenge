# MongoDB-Coding-Challenge
Date:   2021-03-30
Author: Einar Rasmussen
Email:  einar@einar.io

The solution is implemented in Haskell.

## Installation
To install Haskell on Ubuntu Linux, run:
`$ sudo apt-get install haskell-platform`

The you can clone this repository:
`git clone git@github.com:einar-io/MongoDB-Coding-Challenge.git`

## Tool chain
[stack](https://docs.haskellstack.org/en/stable/README/) as project manager.


## Assumptions
During the challenge will state any further assumptions explicitly here:

1. You want me to be productive, so I use the tools I use most.
2. You want me document my thought process and challenges I encounter.
3. You want me document my thought process.


## Analysis
We essentially want a JSON object flattening operation.
This can be implemented by a recursive `eval` function
like the ones used for interpreters where we pass
the current path as state to serve as a prefix.
We can the start the process with a helper function
`evalH`, that calls `eval` with an empty prefix.

While we can generate the correct (semantically speaking) objects,
we must consider two additional things, if we want to pass
the provided test: Pretty printing and order.
The former is easy after reducing the object to one level of nesting.
The latter, however, posses a fundamental problem:
Order is likely not preserved in the underlying HashMap used by
Aeson, and there is not easy way around it.  
To finish this challenge in reasonable time, I decide to
sort the keys in lexicographic order.  This should
be enough to make the provided test pass.


## Limitations
1. The solution will not preserve arbitrary order of the members in the JSON object.
2. Recursion uses the stack.  If we have very deeply nested objects, they
   may exhaust the available stack space.


## Testing
`stack test` builds and runs the unit tests.


### Timeline
12:00 Sent start mail
12:08 Repo in place
12:38 Written control-flow outline
      Simple echo service works
      Found a suitable JSON lib (Aeson)
12:55 Resolved issue with JSON lib
15:00 Finally steared clear of some conversion issues between standard Strings
      and the internal text representation that Aeson uses.
15:13 Setting up tests.
15:50 Testing also displays challenges with text representation.  Taking a short break.
16:08 Back from break.
17:09 Fixed type conversion in the tests.
18:10 Own tests are passing.  A fundamental problem is that order may not be preserved.
18:28 Added limitations to this `README.md`.

## What did I learn in this challenge?
1. 

