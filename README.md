# MongoDB-Coding-Challenge
Date:   2021-03-30  
Author: Einar Rasmussen  
Email:  einar@einar.io

Welcome to my first ever coding challenge solution.
It is called *ItziBitzi* because it is not humongous.
ItziBitzi is implemented in Haskell.

## Installation and tool chain
To install Haskell on Ubuntu Linux, run:
`$ sudo apt-get install haskell-platform`

You can then clone my repository:
`git clone git@github.com:einar-io/MongoDB-Coding-Challenge.git`

I use [stack](https://docs.haskellstack.org/en/stable/README/) as project manager,
so you can build the project by `stack build` and run and interactive session
with `stack run`.  Here you can type in JSON objects such as 
`{"a": {"b": {"c": null}}}`
followed by enter or Ctrl-D (end input).  You should then see flattened object.
You can type a new one or press Ctrl-C to terminate.

For macOS these instructions may be [helpful](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Assumptions
During the challenge will state any further assumptions explicitly here:

1. You want me to be productive, so I use the tools I would usually use for
   these types of problems.
2. You want me document my thought process and challenges I encounter.
3. You want me document my thought process.


## Analysis
We essentially want a flattening operation for  JSON objects.  This can be
implemented by a recursive `eval` function like the ones used for interpreters.
We can pass the current path as a state to serve as a prefix.  We can start the
process with a nonrecursive helper function `evalH`, that calls `eval` with an
empty path.

While we can generate the correct (semantically speaking) objects, we must
consider two additional things, if we want to pass the provided test: Pretty
printing and order.  (1) is easy after reducing the object to one level
of nesting.  (2) however, reveals a fundamental problem: Order is likely
not preserved in the underlying HashMap used by Aeson, and there is not easy
way around it.  To finish this challenge in reasonable time, I decide to sort
the keys in lexicographic order.  This should be enough to make the provided
test pass.


## Limitations
1. The solution will not preserve arbitrary order of the members in the JSON object.
2. Recursion uses the stack.  If we have very deeply nested objects, they
   may exhaust the available stack space.


## Testing
`stack build` builds the project.
`stack test` builds _and_ runs the unit tests.


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
18:58 My three working tests now pass.  I will add more tests and then wrap up.


## The Takeaway
What did I learn in this challenge?

1.  Working with arbitrarily shaped JSON objects in Haskell is not super convenient.
    If I had to do it over again, I would probably have chosen JavaScript.
2.  Working with internal data representation of libraries, you are not familiar with
    is too time consuming to work well in coding challenges.
