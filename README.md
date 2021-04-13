# MongoDB-Coding-Challenge
Date:   2021-03-30  
Author: Einar Rasmussen  
Email:  einar@einar.io


*** Update 2021-04-13 ***
After having solved this challenge in Haskell, 
I acknowledged that I should have rather implemented it in JavaScript.

So I did.

The [JavaScript](https://github.com/einar-io/MongoDB-Coding-Challenge.JS)
version is also available on my homepage where you 
can [try it out right now!](https://einar.io/code/MongoDB-Coding-Challenge.JS/)

--Einar
*** End of update ***


Welcome to my first ever coding challenge solution.
It is called *ItziBitzi* because it is not humongous.
ItziBitzi is implemented in Haskell and uses the Aeson JSON-parser library.


## Installation and tool chain
To install Haskell on Ubuntu Linux please run
`$ sudo apt-get install haskell-platform`.

For macOS these instructions may be [helpful](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

You can then clone my repository with
`git clone git@github.com:einar-io/MongoDB-Coding-Challenge.git`.


## Running the solution
I use [stack](https://docs.haskellstack.org/en/stable/README/) as project manager,
so you can build the project with `stack build` and run an interactive session
with `stack run`.  Here you can type or paste JSON objects such as 
`{"a": {"b": {"c": null}}}`
followed by enter or Ctrl-D (end input).  You should then see flattened object returned.
You can type a new one or press Ctrl-C to terminate.


## Testing
If you just want to run my tests, you can run
`stack build` to build the project, and
`stack test` to run the unit tests.
This type of problem is a good fit for property-based testing with QuickCheck,
but I did not pursue this further in concern of time.


## Assumptions
During the challenge I will state any further assumptions explicitly here:

1. You want me to be productive, so I use the tools I would usually use for
   this type of problem.
2. You want me document my thought process, challenges I encounter and design
   decisions.


## Analysis
We essentially want a flattening operation for  JSON objects.  This can be
implemented by a recursive `eval` function like the ones typically used for
interpreters.  We can pass the current path as a state to serve as a prefix.
We can start the process with a nonrecursive helper function `evalH`, that
calls `eval` with an empty path.

While we can generate the correct (semantically speaking) objects, we must
consider two additional things if we want to pass the provided test: Pretty
printing and order.  The former is easy after reducing the object to one level
of nesting.  The latter, however, reveals a fundamental problem: Order is likely
not preserved in the underlying HashMap used by Aeson, and there is no easy
way around it.  To finish this challenge in reasonable time, I decided to sort
the keys in lexicographic order.  This should be enough to make the provided
test pass.  We note this under limitations.


## Limitations
1. The solution is not guaranteed to preserve order of the key-value pairs in the JSON object.
2. Recursion may use stack space.  If we have excessively nested objects, they
   may exhaust the available stack space.


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
20:20 Finished `README.md`.  Writing a few more unittests.  
20:35 Finished unittests.  


## The Takeaway
What did I learn in this challenge?

1.  Working with arbitrarily shaped JSON objects in Haskell is not super convenient.
    If I had to do it over again, I would consider using JavaScript or another
    dynamically typed language.
2.  Working with internal data representation of libraries, you are not
    familiar with is too time consuming to work well in coding challenges.
3.  Haskell libraries usually do not use the type String to represent text.


## Thank you for reading!
Do not hesitate to contact me, if you have questions or need advice on how to
use my solution.

Kind regards  
Einar
