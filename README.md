# Applications using Monad-Bayes

This is a companion repo to the probabilistic programming library [monad-bayes](https://github.com/tweag/monad-bayes) which shows some of the cool things you can do in combination with other Haskell libraries.

It also hosts a tutorial series on monad-bayes, arranged as Jupyter notebooks.

Applications:

### Language Understanding

An increasingly popular model of natural language communication involves a Bayesian agent reasoning about their interlocutor. 

TODO implements such a model, with a probabilistic context free grammar, a probabilistic parser, and a probabilistic semantics.

Interesting Haskell libraries used:

- aeson
- free
- recursion-schemes
- megaparsec

### Ising Model

TODO
is a nice example of how to use a comonad to implement an efficient Conway's Game of Life. Using the same approach, 

TODO implements an ising model in this fashion, and samples from it with MH.

GIF

Interesting Haskell libraries used:
- adjunctions
- comonad

### Physics simulation

Hamilton
    todo
is a simulator for classical physics on arbitrary manifolds. 

Combined with monad-bayes, we can propagate a distribution forward,

or infer the Hamiltonian from a trajectory, using SMC.

Interesting Haskell libraries used:
- Hamilton

### 