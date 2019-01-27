# A Simple Algorithm and Proof for Type Inference
## What is this?
This repository contains my OCaml implementation of Mitchell Wand's paper: ["A Simple Algorithm and Proof for Type Inference"](http://web.cs.ucla.edu/~palsberg/course/cs239/reading/wand87.pdf)(1987).

This papers lays the foundations for type inference algorithms based on constraint generation and unification.

## How do I use it?
The implementation is in `inference.ml`, just run it with `ocaml inference.ml`.

The `test` function inference takes an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) as input and prints the type of the AST and the generated equations. To solve the equations an algorithm such as [Huet's first order unification algorithm](https://ac.els-cdn.com/0304397575900110/1-s2.0-0304397575900110-main.pdf?_tid=1f97c4e6-cec0-4991-a0b2-18d161f16c10&acdnat=1548627201_16cee2ccac3cd987c8ac0fa99c54bdb0) can be used.

The AST is just Lambda Calculus so Variables, Lambda abstractions and Applications are defined.
# Wand87-A-Simple-Algorithm-and-Proof-for-Type-Inference
