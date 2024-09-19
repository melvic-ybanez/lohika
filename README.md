# Lohika

Lohika is a proof generator currently being written in Scala. It takes in a logical entailment containing
an optional set of premises and a conclusion.

If premises are given, Lohika will attempt to prove the conclusion from them.
Otherwise, Lohika will check if the conclusion is a _tautology_.

The proof generating logic is based on _resolution calculus_ (more on this
in [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works)).

## Contents

- [Most Relevant Features](#most-relevant-features)
    - [Proof Generation](#proof-generation)
    - [MathJax and Unicodes](#mathjax-and-unicodes)
    - [Supported Logical Symbols](#supported-logical-symbols)
- [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works)
- [Installation](#installation)
- [Screenshots and Demos](#screenshots-and-demos)
- [License](#license)

## Most Relevant Features

### Proof Generation

The user can enter a string in the form `P |= Q`, where `P` is
a set of comma-separated _premises_, and `Q` is the _conclusion_ to prove.

If you want Lohika to prove that a proposition is a tautology, just exclude the premises
by omitting the `P |=` part.

Here are some examples to try:

* `(P & Q) => P` (no premises)
* `P | Q, !Q => R |= (P | Q) & (!Q => R) => (P => R)`
* `A => B, B | C |= A | C`

Lohika's output is a series of steps that show whether the conclusion follows from the
premises, or is a tautology. How Lohika does this is explained
in [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works). Some of the technical terms in the proof
are also rendered as links to Wikipedia pages (though they will be loaded within Lohika's UI directly when clicked, since
Lohika uses [ScalaFX](https://github.com/scalafx/scalafx)'s `WebView`). 

### MathJax and Unicodes

The logical symbols will be replaced by their Unicode equivalent in the input field as the user is writing
his input. The solution's formulas, on the other hand, will be rendered using [MathJax](https://www.mathjax.org/). You
can see this in action in the [Screenshots and Demos](#screenshots-and-demos) section.

### Supported Logical Symbols

| Symbol        | How to write them | Example   |
|---------------|-------------------|-----------|
| Negation      | `!`               | `!P`      |
| Disjunction   | `\|`              | `P \| Q`  |
| Conjunction   | `&`               | `P & Q`   |
| Implication   | `=>`              | `P => Q`  |
| Biconditional | `<=>`             | `P <=> Q` |
| Entailment    | `\|=`             | `P \|= Q` |

## How Lohika's Proof Generator Works

Lohika relies on [proof by contradiction](https://en.wikipedia.org/wiki/Proof_by_contradiction). It will
negate the conclusion, and then see if that will lead to a contradiction. If it will, the original conclusion
must be true.

Lohika's proof generator algorithm is explained in the following steps:

1. Convert the premises into their
   corresponding [conjunctive normal forms (CNFs)](https://en.wikipedia.org/wiki/Conjunctive_normal_form).
1. Add all the clauses from the premises' CNFs to the _clause set_.
1. Negate the conclusion.
1. Convert the negated conclusion into CNF.
1. Add the negated conclusion's CNF to the clause set.
1. Recursively apply the resolution rule until either the options are exhausted or a contradiction is found.
   <br>**TODO**: Add more explainations about this (maybe in its own section).
1. If a contradiction is found, then the conclusion follows from the premises (or is a tautology, if there are
   no premises). Otherwise, the conclusion is not provable from the premises (or is not a tautology).

## Installation

Installation instructions will be added after the first release.

## Screenshots and Demos

Here's what Lohika currently looks like:

![dark_lohika](https://github.com/user-attachments/assets/d7560727-dbc2-4617-bcd9-5ad91fcd8f8d)

**Sample video**:

https://github.com/user-attachments/assets/7200e85d-4a4b-4d7e-9d58-49188940dfb8

## License

[MIT License](LICENSE)
