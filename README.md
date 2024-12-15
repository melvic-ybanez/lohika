# Lohika

Lohika is a proof generator for entailments and tautologies in [first-order logic](https://en.wikipedia.org/wiki/First-order_logic). It takes a logical statement, written in first-order logic, 
and applies resolution calculus to it (see [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works)) to
generate a proof. If premises are provided in the input, Lohika will attempt to prove the conclusion from them. Otherwise, Lohika will check if the statement is a _tautology_.

## Contents

- [Most Relevant Features](#most-relevant-features)
    - [Proof Generation](#proof-generation)
    - [Editor View](#editor-view)
    - [MathJax](#mathjax)
    - [Supported Logical Symbols](#supported-logical-symbols)
- [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works)
- [Install and Run Lohika](#install-and-run-lohika)
- [Screenshots and Demos](#screenshots-and-demos)
- [Icons](#icons)
- [License](#license)

## Most Relevant Features

### Proof Generation

The user can enter a string in the form `P |= Q`, where `P` is
a set of comma-separated _premises_, and `Q` is the _conclusion_ to prove. If no premises are 
provided (i.e. the `P |=` part is omitted), Lohika will show whether the given proposition 
is a _tautology_.

Here are some examples to try:

* `A:a,b[P(a, b) & Q(b) -> P(a, b)]` (no premises)
* `P(a) |= E:xP(x)`
* `E:xP(x), A:x(P(x) -> Q(x)) |= E:xQ(x)`

Lohika's output is a series of steps that show whether the conclusion follows from the
premises, or is a tautology. How Lohika does this is explained
in [How Lohika's Proof Generator Works](#how-lohikas-proof-generator-works). Some of the technical terms in the proof
are also rendered as links to Wikipedia pages (though they will be loaded within Lohika's UI directly when clicked, since
Lohika uses [ScalaFX](https://github.com/scalafx/scalafx)'s `WebView`). 

### Editor View

Lohika supports a simple _editor view/pane_ that allows the user to enter complicated logical entailments. For now,
it only contains the most basic features such as syntax-highlighting, auto-indentation, and line numbering, but
there are plans to improve it in the future (e.g. support for file and directory view).

### MathJax

The formulas in the Solutions View will be rendered using [MathJax](https://www.mathjax.org/). You
can see this in action in the [Screenshots and Demos](#screenshots-and-demos) section.

### Supported Logical Symbols

| Symbol                 | How to write them  | Example               |
|------------------------|--------------------|-----------------------|
| Negation               | `!`                | `!P`                  |
| Disjunction            | `\|`               | `P \| Q`              |
| Conjunction            | `&`                | `P & Q`               |
| Implication            | `->`               | `P -> Q`              |
| Biconditional          | `<->`              | `P <-> Q`             |
| Entailment             | `\|=`              | `P \|= Q`             |
| Universal Quantifier   | `A:`               | `A:x,y(P(x) -> Q(y))` |
| Existential Quantifier | `E:`               | `E:a,bR(a,b)`         |
| Define-as Operator     | `:=`               | `A := P & R`          | 

## How Lohika's Proof Generator Works

Lohika relies on [proof by contradiction](https://en.wikipedia.org/wiki/Proof_by_contradiction). It will
negate the conclusion, and then see if that will lead to a contradiction. If it will, the original conclusion
must be true.

Lohika's proof generator algorithm is explained in the following steps:

1. **Unfold** the entailment. Unfolding in Lohika means replacing any defined terms and formulas 
    with their corresponding definitions.
1. Negate the conclusion.
1. Convert the premises and the negated conclusion into their
   corresponding [conjunctive normal forms (CNFs)](https://en.wikipedia.org/wiki/Conjunctive_normal_form). The following preparatory steps are needed before we can convert to CNF:
   1. Conditionals elimination (both implications and bi-conditionals).
   1. Conversion to [Negation Normal Form (NNF)](https://en.wikipedia.org/wiki/Negation_normal_form).
   1. **Standardization**. This eliminates potential name clashes by applying a series of _alpha-conversions_ to make all first-order variables unique.
   1. Conversion to [Prenex Normal Form (PNF)](https://en.wikipedia.org/wiki/Prenex_normal_form).
   1. [Skolemization](https://en.wikipedia.org/wiki/Skolem_normal_form). This removes the existential quantifiers by replacing them with
       _Skolem constants_ and/or _Skolem functions_.
   1. Dropping of universal quantifiers. All remaining variables at this point are assumed to be universally bound.
   
   Once those steps are completed, we can proceed with the classic CNF conversion as if the formulas weren't in first-order.
1. Add all the clauses from the resulting CNFs to the _clause set_.
1. Choose a pair with which we can apply the **resolution rule** to either derive a new clause or find **complementary literals**.

   If it's the former, the new clause is added to the clause set, and we apply resolution for a new selected pair. If, however,
   complementary literals are found, we take that as a contradiction, and the proof is complete.

   If we have exhausted all possible options (i.e. tried all possible pairs) and found no contradictions and derived no new clauses, we conclude
   that the proposition to prove does not follow from the premises (not semantically valid). 

## Install and Run Lohika

1. Install Java, if you haven't already. Lohika is written in Scala, so the executable needs a JRE to run.
1. Head to the [releases](https://github.com/melvic-ybanez/lohika/releases) page and get the jar file of the latest release.
1. Run it via the Java jar command: 
    ```shell
   $ java -jar <path-to-lohika>/lohika-ui-<version>.jar
   ```

## Screenshots and Demos

Here's what Lohika currently looks like:

<img width="1728" alt="load_scripts" src="https://github.com/user-attachments/assets/0f4a9701-1991-4715-8e83-73348b885070" />

And here's a link to a sample video: https://drive.google.com/file/d/10Gyg7z0SSfMt3wRMeieAtYrDw8J0QHEa/view?usp=drive_link

**Note:** The video above is already outdated. See [this](https://github.com/melvic-ybanez/lohika/issues/32) issue and upload an updated video.

## Icons

This project uses icons download from the following sources:

* Uicons by <a href="https://www.flaticon.com/uicons">Flaticon</a>

## License

[MIT License](LICENSE)
