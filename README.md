# MiniPPL in native R syntax

## About this implementation

As a final project for my **Introduction to Probabilistic Programming Languages** course at the University of Buenos Aires (UBA), in June 2026, students were asked to rewrite the small probabilistic programming language developed in class using a programming language—and preferably a programming style—of their choice. The assignment can be found [here](https://jburroni.github.io/teaching/ppl-2026/).

Ezequiel Birman later pointed out that none of the groups had chosen **R**, despite R being a particularly interesting language for this exercise. Besides its obvious connection to statistics, R has several unusual language features that are relevant to interpreter design: lazy evaluation through promises, first-class environments and lexical closures, and extensive facilities for metaprogramming and non-standard evaluation.

So I asked GPT to produce an R implementation, with an additional constraint: it should not merely translate the existing interpreter into R, but should take advantage of these distinctive features of the language and try to make the result *idiomatically R*. In particular, the implementation uses R's native syntax trees and `substitute()` instead of writing a parser, environments and native closures for lexical scope, promises and selective forcing where appropriate, and a small continuation-based layer for probabilistic effects.

Here it is.

---

This is a self-contained base-R translation of the MiniPPL used in the June 26 material. It keeps the original source-language constructs and message interface, but it does not make R impersonate a Lisp with hand-built `list()` syntax.

A model is written as ordinary R code inside `ppl(...)`:

```r
conjugate <- ppl({
  mu ~ normal(0, 1)
  observe(2.3 ~ normal(mu, 1))
  mu
})
```

The implementation uses four particularly R-like mechanisms:

1. `ppl()` receives its argument as an unevaluated **promise**. `substitute()` captures the expression without forcing it, so R's own parser supplies the AST.
2. Source functions compile to actual **R closures**. R environments provide lexical scope, including closure capture and mutually recursive top-level definitions.
3. The compiler selectively defeats host laziness where the original language is strict: operators and function arguments are evaluated left-to-right before application, and source arguments are explicitly forced. Native `if` remains branch-lazy.
4. Only probabilistic effects use explicit CPS. `sample` and `observe` suspend into a small, trampolined **continuation monad** and expose reusable R closures to inference controllers.

No package installation is required.

## Run it

```r
source("minippl.R")
source("examples.R")  # demonstrations
source("tests.R")     # semantic and inference checks
```

## Surface language

The R surface is a direct spelling of the original language, not an extension of it.

| MiniPPL construct | R spelling |
|---|---|
| literal / variable | ordinary R literal or name |
| sequential `let` | statements such as `x <- expression` inside `{ ... }` |
| `if` | `if (test) then else alternative` |
| `fn` | `function(x, ...) body` |
| application | an ordinary R call, including infix operators |
| `sample` | `sample(distribution)` |
| `observe` | `observe(distribution, value)` |
| top-level `defn` | a leading `name <- function(...) ...` declaration |

Two pieces of statistical syntax are optional desugarings:

```r
x ~ distribution                 # x <- sample(distribution)
observe(value ~ distribution)    # observe(distribution, value)
```

A contiguous run of function assignments at the beginning of a program is treated as the original top-level `defn` section, so recursion is natural:

```r
geometric <- ppl({
  geom <- function() {
    if (sample(bernoulli(0.3))) 0 else 1 + geom()
  }
  geom()
})
```

Assignments after the declaration section are sequential lexical bindings. Rebinding creates a fresh frame instead of mutating a frame captured by an earlier closure:

```r
lexical <- ppl({
  x <- 10
  f <- function(y) x + y
  x <- 20
  f(1)                         # 11, not 21
})
```

## Why call-by-need helps—but is not the continuation

R promises are excellent for capturing syntax and controlling when source fragments are inspected. They are not enough to implement a probabilistic effect. A promise means “compute this expression later”; at a sample site the controller needs “continue the rest of the program with the value I choose.”

The core computation type is therefore:

```text
P a = (a -> Bounce (Step r)) -> Bounce (Step r)

Step r = Done r
       | Sample address distribution (value -> Step r)
       | Observe address distribution value (() -> Step r)
```

`Bounce` is a trampoline. It permits deep source recursion without relying on tail-call optimization from R.

At `sample`, the continuation is an ordinary reusable R closure. Likelihood weighting invokes it once with a random draw. SMC can share it among resampled descendants. Exact enumeration invokes it once per support value. There is no copied control stack or mutable machine state.

## Evaluation order

R itself is call-by-need, but the source language remains call-by-value:

```r
strict <- ppl({
  constant <- function(x) 7
  constant(observe(2.3 ~ normal(0, 1)))
})
```

The observation is performed even though `constant` does not use `x`. Calls evaluate the operator first and then every argument from left to right. Likewise, `&&` and `||` are strict aliases of the source-language logical primitives; only `if` chooses a branch lazily.

## Message interface and controllers

All controllers consume the same three step shapes:

```text
Done(value)
Sample(address, distribution, continuation)
Observe(address, distribution, value, continuation)
```

Included controllers:

```r
run_lw(model, seed = 1)
likelihood_weighting(model, N = 10000, seed = 1)
smc(model, N = 1000, seed = 1)
single_site_mh(model, steps = 10000, warmup = 2000, seed = 1)

enumerate_traces(model)
posterior_table(enumerate_traces(model))
```

The exact controller is intentionally minimal and branches only on Bernoulli samples, as in the finite eight-bit exercise.

For the conjugate model:

```r
lw <- likelihood_weighting(conjugate, N = 5000, seed = 1)
sum(lw$values * lw$weights)

particles <- smc(conjugate, N = 5000, seed = 2)
mean(particles)

chain <- single_site_mh(conjugate, 10000, warmup = 2000, seed = 3)
mean(chain)
```

The exact posterior mean is `1.15`, with standard deviation `sqrt(0.5)`.

## Addresses

Addresses are structural paths through the captured R syntax. Components identify body expressions, sequential bindings, tests and branches, function positions, arguments, distribution expressions, and observed values. A source function receives its dynamic call-site address through a hidden formal, so recursive calls acquire distinct nested addresses.

`address_key()` renders a path for address-keyed traces used by single-site MH.

## Deterministic prelude

The closed source environment contains the same categories as the course implementation:

- arithmetic, comparisons, and logical primitives;
- persistent vectors and maps, with zero-based `get`/`nth` indexing;
- matrix primitives;
- Normal, log-Normal, beta, gamma (shape/rate), exponential, continuous and discrete uniform, Poisson, Bernoulli, categorical, and Dirichlet distributions.

R spellings such as `^`, `%%`, `!`, `&&`, `||`, and `c(...)` are aliases for existing source primitives, not additional capabilities. Names that are not legal bare R identifiers remain available with backticks, for example:

```r
`hash-map`("answer", 42)
`uniform-continuous`(0, 1)
`empty?`(c())
```

The generated program environment is closed above this prelude. A model cannot silently call arbitrary functions from the user's R session.

## Deliberate limits

To stay at the size of the original language:

- there is no custom parser;
- there is no escape to arbitrary host R evaluation;
- source functions have positional parameters only—no defaults, `...`, or named calls;
- assignment is lexical binding, not mutable state;
- `x ~ d` is allowed only as a statement, and `observe(y ~ d)` is only syntax for the ordinary two-argument `observe`;
- exact enumeration supports Bernoulli sample sites only.

`show_compiled(model)` prints the generated continuation code, which is useful for seeing exactly where R's syntax ends and the probabilistic effect layer begins.

## Files

- `minippl.R` — loader.
- `prelude.R` — values, distributions, and deterministic primitives.
- `runtime.R` — CPS compiler, continuation runtime, and inference controllers.
- `examples.R` — closure, recursion, conjugacy, and exact-enumeration examples.
- `tests.R` — executable semantic and numerical checks.
