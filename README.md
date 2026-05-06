# Olox

A Lox interpreter implemented in OCaml, following Part 1 of Robert Nystrom's [Crafting Interpreters](https://craftinginterpreters.com/). It's a fully-featured scripting language with classes, inheritance, methods, closures, and proper lexical scoping.

## How It Works

Code flows through four main stages:

1. Scanner: Reads characters, produces tokens
2. Parser: Builds an AST from tokens  
3. Resolver: Walks the AST to figure out which scope each variable lives in
4. Interpreter: Evaluates the AST

This is a tree-walk interpreter, the simplest kind, but it works.

## The Pieces

- Scanner (`lib/scanner.ml`): Turns raw source into tokens. Handles keywords, strings, numbers, operators, and syntax errors.
- Parser (`lib/parser.ml`): Recursive descent parser that respects operator precedence. Parses expressions, statements, functions, classes (with inheritance), and control flow.
- Resolver (`lib/resolver.ml`): Before we run any code, this walks the AST and figures out which scope each variable reference belongs to. Makes closures work correctly.
- Interpreter (`lib/interpreter.ml`): The actual execution engine. Manages environments, function calls, method dispatch, and field access.

## Performance

Running `fib(35)`:

| Interpreter | Time (mean ± σ) |
|-------------|-----------------|
| Olox        | 5.578 s ± 0.023 s |
| Python 3.14 | 1.217 s ± 0.025 s |

OCaml is ~4.6x slower than Python on this recursive benchmark. The benchmark runs the same fibonacci algorithm (`fib(35)`) in both languages.

## What's Next

I plan to continue with Part 2 of Crafting Interpreters, implementing a bytecode compiler and virtual machine in Zig. Follow along at https://tangled.org/did:plc:2hpdf22ft2ypzciw222xt4au


