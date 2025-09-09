Implementations of the Lox programming language from [_Crafting Interpreters_](https://www.craftinginterpreters.com/) by Robert Nystrom.

## Java

Similar to the implementation from the book, but using more features from modern Java.
The biggest difference is that I replaced the visitor pattern with functional-style pattern matching on records.

## Janet

This whole thing is mainly a project for learning Janet, so I'm not going to claim it's idiomatic or high quality Janet code.
It's not as far along as the Java version either but it does have some interesting aspects,
most notably Janet's support for [parsing expression grammars](https://janet-lang.org/docs/peg.html)
made the Janet scanner a fraction a size of the Java one.
I also implemented proper Ctrl-C and Ctrl-D support in the repl.
