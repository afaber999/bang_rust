# BANG compiler in Rust

This is my attempt to write a compiler Bang compiler in Rust (for more information about the Bang lanuage and Basm see YouTube video's of Alexey Kutepov on [Tsoding Daily](https://www.youtube.com/channel/UCrqM0Ym_NbK1fqeQG2VIohg) ).
You can download the entire tool chain form his [github page](https://github.com/tsoding/bm), you can disassemble the output of the compiler with the debasm tool and execute the the output of the Bang compiler (a bm file) with the bm emulator (bme).


## Why a Bang compiler in Rust
This project has no practical application, the Bang language itself doesn't have any practical use, it's just a toy project for myself to learn the Rust programming language. Writing a simple compiler seems a fun and educating excersice. 
In addition, it is is nice to have existing tooling and a working compiler written in C and live Tsoding session on Youtube to get additional background information about the language


## How to compile
Just use cargo build

## How to run
Use cargo run filename.bang (e.g. cargo run .\examples\rule110.bang)


## Status
I try to keep up with the latest features, is is possible to compile the the rule 110 (rule110.bang) and the game of life (gol.bang) bang files.

## Todos:
* Parsing fails when last line doesn't contain a newline char
* Implementing latest feature like var/halt 
* remove debug println! statements

## Credits
All credits go to Alexey Kutepov on [Tsoding Daily](https://www.youtube.com/channel/UCrqM0Ym_NbK1fqeQG2VIohg), please visit and subscribe his channel.





