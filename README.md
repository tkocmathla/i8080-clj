# i8080-clj

i8080-clj is a working [Intel 8080](https://en.wikipedia.org/wiki/Intel_8080) emulator in Clojure. Implementing a CPU emulator in Clojure is heroically inefficient, but is undeniably educational and even fun (for varying definitions of fun).

This project owes a lot to [Emulator 101](http://www.emulator101.com) for inspiration and guidance. 

## Getting started

Since i8080-clj is a CPU emulator, it doesn't do much on its own. Like any processor, it needs a binary executable to do something interesting. 

To really dive in and learn about the instruction set, check out the [i8080 Programmer's Manual](resources/8080%20Programmers%20Manual.pdf) in this repository. 

Otherwise, try running [Space Invaders](https://github.com/tkocmathla/space-invaders-clj)!

## Running the tests

Most instructions have an associated unit test. To run them all, just do:

    lein test

## License

Copyright © 2018-2020 Matt Grimm

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## Acknowledgements

* [Emulator 101](http://www.emulator101.com)
