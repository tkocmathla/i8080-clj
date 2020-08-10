# i8080-clj

i8080-clj is a working [Intel 8080](https://en.wikipedia.org/wiki/Intel_8080) emulator in Clojure. Implementing a CPU emulator in Clojure is heroically inefficient, but is undeniably educational and even fun (for varying definitions of fun).

Since i8080-clj is a CPU emulator, it doesn't do much on its own. Like any processor, it needs a machine and an executable to do something interesting. 

To really dive in and learn about the instruction set, check out the [i8080 Programmer's Manual](resources/8080%20Programmers%20Manual.pdf) in this repository. 

Otherwise, try running :space_invader: [Space Invaders](https://github.com/tkocmathla/space-invaders-clj)!

## Getting started

The i8080 is a relatively simple processor to emulate, since it's 8 bit and only has 256 instructions. We can store the entire machine state in a simple map, which makes debugging easy:

``` clojure
{; registers
 :cpu/a 0         ; accumulator
 :cpu/b 0         ;
 :cpu/c 0         ;
 :cpu/d 0         ;
 :cpu/e 0         ;
 :cpu/h 0         ; hi address byte
 :cpu/l 0         ; lo address byte

 :cpu/sp 0        ; stack pointer
 :cpu/pc 0        ; program counter
 :cpu/mem mem-64k ; memory

 :cpu/int-enable? false ; enable interrupt
 :cpu/nopc? false       ; don't advance pc

 ; condition codes
 :cpu/cc/z 0  ; zero
 :cpu/cc/s 0  ; sign
 :cpu/cc/p 0  ; parity
 :cpu/cc/cy 0 ; carry
 :cpu/cc/ac 0 ; aux carry

 :cpu/last-mem nil ; addr and data of write from last op (or nil)
 }
```

This means executing an instruction is as simple as calling a function that takes a map of the current state as input and returns the next state as output.

## Installation

To install, add the following to your project `:dependencies`:

    [i8080-clj "0.4.0"]

## Running the tests

Most instructions have an associated unit test. To run them all, just do:

    lein test

## License

Copyright © 2018-2020 Matt Grimm

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## Acknowledgements

* This project owes a lot to [Emulator 101](http://www.emulator101.com) for inspiration and guidance. 
