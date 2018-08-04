(ns i8080-clj.core)

(def initial-state
  {; registers
   :a 0     ; accumulator
   :b 0     ;
   :c 0     ;
   :d 0     ;
   :e 0     ;
   :h 0     ; hi address byte
   :l 0     ; lo address byte

   :sp 0    ; stack pointer
   :pc 0    ; program counter
   :mem nil ; memory

   ; condition codes
   :cc {:z 0  ; zero
        :s 0  ; sign
        :p 0  ; parity
        :cy 0 ; carry
        :ac 0 ; aux carry
        }})
