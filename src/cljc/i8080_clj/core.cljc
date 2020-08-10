(ns i8080-clj.core)

(defn cpu []
  (let [mem-64k (int-array 0x10000 0)]
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
     }))
