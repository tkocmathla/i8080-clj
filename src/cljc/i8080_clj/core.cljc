(ns i8080-clj.core
  (:require
    [i8080-clj.ops :refer [ops]]
    [i8080-clj.opfns :refer [push-pc]]))

(defonce mem-64k (vec (repeat 0x10000 0)))

(def cpu
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
   })

(defn get-byte
  "Gets the ith byte from memory"
  [mem i]
  (bit-and (get mem i) 0xff))

(defn get-args
  [{:keys [cpu/mem cpu/pc]} size]
  (when (> size 1)
    (mapv (partial get-byte mem) (range (inc pc) (+ pc size)))))

(defn disassemble-op
  [{:keys [cpu/mem cpu/pc] :as state}]
  (let [opcode (get-byte mem pc)
        {:keys [size] :as op} (ops opcode)]
    (assoc op :args (get-args state size))))

(defn execute-op
  [state {:keys [f size args] :as op}]
  (let [{:keys [cpu/nopc?] :as new-state}
        (case size
          1 (f state)
          2 (f state (args 0))
          3 (f state (args 0) (args 1)))]
    (cond-> (assoc new-state :cpu/nopc? false)
      (not nopc?) (update :cpu/pc + size))))

(defn interrupt
  [state i]
  (assoc (push-pc state) :cpu/pc (* 8 i), :cpu/int-enable? false))
