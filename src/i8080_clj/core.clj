(ns i8080-clj.core
  (:require
    [clojure.java.io :as io]
    [i8080-clj.ops :refer [ops]]
    [i8080-clj.opfns :refer :all]))

(defonce mem-16k (vec (repeat 0x10000 0)))

(def initial-state
  {; registers
   :a 0         ; accumulator
   :b 0         ;
   :c 0         ;
   :d 0         ;
   :e 0         ;
   :h 0         ; hi address byte
   :l 0         ; lo address byte

   :sp 0        ; stack pointer
   :pc 0        ; program counter
   :mem mem-16k ; memory

   :int-enable? false ; enable interrupt

   ; condition codes
   :cc {:z 0  ; zero
        :s 0  ; sign
        :p 0  ; parity
        :cy 0 ; carry
        :ac 0 ; aux carry
        }})

(defn get-byte
  "Gets the ith byte from memory"
  [mem i]
  (bit-and (get mem i) 0xff))

(defn get-args
  [{:keys [mem pc]} size]
  (when (> size 1)
    (map (partial get-byte mem) (range (inc pc) (+ pc size)))))

(defn disassemble-op
  [{:keys [mem pc] :as state}]
  (let [opcode (get-byte mem pc)
        {:keys [size] :as op} (ops opcode)]
    (assoc op :args (get-args state size))))

(defn execute-op
  [state {:keys [f size args] :as op}]
  #_(prn (:op op) args)
  (let [{:keys [nopc?] :as new-state} (apply f state args)]
    (cond-> (dissoc new-state :nopc?)
      (not nopc?) (update :pc + size))))

(defn interrupt
  [state i]
  (assoc (push-pc state) :pc (* 8 i), :int-enable? true))
