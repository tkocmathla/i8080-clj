(ns i8080-clj.core
  (:require
    [i8080-clj.ops :refer [ops]]
    [i8080-clj.opfns :refer [push-pc]]))

(defonce mem-64k (vec (repeat 0x10000 0)))

(defrecord FlagState [z s p cy ac]
  Object
  (toString [o]
    (doseq [[k v] o]
      (println (format "  %s 0x%02x" k v)))))

(defrecord CPUState [a b c d e h l sp pc mem int-enable? ^FlagState cc]
  Object
  (toString [o]
    (doseq [k [:a :b :c :d :e :h :l]]
      (println (format "%s 0x%02x" k (get o k))))
    (doseq [k [:pc :sp]]
      (println (format "%s 0x%04x" k (get o k))))
    (doseq [k [:int-enable? :nopc?]]
      (println (format "%s %s" k (get o k))))
    (println ":cc")
    (println (str (get o :cc)))))

(def initial-state
  (map->CPUState
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
     :mem mem-64k ; memory

     :int-enable? false ; enable interrupt
     :nopc? false       ; don't advance pc

     ; condition codes
     :cc
     (map->FlagState
       {:z 0  ; zero
        :s 0  ; sign
        :p 0  ; parity
        :cy 0 ; carry
        :ac 0 ; aux carry
        })}))

(defn get-byte
  "Gets the ith byte from memory"
  [mem i]
  (bit-and (get mem i) 0xff))

(defn get-args
  [{:keys [mem pc]} size]
  (when (> size 1)
    (mapv (partial get-byte mem) (range (inc pc) (+ pc size)))))

(defn disassemble-op
  [{:keys [mem pc] :as state}]
  (let [opcode (get-byte mem pc)
        {:keys [size] :as op} (ops opcode)]
    (assoc op :args (get-args state size))))

(defn execute-op
  [state {:keys [f size args] :as op}]
  (let [{:keys [nopc?] :as new-state}
        (case size
          1 (f state)
          2 (f state (args 0))
          3 (f state (args 0) (args 1)))]
    (cond-> (assoc new-state :nopc? false)
      (not nopc?) (update :pc + size))))

(defn interrupt
  [state i]
  (assoc (push-pc state) :pc (* 8 i), :int-enable? false))
