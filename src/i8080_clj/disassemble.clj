(ns i8080-clj.disassemble
  (:require
    [clojure.java.io :as io]
    [i8080-clj.core :refer [initial-state]]
    [i8080-clj.ops :refer [ops advance-pc?]]))

(defn get-byte
  "Gets the ith byte from memory"
  [mem i]
  (bit-and (get mem i) 0xff))

(defn get-args
  [{:keys [mem pc]} size]
  (when size
    (map (partial get-byte mem) (range (inc pc) (+ pc size)))))

(defn disassemble-op
  [{:keys [mem pc] :as state}]
  (let [opcode (get-byte mem pc)
        {:keys [size] :as op} (ops opcode)]
    (assoc op :args (get-args state size))))

(defn execute-op
  [state {:keys [f size args] :as op}]
  (let [{:keys [nopc?] :as new-state} (apply f state args)]
    (cond-> (dissoc new-state nopc?)
      (not nopc?) (update :pc + size))))
