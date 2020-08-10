(ns i8080-clj.api
  (:require
    [i8080-clj.opfns :refer [get-byte]]
    [i8080-clj.ops :refer [ops]]))

(defn get-args
  [{:keys [cpu/pc] :as state} size]
  (condp = size
    1 nil
    2 [(get-byte state (inc pc))]
    3 [(get-byte state (inc pc)) (get-byte state (+ pc 2))]))

(defn disassemble-op
  [{:keys [cpu/pc] :as state}]
  (let [opcode (get-byte state pc)
        {:keys [size] :as op} (ops opcode)]
    (assoc op :args (get-args state size))))

(defn execute-op
  [state {:keys [f ^long size args] :as op}]
  (let [{:keys [cpu/nopc?] :as new-state}
        (condp = size
          1 (f (assoc state :cpu/last-mem nil))
          2 (f (assoc state :cpu/last-mem nil) (args 0))
          3 (f (assoc state :cpu/last-mem nil) (args 0) (args 1)))]
    (cond-> (assoc new-state :cpu/nopc? false)
      (not nopc?) (update :cpu/pc + size))))
