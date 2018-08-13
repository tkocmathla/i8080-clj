(ns space-invaders-clj.core
  (:require [i8080-clj.core :as cpu]
            [i8080-clj.opfns :refer [>> << |]]))

(def machine
  {; i8080 cpu state
   :state nil

   ; interrupt code
   :int-code 0

   ; last, next execution time
   :last-time 0
   :next-time 0

   ; lsb, msb, and offset of external shift hardware
   :sh-lo 0
   :sh-hi 0
   :sh-off 0})

(defn handle-interrupt
  [machine now]
  (let [{:keys [last-time next-time int-code state]} machine
        {:keys [int-enable?]} state
        interrupt? (and int-enable? (> now next-time))]
    (cond-> machine
      (zero? last-time)
      (assoc :last-time now, :next-time (+ now 16000.0), :int-code 1)

      (and interrupt? (= 1 int-code)) 
      (assoc :state (cpu/interrupt state 1), :next-time (+ now 8000.0), :int-code 2)

      (and interrupt? (= 2 int-code))
      (assoc :state (cpu/interrupt state 2), :next-time (+ now 8000.0), :int-code 1))))

(defn handle-in
  [{:keys [sh-lo sh-hi sh-off] :as machine} port]
  (case port
    0 (assoc-in machine [:state :a] 1)
    1 (assoc-in machine [:state :a] 0)
    3 (let [x (| (<< sh-hi 8) sh-lo)
            sh-x (bit-and (>> x (- 8 sh-off)) 0xff)]
        (assoc-in machine [:state :a] sh-x))
    machine))

(defn handle-out
  [{:keys [sh-hi] :as machine} port]
  (let [a (get-in machine [:state :a])]
    (case port
      2 (assoc machine :sh-off (bit-and a 0x7))
      4 (assoc machine :sh-lo sh-hi :sh-hi a)
      machine)))

(defn step-cpu
  ""
  [machine]
  (let [now (System/nanoTime)]
    (loop [{:keys [last-time state] :as m} (handle-interrupt machine now)
           cycles (* 2 (- now last-time))]
      (if (pos? cycles)
        (let [op (cpu/disassemble-op state)]
          (cond
            (= :IN (:op op))
            (-> (handle-in m (first (:args op)))
                (update-in [:state :pc] + 2)
                (recur (- cycles 3)))

            (= :OUT (:op op))
            (-> (handle-out m (first (:args op)))
                (update-in [:state :pc] + 2)
                (recur (- cycles 3)))

            :else
            (-> (assoc m :state (cpu/execute-op state op))
                (recur (- cycles (:cycles op))))))
        (assoc machine :last-time now)))))

(defn run []
  (loop [m machine]
    (Thread/sleep 1)
    (recur (step-cpu m))))
