(ns space-invaders-clj.core
  (:require [i8080-clj.core :as cpu]
            [i8080-clj.opfns :refer [>> << |]]
            [space-invaders-clj.rom :as rom]
            [quil.core :as q]
            [quil.middleware :as qm]
            [taoensso.tufte :as tufte :refer [defnp p profile]]))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(def refresh-rate-in-usec (* 1/60 1e6))
(def half-refresh-rate-in-usec (/ refresh-rate-in-usec 2))
(def now-usec #(/ (System/nanoTime) 1e3))

(def initial-machine
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

(defonce state (atom {}))

(defnp handle-in
  [{:keys [sh-lo sh-hi sh-off] :as machine} ^long port]
  (case port
    0 (assoc-in machine [:state :a] 1)
    1 (assoc-in machine [:state :a] 0)
    3 (let [x (| (<< sh-hi 8) sh-lo)
            sh-x (bit-and (>> x (- 8 sh-off)) 0xff)]
        (assoc-in machine [:state :a] sh-x))
    ; else
    machine))

(defnp handle-out
  [{:keys [sh-hi] :as machine} ^long port]
  (let [a (get-in machine [:state :a])]
    (case port
      2 (assoc machine :sh-off (bit-and a 0x7))
      4 (assoc machine :sh-lo sh-hi :sh-hi a)
      ; else
      machine)))

(defn init-interrupts
  [{:keys [last-time] :as machine} now]
  (cond-> machine
    (zero? last-time)
    (assoc :last-time now, :next-time (+ now refresh-rate-in-usec), :int-code 1)))

(defnp handle-interrupt
  "The game's video hardware generates 2 interrupts which we will have to
   emulate in software, a end of frame and a middle of frame. Both execute at
   60 Hz (60 times a second)."
  [machine now]
  (let [{:keys [last-time next-time int-code state] :as next-machine} (init-interrupts machine now)
        {:keys [int-enable?]} state
        interrupt? (and int-enable? (> now next-time))]
    (cond
      (and interrupt? (= 1 int-code)) 
      (assoc next-machine :state (cpu/interrupt state 1), :next-time (+ now half-refresh-rate-in-usec), :int-code 2)

      (and interrupt? (= 2 int-code))
      (assoc next-machine :state (cpu/interrupt state 2), :next-time (+ now half-refresh-rate-in-usec), :int-code 1)
      
      :else next-machine)))

(defn step-cpu
  ""
  [machine]
  (let [now (now-usec)]
    (loop [{:keys [last-time state] :as m} (handle-interrupt machine now)
           cycles (min (* 2 (- now last-time)) 1e5)]
      (if (pos? cycles)
        (let [op (cpu/disassemble-op state)]
          ;(println (format "%04x" (get-in m [:state :pc])) (:op op) (:args op))
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
        (assoc m :last-time now)))))

(defn run []
  (println (now-usec) "stepping cpu")
  (reset! state (step-cpu @state))
  (recur))

;; WIP Drawing ----------------------------------------------------------------

(defn setup-state []
  (q/frame-rate 60)
  (reset! state (assoc initial-machine :state (rom/load-rom cpu/initial-state "invaders.rom")))
  (future (run)))

(defn draw-state []
  (let [pxs (q/pixels)]
    (doseq [xy (range 7168) ; 7168 = 224 * 256 / 8
            :let [b (get-in @state [:state :mem (+ 0x2400 xy)])]]
      (loop [b b, i 0]
        (when (< i 8)
          (aset-int pxs (+ (* xy 8) i) (q/color ({0 0, 1 255} (bit-and b 1))))
          (recur (>> b 1) (inc i))))))
  (q/update-pixels))

(q/defsketch space-invaders
  :renderer :p2d
  ;       w x h
  :size [256 224] ; pre-rotated dimensions
  :setup setup-state
  :draw draw-state
  :features [:no-bind-output])
