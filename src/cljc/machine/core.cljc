(ns machine.core
  (:require
    [i8080-clj.core :as cpu]
    [i8080-clj.opfns :refer [>> << | *protect-mem*] :as opfns]
    [machine.rom :as rom]
    [quil.core :as q]))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(def ^:dynamic *debug* false)
(def state (atom {}))

(def initial-machine
  {; i8080 cpu state
   :cpu nil

   ; interrupt code
   :int-code 1

   ; count of cycles executed since last interrupt
   :cycles 0

   ; lsb, msb, and offset of external shift hardware
   :sh-lo 0
   :sh-hi 0
   :sh-off 0})

(defn load-machine []
  (assoc initial-machine :cpu (rom/load-rom cpu/initial-state "invaders.rom")))

(defn handle-in
  [{:keys [sh-lo sh-hi sh-off] :as machine} ^long port]
  (case port
    0 (assoc-in machine [:cpu :a] 1)
    1 (assoc-in machine [:cpu :a] 0)
    3 (let [x (| (<< sh-hi 8) sh-lo)
            sh-x (bit-and (>> x (- 8 sh-off)) 0xff)]
        (when (nil? sh-x)
          (throw (Exception. "handle-in writing nil to :a")))
        (assoc-in machine [:cpu :a] sh-x))
    ; else
    machine))

(defn handle-out
  [{:keys [sh-hi] :as machine} ^long port]
  (let [a (get-in machine [:cpu :a])]
    (case port
      2 (assoc machine :sh-off (bit-and a 0x7))
      4 (assoc machine :sh-lo sh-hi :sh-hi a)
      ; else
      machine)))

(defn handle-interrupt
  [machine]
  (let [{:keys [cycles int-code cpu]} machine
        {:keys [int-enable?]} cpu
        interrupt? (and int-enable? (>= cycles 16667))]
    (when interrupt? (prn "interrupting at" cycles "cycles"))
    (cond-> machine
      (and interrupt? (= 1 int-code))
      (assoc :cpu (cpu/interrupt cpu 1), :cycles 0, :int-code 2)

      (and interrupt? (= 2 int-code))
      (assoc :cpu (cpu/interrupt cpu 2), :cycles 0, :int-code 1))))

(defn step-cpu
  ""
  [machine]
  (let [{:keys [cycles cpu] :as m} (handle-interrupt machine)
        op (cpu/disassemble-op cpu)]
    (when *debug*
      (println (format "%04x" (get-in m [:cpu :pc])) (:op op) (:args op)))
    (cond-> (update m :cycles + (:cycles op))
      (= :IN (:op op))
      (-> (handle-in (first (:args op)))
          (update-in [:cpu :pc] + 2))

      (= :OUT (:op op))
      (-> (handle-out (first (:args op)))
          (update-in [:cpu :pc] + 2))

      :else
      (assoc :cpu (cpu/execute-op cpu op)))))

(defn step-ops
  [machine n]
  (loop [m machine, n n]
    (if (pos? n)
      (let [m (handle-interrupt m (inc (:next-time machine)))
            op (cpu/disassemble-op (:cpu m))]
        (when *debug*
          (println (format "%04x:" (get-in m [:cpu :pc]))
                   (name (:op op))
                   (reverse (map #(format "%02x" %) (:args op)))))
        (cond
          (= :IN (:op op))
          (-> (handle-in m (first (:args op)))
              (update-in [:cpu :pc] + 2)
              (recur (dec n)))

          (= :OUT (:op op))
          (-> (handle-out m (first (:args op)))
              (update-in [:cpu :pc] + 2)
              (recur (dec n)))

          :else
          (-> (assoc m :cpu (cpu/execute-op (:cpu m) op))
              (recur (dec n)))))
      m)))

(comment
  (require '[clojure.data :refer [diff]])
  (require '[clojure.pprint :as pp])

  ; 42420 + 14
  ; 42034
  ; a few steps after OUT
  #_
  (swap! state assoc :machine (load-machine))
  (prn (str (:cpu (:machine @state))))
  #_
  (binding [*debug* true]
    (let [old-cpu (assoc (get-in @state [:machine :cpu]) :mem nil)
          new-m (step-ops (:machine @state) 1000)
          new-cpu (assoc (:cpu new-m) :mem nil)
          [o n _] (diff old-cpu new-cpu)]
      (swap! state assoc :machine new-m)
      (println)
      (println "diff:")
      (println "-" o)
      (println "+" n)
      (println)
      (str new-cpu))))

(defn run-cpu []
  (swap! state assoc :machine (step-cpu (@state :machine)))
  (recur))

;; WIP Drawing ----------------------------------------------------------------

(defn setup []
  (q/frame-rate 60)
  (reset!
    state
    {:image (q/create-image 256 224 :rgb)
     :machine (load-machine)})
  (future (run-cpu)))

(defn draw []
  ; rotate image 90 degrees counter-clockwise about the bottom-left corner
  (q/translate 0 (q/height))
  (q/rotate (q/radians -90))
  (q/image (@state :image) 0 0)
  (let [pxs (q/pixels (@state :image))]
    (doseq [xy (range 7168) ; 7168 = 224 * 256 / 8
            :let [b (get-in @state [:machine :cpu :mem (+ 0x2400 xy)])]]
      (loop [b b, i 0]
        (when (< i 8)
          (aset-int pxs (+ (* xy 8) i) (q/color ({0 0, 1 255} (bit-and b 1))))
          (recur (>> b 1) (inc i))))))
  (q/update-pixels (@state :image)))

#_
(q/defsketch space-invaders
  :size [224 256]
  :setup setup
  :draw draw
  :features [:no-bind-output])
