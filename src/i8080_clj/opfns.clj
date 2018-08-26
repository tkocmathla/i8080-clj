(ns i8080-clj.opfns
  "Helper functions for 8080 instruction set"
  (:require [taoensso.tufte :refer [defnp]]))

(def << bit-shift-left)
(def >> bit-shift-right)
(def | bit-or)

(defnp parity
  "Returns 1 if byte b has an even number of 1's, or 0 otherwise"
  [^long b]
  (let [n (Long/bitCount b)]
    (if (even? n) 1 0)))

(defnp byte-at-hl
  "Returns the byte at the address in the HL register pair"
  [state]
  (let [addr (| (<< (:h state) 8) (:l state))]
    (get-in state [:mem addr])))

(defnp byte-to-hl
  "Assigns the value b to the memory address in the HL register pair"
  [state b]
  (let [addr (| (<< (:h state) 8) (:l state))]
    (assoc-in state [:mem addr] b)))

;; Arithmetic group -----------------------------------------------------------

(defn- add*
  "Implements all basic arithmetic ops.
  
  f - addition or subtraction function
  x - value to add or subtract with register a"
  [f x state & [{:keys [with-carry?]}]]
  (let [ans (cond-> (f (:a state) x)
              with-carry? (f (get-in state [:cc :cy])))]
    (-> state
        (assoc-in [:cc :z] (if (zero? (bit-and ans 0xff)) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :cy] (if (> ans 0xff) 1 0))
        (assoc-in [:cc :p] (parity (bit-and ans 0xff)))
        (assoc :a (bit-and ans 0xff)))))

(defnp add [reg state] (add* + (reg state) state))
(defnp adc [reg state] (add* + (reg state) state {:with-carry? true}))
(defnp sub [reg state] (add* - (reg state) state))
(defnp sbb [reg state] (add* - (reg state) state {:with-carry? true}))
(defnp adi [state b] (add* + b state))
(defnp aci [state b] (add* + b state {:with-carry? true}))
(defnp sbi [state b] (add* - b state))
(defnp sui [state b] (add* - b state {:with-carry? true}))
(defnp add-m [state] (add* + (byte-at-hl state) state))
(defnp adc-m [state] (add* + (byte-at-hl state) state {:with-carry? true}))
(defnp sub-m [state] (add* - (byte-at-hl state) state))
(defnp sbb-m [state] (add* - (byte-at-hl state) state {:with-carry? true}))

(defn- dad*
  "Implements all double-add instructions.
  
  x - value to add with register pair hl"
  [x state]
  (let [ans (+ x (| (<< (:h state) 8) (:l state)))]
    (-> state
        (assoc :h (>> (bit-and ans 0xff00) 8))
        (assoc :l (bit-and ans 0xff))
        (assoc-in [:cc :cy] (if (pos? (bit-and ans 0xffff0000)) 1 0)))))

(defnp dad-b [{:keys [b c] :as state}] (dad* (| (<< b 8) c) state))
(defnp dad-d [{:keys [d e] :as state}] (dad* (| (<< d 8) e) state))
(defnp dad-h [{:keys [h l] :as state}] (dad* (| (<< h 8) l) state))
(defnp dad-sp [{:keys [sp] :as state}] (dad* sp state))

(defnp dcr
  "Decrements value in register"
  [reg state]
  (let [ans (dec (reg state))]
    (-> state
        (assoc reg (bit-and ans 0xff))
        (assoc-in [:cc :z] (if (zero? ans) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :p] (parity ans))
        (assoc-in [:cc :ac] (if (zero? (bit-and ans 0xf)) 1 0)))))

(defnp dcr-m
  "Decrements value in memory"
  [state]
  (let [new-state (byte-to-hl state (dec (byte-at-hl state)))
        ans (byte-at-hl new-state)]
    (-> new-state
        (assoc-in [:cc :z] (if (zero? ans) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :p] (parity ans))
        (assoc-in [:cc :ac] (if (zero? (bit-and ans 0xf)) 1 0)))))

(defnp inx
  "Increments the 16-bit number held in the specified register pair"
  [reg-hi reg-lo state]
  (let [ans (inc (| (<< (reg-hi state) 8) (reg-lo state)))]
    (-> state
        (assoc reg-hi (>> (bit-and ans 0xff00) 8))
        (assoc reg-lo (bit-and ans 0xff)))))

(defnp inx-sp
  "Increments the stack pointer"
  [state]
  (update state :sp (comp #(bit-and % 0xffff) inc)))

;; Branch group ---------------------------------------------------------------

(defnp jmp
  "Implements all jump ops.
  
  Jumps to the address in the byte pair hi lo if (f state) is truthy."
  [f state lo hi]
  (cond-> state
    (f state)
    (-> (assoc :pc (| (<< hi 8) lo))
        (assoc :nopc? true))))

(defnp call
  "Implements all call ops.
  
  Calls subroutine at the address in the byte pair hi lo if (f state) is truthy."
  [f state lo hi]
  (let [next-op (+ (:pc state) 3)]
    (cond-> state
      (f state)
      (-> ; push address of next instruction onto stack (return address)
          (assoc-in [:mem (- (:sp state) 1)] (bit-and (>> next-op 8) 0xff))
          (assoc-in [:mem (- (:sp state) 2)] (bit-and next-op 0xff))
          (update :sp - 2)
          ; jump to target address
          (assoc :pc (| (<< hi 8) lo))
          (assoc :nopc? true)))))

(defnp ret
  "Implements all return ops.

  Returns program control to address at stack pointer if (f state) is truthy."
  [f state]
  (let [lo (get-in state [:mem (:sp state)])
        hi (get-in state [:mem (inc (:sp state))])]
    (cond-> state
      (f state)
      (-> (assoc :pc (| (<< hi 8) lo))
          (update :sp + 2)
          (assoc :nopc? true)))))

;; Logical group --------------------------------------------------------------

(defn- bool*
  "Implements all boolean ops except cma.
  
  f - bitwise boolean function (e.g. bit-and)
  x - value against which to compute boolean function with register a"
  [f x state]
  (let [ans (f (:a state) x)]
    (-> (assoc state :a ans)
        (assoc-in [:cc :z] (if (zero? ans) 1 0))
        (assoc-in [:cc :s] (if (= 0x80 (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :cy] 0)
        (assoc-in [:cc :ac] 0)
        (assoc-in [:cc :p] (parity ans)))))

(defnp ana [reg state] (bool* bit-and (reg state) state))
(defnp xra [reg state] (bool* bit-xor (reg state) state))
(defnp ora [reg state] (bool* bit-or (reg state) state))
(defnp cma [state] (update state :a bit-not))
(defnp ani [state b1] (bool* bit-and b1 state))
(defnp xri [state b1] (bool* bit-xor b1 state))
(defnp ori [state b1] (bool* bit-or b1 state))
(defnp ana-m [state] (bool* bit-and (byte-at-hl state) state))
(defnp xra-m [state] (bool* bit-xor (byte-at-hl state) state))
(defnp ora-m [state] (bool* bit-or (byte-at-hl state) state))

(defnp rrc
  "Rotates accumulator register by one right shift"
  [state]
  (let [x (:a state)]
    (-> state
        (assoc :a (| (<< (bit-and x 1) 7) (>> x 1)))
        (assoc-in [:cc :cy] (bit-and x 1)))))

(defnp rlc
  "Rotates accumulator register by one left shift"
  [state]
  (let [x (:a state)]
    (-> state
        (assoc :a (| (>> (bit-and x 0x80) 7) (bit-and (<< x 1) 0xff)))
        (assoc-in [:cc :cy] (if (pos? (bit-and x 0x80)) 1 0)))))

(defnp ral
  "Rotates accumulator register through the carry by one left shift"
  [state]
  (let [x (:a state)]
    (-> state
        (assoc :a (| (<< x 1) (get-in state [:cc :cy])))
        (assoc-in [:cc :cy] (if (pos? (bit-and x 0x80)) 1 0)))))

(defnp rar
  "Rotates accumulator register through the carry by one right shift"
  [state]
  (let [x (:a state)]
    (-> state
        (assoc :a (| (<< (get-in state [:cc :cy]) 7) (>> x 1)))
        (assoc-in [:cc :cy] (bit-and x 1)))))

(defn- cmp*
  [x state]
  (let [x (- (:a state) x)]
    (-> state
        (assoc-in [:cc :z] (if (zero? x) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and x 0x80)) 1 0))
        (assoc-in [:cc :cy] (if (< (:a state) x) 1 0))
        (assoc-in [:cc :p] (parity x)))))

(defnp cmp [reg state] (cmp* (reg state) state))
(defnp cpi [state b] (cmp* b state))
(defnp cmp-m [state] (cmp* (byte-at-hl state) state))

;; ----------------------------------------------------------------------------

(defn- mov*
  "Assigns value x to register"
  [reg x state]
  (assoc state reg x))

(defnp mov [reg1 reg2 state] (mov* reg2 (reg1 state) state))
(defnp mvi [reg state b] (mov* reg b state))
(defnp mov-from-m [reg state] (mov* reg (byte-at-hl state) state))
(defnp mov-to-m [reg state] (byte-to-hl state (reg state)))

(defnp xchg
  [{:keys [d e h l] :as state}]
  (assoc state :d h, :e l, :h d, :l e))

(defnp lxi
  "Implements all load immediate ops except LXI-SP."
  [hi lo state b1 b2]
  (assoc state hi b2, lo b1))

(defnp lxi-sp
  [state lo hi]
  (assoc state :sp (| (<< hi 8) lo)))

(defnp lda
  [state lo hi]
  (let [adr (| (<< hi 8) lo)]
    (assoc state :a (get-in state [:mem adr]))))

(defnp ldax
  [hi lo state]
  (let [adr (| (<< (hi state) 8) (lo state))]
    (assoc state :a (get-in state [:mem adr]))))

(defnp sta
  [state lo hi]
  (let [adr (| (<< hi 8) lo)]
    (assoc-in state [:mem adr] (:a state))))

;; ----------------------------------------------------------------------------

(defn- push*
  [hi lo state]
  (-> state
      (assoc-in [:mem (- (:sp state) 1)] hi)
      (assoc-in [:mem (- (:sp state) 2)] lo)
      (update :sp - 2)))

(defnp push-b [{:keys [b c] :as state}] (push* b c state))
(defnp push-d [{:keys [d e] :as state}] (push* d e state))
(defnp push-h [{:keys [h l] :as state}] (push* h l state))
(defnp push-pc [{:keys [pc] :as state}] (push* (>> (bit-and pc 0xff00) 8) (bit-and pc 0xff) state))

(defnp push-psw
  [state]
  (let [{:keys [z s p cy ac]} (:cc state)
        psw (| z (<< s 1) (<< p 2) (<< cy 3) (<< ac 4))]
    (-> state
        (assoc-in [:mem (- (:sp state) 1)] (:a state))
        (assoc-in [:mem (- (:sp state) 2)] psw)
        (update :sp - 2))))

(defn- pop*
  [reg-hi reg-lo state]
  (-> state
      (assoc reg-lo (get-in state [:mem (:sp state)]))
      (assoc reg-hi (get-in state [:mem (inc (:sp state))]))
      (update :sp + 2)))

(defnp pop-b [state] (pop* :b :c state))
(defnp pop-d [state] (pop* :d :e state))
(defnp pop-h [state] (pop* :h :l state))

(defnp pop-psw
  [state]
  (let [psw (get-in state [:mem (:sp state)])]
    (-> state
        (assoc :a (get-in state [:mem (inc (:sp state))]))
        (assoc-in [:cc :z] (if (pos? (bit-and psw 0x01)) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and psw 0x02)) 1 0))
        (assoc-in [:cc :p] (if (pos? (bit-and psw 0x04)) 1 0))
        (assoc-in [:cc :cy] (if (pos? (bit-and psw 0x08)) 1 0))
        (assoc-in [:cc :ac] (if (pos? (bit-and psw 0x10)) 1 0))
        (update :sp + 2))))
