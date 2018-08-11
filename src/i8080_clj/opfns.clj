(ns i8080-clj.opfns
  "Helper functions for 8080 instruction set")

(def << bit-shift-left)
(def >> bit-shift-right)
(def | bit-or)

(defn parity
  "Returns 1 if byte b has an even number of 1's, or 0 otherwise"
  [b]
  (let [f (fn [[b sum]] [(>> b 1) (+ sum (bit-and b 1))])
        [[_ sum]] (drop-while (comp pos? first) (iterate f [b 0]))]
    (if (even? sum) 1 0)))

(defn byte-at-hl
  "Returns the byte at the address in the HL register pair"
  [state]
  (let [addr (| (<< (state :h) 8) (state :l))]
    (get-in state [:mem addr])))

(defn byte-to-hl
  "Assigns the value b to the memory address in the HL register pair"
  [state b]
  (let [addr (| (<< (state :h) 8) (state :l))]
    (assoc-in state [:mem addr] b)))

;; Arithmetic group -----------------------------------------------------------

(defn- add*
  "Implements all arithmetic ops.
  
  f - addition or subtraction function
  x - value to add or subtract with register a"
  [f x state & [{:keys [with-carry?]}]]
  (let [ans (cond-> (f (state :a) x)
              with-carry? (f (get-in state [:cc :cy])))]
    (-> state
        (assoc-in [:cc :z] (if (zero? (bit-and ans 0xff)) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :cy] (if (> ans 0xff) 1 0))
        (assoc-in [:cc :p] (parity (bit-and ans 0xff)))
        (assoc :a (bit-and ans 0xff)))))

(defn add [reg state] (add* + (state reg) state))
(defn adc [reg state] (add* + (state reg) state {:with-carry? true}))
(defn sub [reg state] (add* - (state reg) state))
(defn sbb [reg state] (add* - (state reg) state {:with-carry? true}))
(defn adi [state b] (add* + b state))
(defn aci [state b] (add* + b state {:with-carry? true}))
(defn sbi [state b] (add* - b state))
(defn sui [state b] (add* - b state {:with-carry? true}))
(defn add-m [state] (add* + (byte-at-hl state) state))
(defn adc-m [state] (add* + (byte-at-hl state) state {:with-carry? true}))
(defn sub-m [state] (add* - (byte-at-hl state) state))
(defn sbb-m [state] (add* - (byte-at-hl state) state {:with-carry? true}))

(defn dcr
  [reg state]
  (let [ans (dec (state reg))]
    (-> state
        (assoc reg (bit-and ans 0xff))
        (assoc-in [:cc :z] (if (zero? ans) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and ans 0x80)) 1 0))
        (assoc-in [:cc :p] (parity ans))
        (assoc-in [:cc :ac] (if (zero? (bit-and ans 0xf)) 1 0)))))

(defn inx
  "Increments the 16-bit number held in the specified register pair"
  [reg-hi reg-lo state]
  (let [ans (inc (| (<< (state reg-hi) 8) (state reg-lo)))]
    (-> state
        (assoc reg-hi (>> (bit-and ans 0xff00) 8))
        (assoc reg-lo (bit-and ans 0xff)))))

(defn inx-sp
  "Increments the stack pointer"
  [state]
  (update state :sp (comp #(bit-and % 0xffff) inc)))

;; Branch group ---------------------------------------------------------------

(defn jmp
  "Implements all jump ops.
  
  Jumps to the address in the byte pair b2 b1 if (f state) is truthy."
  [f state b1 b2]
  (cond-> state
    (f state)
    (-> (assoc :pc (| (<< b2 8) b1))
        (assoc :nopc? true))))

(defn call
  "Implements all call ops.
  
  Calls subroutine at the address in the byte pair b2 b1 if (f state) is truthy."
  [f state b1 b2]
  (let [next-op (+ (state :pc) 2)]
    (cond-> state
      (f state)
      (-> ; push address of next instruction onto stack (return address)
          (assoc-in [:mem (- (state :sp) 1)] (bit-and (>> next-op 8) 0xff))
          (assoc-in [:mem (- (state :sp) 2)] (bit-and next-op 0xff))
          (update :sp #(- % 2))
          ; jump to target address
          (assoc :pc (| (<< b2 8) b1))
          (assoc :nopc? true)))))

(defn ret
  "Implements all return ops.

  Returns program control to address at stack pointer if (f state) is truthy."
  [f state]
  (let [lo (get-in state [:mem (state :sp)])
        hi (get-in state [:mem (inc (state :sp))])]
    (cond-> state
      (f state)
      (-> (assoc :pc (| (<< hi 8) lo))
          (update :sp #(+ % 2))
          (assoc :nopc? true)))))

;; Logical group --------------------------------------------------------------

(defn- bool*
  "Implements all boolean ops.
  
  f - bitwise boolean function (e.g. bit-and)
  x - value against which to compute boolean function with register a"
  [f x state]
  (let [ans (apply f (state :a) x)]
    (cond-> (assoc state :a ans)
      (not= f bit-not)
      (-> (assoc-in [:cc :z] (if (zero? ans) 1 0))
          (assoc-in [:cc :s] (if (= 0x80 (bit-and ans 0x80)) 1 0))
          (assoc-in [:cc :cy] 0)
          (assoc-in [:cc :p] (parity ans))))))

(defn ana [reg state] (bool* bit-and (state reg) state))
(defn xra [reg state] (bool* bit-xor (state reg) state))
(defn ora [reg state] (bool* bit-or (state reg) state))
(defn cma [reg state] (bool* bit-not (state reg) state))
(defn ani [state b1] (bool* bit-and b1 state))
(defn xri [state b1] (bool* bit-xor b1 state))
(defn ori [state b1] (bool* bit-or b1 state))
(defn ana-m [state] (bool* bit-and (byte-at-hl state) state))
(defn xra-m [state] (bool* bit-xor (byte-at-hl state) state))
(defn ora-m [state] (bool* bit-or (byte-at-hl state) state))

(defn rrc
  "Rotates accumulator register by one right shift"
  [state]
  (let [x (state :a)]
    (-> state
        (assoc :a (| (<< (bit-and x 1) 7) (>> x 1)))
        (assoc-in [:cc :cy] (bit-and x 1)))))

(defn rlc
  "Rotates accumulator register by one left shift"
  [state]
  (let [x (state :a)]
    (-> state
        (assoc :a (| (>> (bit-and x 0x80) 7) (bit-and (<< x 1) 0xff)))
        (assoc-in [:cc :cy] (if (pos? (bit-and x 0x80)) 1 0)))))

(defn ral
  "Rotates accumulator register through the carry by one left shift"
  [state]
  (let [x (state :a)]
    (-> state
        (assoc :a (| (<< x 1) (get-in state [:cc :cy])))
        (assoc-in [:cc :cy] (if (pos? (bit-and x 0x80)) 1 0)))))

(defn rar
  "Rotates accumulator register through the carry by one right shift"
  [state]
  (let [x (state :a)]
    (-> state
        (assoc :a (| (<< (get-in state [:cc :cy]) 7) (>> x 1)))
        (assoc-in [:cc :cy] (bit-and x 1)))))

(defn- cmp*
  [x state]
  (let [x (- (state :a) x)]
    (-> state
        (assoc-in [:cc :z] (if (zero? x) 1 0))
        (assoc-in [:cc :s] (if (pos? (bit-and x 0x80)) 1 0))
        (assoc-in [:cc :cy] (if (< (state :a) x) 1 0))
        (assoc-in [:cc :p] (parity x)))))

(defn cmp [reg state] (cmp* (state reg) state))
(defn cpi [state b] (cmp* b state))
(defn cmp-m [state] (cmp* (byte-at-hl state) state))

;; ----------------------------------------------------------------------------

(defn- mov*
  "Assigns value x to register"
  [reg x state]
  (assoc state reg x))

(defn mov [reg1 reg2 state] (mov* reg2 (state reg1) state))
(defn mvi [reg state b] (mov* reg b state))
(defn mov-from-m [reg state] (mov* reg (byte-at-hl state) state))
(defn mov-to-m [reg state] (byte-to-hl state (state reg)))

(defn lxi
  "Implements all load immediate ops except LXI-SP."
  [hi lo state b1 b2]
  (assoc state hi b2, lo b1))

(defn lxi-sp
  [state lo hi]
  (assoc state :sp (| (<< hi 8) lo)))

(defn ldax
  [hi lo state]
  (let [adr (| (<< (state hi) 8) (state lo))]
    (assoc state :a (get-in state [:mem adr]))))

;; ----------------------------------------------------------------------------
