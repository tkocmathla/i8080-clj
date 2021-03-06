(ns i8080-clj.opfns
  "Helper functions for 8080 instruction set")

(def ^:dynamic *protect-mem*
  "When true, throws an exception on illegal memory accesses"
  true)

(def ^:dynamic *log-mem*
  "When true, records the last memory write to :cpu/last-mem"
  false)

(def << bit-shift-left)
(def >> bit-shift-right)
(def | bit-or)

(defn get-byte
  "Gets the ith byte from memory"
  [state ^long i]
  (let [{:keys [^ints cpu/mem]} state]
    (aget mem i)))

(defn write-byte
  "Writes b to memory at addr"
  [state ^long addr ^long b]
  (let [{:keys [^ints cpu/mem]} state
        addr (bit-and addr 0xffff)]
    (when *protect-mem*
      (cond
        (nil? b) (throw #?(:clj (Exception. "Can't write nil to memory!")
                           :cljs (js/Error "Can't write nil to memory!")))
        (< addr 0x2000) (throw #?(:clj (Exception. "Can't write to ROM!")
                                 :cljs (js/Error "Can't write to ROM!")))
        (>= addr 0x4000) (throw #?(:clj (Exception. "Can't write to game RAM!")
                                  :cljs (js/Error "Can't write to game RAM!")))))
    (aset-int mem addr (bit-and b 0xff))
    (cond-> state
      *log-mem* (assoc :cpu/last-mem [addr b]))))

(defn write-bytes
  "Writes values in xs to sequential locations in memory starting at addr"
  [state ^long addr xs]
  (reduce
    (fn [m [i x]] (write-byte m (+ addr i) x))
    state
    (map-indexed vector xs)))

(defn parity
  "Returns 1 if byte b has an even number of 1's, or 0 otherwise"
  [^long b]
  (if (zero? (mod (Long/bitCount b) 2)) 1 0))

(defn byte-at-hl
  "Returns the byte at the address in the HL register pair"
  [state]
  (let [addr (bit-and (| (<< (:cpu/h state) 8) (:cpu/l state)) 0xffff)]
    (get-byte state addr)))

(defn byte-to-hl
  "Assigns the value b to the memory address in the HL register pair"
  [state b]
  (let [addr (| (<< (:cpu/h state) 8) (:cpu/l state))]
    (write-byte state addr b)))

;; Arithmetic group -----------------------------------------------------------

(defn- add*
  "Implements all basic arithmetic ops.

  f - addition or subtraction function
  x - value to add or subtract with register a"
  [f x state & [{:keys [with-carry?]}]]
  (let [ans (cond-> (f (:cpu/a state) x)
              with-carry? (f (:cpu/cc/cy state)))]
    (assoc state
           :cpu/cc/z (if (zero? (bit-and ans 0xff)) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/cy (if (> ans 0xff) 1 0)
           :cpu/cc/p (parity ans)
           :cpu/a (bit-and ans 0xff))))

(defn add [reg state] (add* + (reg state) state))
(defn adc [reg state] (add* + (reg state) state {:with-carry? true}))
(defn sub [reg state] (add* - (reg state) state))
(defn sbb [reg state] (add* - (reg state) state {:with-carry? true}))
(defn adi [state b] (add* + b state))
(defn aci [state b] (add* + b state {:with-carry? true}))
(defn sbi [state b] (add* - b state))
(defn sui [state b] (add* - b state {:with-carry? true}))
(defn add-m [state] (add* + (byte-at-hl state) state))
(defn adc-m [state] (add* + (byte-at-hl state) state {:with-carry? true}))
(defn sub-m [state] (add* - (byte-at-hl state) state))
(defn sbb-m [state] (add* - (byte-at-hl state) state {:with-carry? true}))

(defn- dad*
  "Implements all double-add instructions.

  x - value to add with register pair hl"
  [x state]
  (let [ans (+ x (| (<< (:cpu/h state) 8) (:cpu/l state)))]
    (assoc state
           :cpu/h (bit-and (>> ans 8) 0xff)
           :cpu/l (bit-and ans 0xff)
           :cpu/cc/cy (if (pos? (bit-and ans 0xffff0000)) 1 0))))

(defn dad-b [{:keys [cpu/b cpu/c] :as state}] (dad* (| (<< b 8) c) state))
(defn dad-d [{:keys [cpu/d cpu/e] :as state}] (dad* (| (<< d 8) e) state))
(defn dad-h [{:keys [cpu/h cpu/l] :as state}] (dad* (| (<< h 8) l) state))
(defn dad-sp [{:keys [cpu/sp] :as state}] (dad* sp state))

(defn inr
  "Increments value in register"
  [reg state]
  (let [ans (inc (reg state))]
    (assoc state
           reg (bit-and ans 0xff)
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/p (parity ans)
           :cpu/cc/ac (if (zero? (bit-and ans 0xf)) 1 0))))

(defn inr-m
  "Increments value in memory"
  [state]
  (let [new-state (byte-to-hl state (inc (byte-at-hl state)))
        ans (byte-at-hl new-state)]
    (assoc new-state
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/p (parity ans)
           :cpu/cc/ac (if (zero? (bit-and ans 0xf)) 1 0))))

(defn dcr
  "Decrements value in register"
  [reg state]
  (let [ans (dec (reg state))]
    (assoc state
           reg (bit-and ans 0xff)
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/p (parity ans)
           :cpu/cc/ac (if (zero? (bit-and ans 0xf)) 1 0))))

(defn dcr-m
  "Decrements value in memory"
  [state]
  (let [new-state (byte-to-hl state (dec (byte-at-hl state)))
        ans (byte-at-hl new-state)]
    (assoc new-state
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/p (parity ans)
           :cpu/cc/ac (if (zero? (bit-and ans 0xf)) 1 0))))

(defn inx
  "Increments the 16-bit number held in the specified register pair"
  [reg-hi reg-lo state]
  (let [ans (inc (| (<< (reg-hi state) 8) (reg-lo state)))]
    (assoc state
           reg-hi (bit-and (>> ans 8) 0xff)
           reg-lo (bit-and ans 0xff))))

(defn inx-sp
  "Increments the stack pointer"
  [state]
  (update state :cpu/sp (comp #(bit-and % 0xffff) inc)))

(defn dcx
  "Decrements the 16-bit number held in the specified register pair"
  [reg-hi reg-lo state]
  (let [ans (dec (| (<< (reg-hi state) 8) (reg-lo state)))]
    (assoc state
           reg-hi (bit-and (>> ans 8) 0xff)
           reg-lo (bit-and ans 0xff))))

(defn dcx-sp
  "Decrements the stack pointer"
  [state]
  (update state :cpu/sp (comp #(bit-and % 0xffff) dec)))

(defn daa
  "Decimal adjust accumulator"
  [state]
  (let [new-st (cond-> state (> (bit-and (:cpu/a state) 0xf) 9) (update :cpu/a + 6))]
    (if (> (bit-and (:cpu/a new-st) 0xf0) 0x90)
      (let [ans (+ (:cpu/a new-st) 0x60)]
        (assoc new-st :cpu/a (bit-and ans 0xff)
               :cpu/cc/z (if (zero? ans) 1 0)
               :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
               :cpu/cc/p (parity ans)
               :cpu/cc/cy (if (> ans 0xff) 1 0)
               :cpu/cc/ac (if (zero? (bit-and ans 0xf)) 1 0)))
      state)))

;; Branch group ---------------------------------------------------------------

(defn jmp
  "Implements all jump ops.

  Jumps to the address in the byte pair hi lo if (f state) is truthy."
  [f state lo hi]
  (cond-> state
    (f state)
    (assoc :cpu/pc (bit-and (| (<< hi 8) lo) 0xffff)
           :cpu/nopc? true)))

(defn call
  "Implements all call ops.

  Calls subroutine at the address in the byte pair hi lo if (f state) is truthy."
  [f state lo hi]
  (let [next-op (+ (:cpu/pc state) 3)]
    (cond-> state
      (f state)
      (-> ; push address of next instruction onto stack (return address)
          (write-byte (- (:cpu/sp state) 1) (>> next-op 8))
          (write-byte (- (:cpu/sp state) 2) next-op)
          (update :cpu/sp - 2)
          ; jump to target address
          (assoc :cpu/pc (bit-and (| (<< hi 8) lo) 0xffff)
                 :cpu/nopc? true)))))

(defn ret
  "Implements all return ops.

  Returns program control to address at stack pointer if (f state) is truthy."
  [f state]
  (let [lo (get-byte state (:cpu/sp state))
        hi (get-byte state (inc (:cpu/sp state)))]
    (cond-> state
      (f state)
      (-> (assoc :cpu/pc (bit-and (| (<< hi 8) lo) 0xffff)
                 :cpu/nopc? true)
          (update :cpu/sp + 2)))))

(defn pchl
  "Loads the program counter with the address contained in the HL register pair"
  [{:keys [cpu/h cpu/l] :as state}]
  (assoc state
         :cpu/pc (bit-and (| (<< h 8) l) 0xffff)
         :cpu/nopc? true))

;; Logical group --------------------------------------------------------------

(defn- bool*
  "Implements all boolean ops except cma.

  f - bitwise boolean function (e.g. bit-and)
  x - value against which to compute boolean function with register a"
  [f x state]
  (let [ans (bit-and (f (:cpu/a state) x) 0xff)]
    (assoc state
           :cpu/a ans
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/cy 0
           :cpu/cc/ac 0
           :cpu/cc/p (parity ans))))

(defn ana [reg state] (bool* bit-and (reg state) state))
(defn xra [reg state] (bool* bit-xor (reg state) state))
(defn ora [reg state] (bool* bit-or (reg state) state))
(defn cma [state] (update state :cpu/a (fn [a] (bit-and (bit-not a) 0xff))))
(defn ani [state b1] (bool* bit-and b1 state))
(defn xri [state b1] (bool* bit-xor b1 state))
(defn ori [state b1] (bool* bit-or b1 state))
(defn ana-m [state] (bool* bit-and (byte-at-hl state) state))
(defn xra-m [state] (bool* bit-xor (byte-at-hl state) state))
(defn ora-m [state] (bool* bit-or (byte-at-hl state) state))

(defn rrc
  "Rotates accumulator register by one right shift"
  [state]
  (let [x (:cpu/a state)]
    (assoc state
           :cpu/a (bit-and (| (<< (bit-and x 1) 7) (>> x 1)) 0xff)
           :cpu/cc/cy (bit-and x 1))))

(defn rlc
  "Rotates accumulator register by one left shift"
  [state]
  (let [x (:cpu/a state)]
    (assoc state
           :cpu/a (bit-and (| (>> (bit-and x 0x80) 7) (bit-and (<< x 1) 0xff)) 0xff)
           :cpu/cc/cy (if (pos? (bit-and x 0x80)) 1 0))))

(defn ral
  "Rotates accumulator register through the carry by one left shift"
  [state]
  (let [x (:cpu/a state)]
    (assoc state
           :cpu/a (bit-and (| (<< x 1) (:cpu/cc/cy state)) 0xff)
           :cpu/cc/cy (if (pos? (bit-and x 0x80)) 1 0))))

(defn rar
  "Rotates accumulator register through the carry by one right shift"
  [state]
  (let [x (:cpu/a state)]
    (assoc state
           :cpu/a (bit-and (| (<< (:cpu/cc/cy state) 7) (>> x 1)) 0xff)
           :cpu/cc/cy (bit-and x 1))))

(defn- cmp*
  "Implements all compare ops"
  [x state]
  (let [ans (- (:cpu/a state) x)]
    (assoc state
           :cpu/cc/z (if (zero? ans) 1 0)
           :cpu/cc/s (if (= 0x80 (bit-and ans 0x80)) 1 0)
           :cpu/cc/cy (if (> x (:cpu/a state)) 1 0)
           :cpu/cc/p (parity ans))))

(defn cmp [reg state] (cmp* (reg state) state))
(defn cpi [state x] (cmp* x state))
(defn cmp-m [state] (cmp* (byte-at-hl state) state))

;; ----------------------------------------------------------------------------

(defn- mov*
  "Assigns value x to register"
  [reg x state]
  (assoc state reg x))

(defn mov [reg1 reg2 state] (mov* reg2 (reg1 state) state))
(defn mvi [reg state b] (mov* reg b state))
(defn mov-from-m [reg state] (mov* reg (byte-at-hl state) state))
(defn mov-to-m [reg state] (byte-to-hl state (reg state)))

(defn xchg
  "Exchange registers"
  [{:keys [cpu/d cpu/e cpu/h cpu/l] :as state}]
  (assoc state :cpu/d h, :cpu/e l, :cpu/h d, :cpu/l e))

(defn xthl
  "Exchange stack"
  [{:keys [cpu/h cpu/l cpu/sp cpu/mem] :as state}]
  (-> state
      (assoc :cpu/l (get mem sp), :cpu/h (get mem (inc sp)))
      (write-byte sp l)
      (write-byte (inc sp) h)))

(defn lxi
  "Implements all load immediate ops except LXI-SP."
  [hi lo state b1 b2]
  (assoc state hi b2, lo b1))

(defn lxi-sp
  "Load immediate to stack pointer"
  [state lo hi]
  (assoc state :cpu/sp (| (<< hi 8) lo)))

(defn lda
  "Load accumulator direct"
  [state lo hi]
  (let [addr (| (<< hi 8) lo)]
    (assoc state :cpu/a (get-byte state addr))))

(defn ldax
  "Load accumulator"
  [hi lo state]
  (let [addr (bit-and (| (<< (hi state) 8) (lo state)) 0xffff)]
    (assoc state :cpu/a (get-byte state addr))))

(defn lhld
  "Load h and l direct"
  [state lo hi]
  (let [addr (| (<< hi 8) lo)]
    (assoc state
           :cpu/l (get-byte state addr)
           :cpu/h (get-byte state (inc addr)))))

(defn sta
  "Store accumulator direct"
  [state lo hi]
  (let [addr (| (<< hi 8) lo)]
    (write-byte state addr (:cpu/a state))))

(defn stax
  "Store accumulator"
  [hi lo state]
  (let [addr (bit-and (| (<< (hi state) 8) (lo state)) 0xffff)]
    (write-byte state addr (:cpu/a state))))

(defn shld
  "Store h and l direct"
  [state lo hi]
  (let [addr (| (<< hi 8) lo)]
    (-> state
        (write-byte addr (:cpu/l state))
        (write-byte (inc addr) (:cpu/h state)))))

;; Stack group ----------------------------------------------------------------

(defn- push*
  "Implements all push ops"
  [hi lo state]
  (-> state
      (write-byte (- (:cpu/sp state) 1) hi)
      (write-byte (- (:cpu/sp state) 2) lo)
      (update :cpu/sp - 2)))

(defn push-b [{:keys [cpu/b cpu/c] :as state}] (push* b c state))
(defn push-d [{:keys [cpu/d cpu/e] :as state}] (push* d e state))
(defn push-h [{:keys [cpu/h cpu/l] :as state}] (push* h l state))
(defn push-pc [{:keys [cpu/pc] :as state}] (push* (>> (bit-and pc 0xff00) 8) (bit-and pc 0xff) state))

(defn push-psw
  [state]
  (let [{z :cpu/cc/z s :cpu/cc/s p :cpu/cc/p cy :cpu/cc/cy ac :cpu/cc/ac} state
        psw (| z (<< s 1) (<< p 2) (<< cy 3) (<< ac 4))]
    (push* (:cpu/a state) psw state)))

(defn- pop*
  "Implements all pop ops except POP-PSW"
  [reg-hi reg-lo state]
  (-> state
      (assoc reg-lo (get-byte state (:cpu/sp state)))
      (assoc reg-hi (get-byte state (inc (:cpu/sp state))))
      (update :cpu/sp + 2)))

(defn pop-b [state] (pop* :cpu/b :cpu/c state))
(defn pop-d [state] (pop* :cpu/d :cpu/e state))
(defn pop-h [state] (pop* :cpu/h :cpu/l state))

(defn pop-psw
  [state]
  (let [psw (get-byte state (:cpu/sp state))]
    (-> (assoc state
               :cpu/a (get-byte state (inc (:cpu/sp state)))
               :cpu/cc/z (if (pos? (bit-and psw 0x01)) 1 0)
               :cpu/cc/s (if (pos? (bit-and psw 0x02)) 1 0)
               :cpu/cc/p (if (pos? (bit-and psw 0x04)) 1 0)
               :cpu/cc/cy (if (pos? (bit-and psw 0x08)) 1 0)
               :cpu/cc/ac (if (pos? (bit-and psw 0x10)) 1 0))
        (update :cpu/sp + 2))))

(defn interrupt
  "Handles interrupts"
  [state i]
  (assoc (push-pc state) :cpu/pc (* 8 i), :cpu/int-enable? false))
