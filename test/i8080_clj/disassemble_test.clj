(ns i8080-clj.disassemble-test
  (:require
    [clojure.test :refer :all]
    [i8080-clj.core :refer :all]
    [i8080-clj.opfns :refer [*protect-mem*] :as opfns]))

(deftest parity-test
  (is (= 2r10) 0)
  (is (= 2r11) 1)
  (is (= 2r101) 1)
  (is (= 2r100) 0))

;; ----------------------------------------------------------------------------

(deftest add-test
  (doseq [[opcode reg] (map vector [0x80 0x81 0x82 0x83 0x84 0x85] [:b :c :d :e :h :l])]
    (let [ini-st (-> initial-state (assoc-in [:mem 0] opcode) (assoc :a 0x6c reg 0x2e))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0x9a (:a new-st)) (str "add :a and " reg))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 1 (get-in new-st [:cc :s])))
      (is (= 1 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy]))))))

(deftest adc-test
  (doseq [[opcode reg] (map vector [0x88 0x89 0x8a 0x8b 0x8c 0x8d] [:b :c :d :e :h :l])]
    (let [ini-st (-> initial-state (assoc-in [:mem 0] opcode) (assoc :a 0x42 reg 0x3d))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0x7f (:a new-st)) (str "adc :a and " reg))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 0 (get-in new-st [:cc :s])))
      (is (= 0 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy]))))))

(deftest adi-test
  (let [ini-st (-> initial-state
                   (assoc-in [:mem 0] 0xc6)
                   (assoc-in [:mem 1] 0x42)
                   (assoc :a 0x14))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 2 (:pc new-st)))
    (is (= 0x56 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :z])))
    (is (= 0 (get-in new-st [:cc :s])))
    (is (= 1 (get-in new-st [:cc :p])))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest ana-c-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0xa1) (assoc :a 0xfc :c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:pc new-st)))
    (is (= 0xc (:a new-st)))))

(deftest cma-test
  (let [ini-st (assoc initial-state :a 0x51 :mem [0x2f])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:pc new-st)))
    (is (= 0xae (:a new-st)))))

(deftest cmp-test
  (doseq [[opcode reg] (map vector [0xb8 0xb9 0xba 0xbb 0xbc 0xbd] [:b :c :d :e :h :l])]
    (let [ini-st (assoc initial-state :a 0x05 reg 0xa :mem [opcode])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 1 (get-in new-st [:cc :s])))
      (is (= 0 (get-in new-st [:cc :p]))))))

(deftest daa-test
  (let [ini-st (assoc initial-state :a 0x9b :mem [0x27])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAA))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))

(deftest dad-b-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :b 0x03 :c 0x39 :h 0x0a :l 0x17 :mem [0x09])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-B))
      (is (= 1 (:pc new-st)))
      (is (= 0x0d (:h new-st)))
      (is (= 0x50 (:l new-st))))))

(deftest dad-d-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :d 0x03 :e 0x39 :h 0x0a :l 0x17 :mem [0x19])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-D))
      (is (= 1 (:pc new-st)))
      (is (= 0x0d (:h new-st)))
      (is (= 0x50 (:l new-st))))))

(deftest dad-h-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :h 0x0a :l 0x17 :mem [0x29])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-H))
      (is (= 1 (:pc new-st)))
      (is (= 0x14 (:h new-st)))
      (is (= 0x2e (:l new-st))))))

(deftest dcr-b-test
  (let [ini-st (assoc initial-state :b 0x02 :mem [0x05])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCR-B))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:b new-st)))
    (is (= 0 (get-in new-st [:cc :s])))
    (is (= 0 (get-in new-st [:cc :p])))))

(deftest dcr-m-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :h 0x00 :l 0x01 :mem [0x35 0x20])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DCR-M))
      (is (= 1 (:pc new-st)))
      (is (= 0x1f (opfns/byte-at-hl new-st)))
      (is (= 0 (get-in new-st [:cc :p]))))))

(deftest dcx-h-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0x2b) (assoc :h 0x98 :l 0x00))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCX-H))
    (is (= 1 (:pc new-st)))
    (is (= 0x97 (:h new-st)))
    (is (= 0xff (:l new-st)))))

(deftest inr-test
  (let [ini-st (assoc initial-state :c 0x99 :mem [0x0c])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INR-C))
    (is (= 1 (:pc new-st)))
    (is (= 0x9a (:c new-st)))))

(deftest inx-test
  (let [ini-st (assoc initial-state :d 0x38 :e 0xff :mem [0x13])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-D))
    (is (= 1 (:pc new-st)))
    (is (= 0x39 (:d new-st)))
    (is (= 0x00 (:e new-st)))))

(deftest inx-sp-test
  (let [ini-st (assoc initial-state :sp 0xffff :mem [0x33])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-SP))
    (is (= 1 (:pc new-st)))
    (is (= 0 (:sp new-st)))))

(deftest jmp-test
  (let [ini-st (assoc initial-state :mem [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest ldax-d-test
  (let [ini-st (-> initial-state
                   (assoc-in [:mem 0] 0x1a)
                   (assoc-in [:mem 0x938b] 0x42)
                   (assoc :d 0x93 :e 0x8b))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LDAX-D))
    (is (= 1 (:pc new-st)))
    (is (= 0x42 (:a new-st)))))

(deftest lhld-test
  (let [ini-st (assoc initial-state :mem [0x2a 0x03 0x00 0xff 0x03])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LHLD))
    (is (= 3 (:pc new-st)))
    (is (= 0x03 (:h new-st)))
    (is (= 0xff (:l new-st)))))

(deftest lxi-d-test
  (let [ini-st (assoc initial-state :e 3 :mem [0x11 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-D))
    (is (= 3 (:pc new-st)))
    (is (= 0x01 (:e new-st)))
    (is (= 0x02 (:d new-st)))))

(deftest lxi-h-test
  (let [ini-st (assoc initial-state :mem [0x21 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-H))
    (is (= 3 (:pc new-st)))
    (is (= 0x01 (:l new-st)))
    (is (= 0x02 (:h new-st)))))

(deftest lxi-sp-test
  (let [ini-st (assoc initial-state :mem [0x31 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-SP))
    (is (= 3 (:pc new-st)))
    ; 0x02 (hi) 0x01 (lo) == 513
    (is (= 513 (:sp new-st)))))

(deftest mov-b-b-test
  (let [ini-st (assoc initial-state :mem [0x40])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-B))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest mov-b-c-test
  (let [ini-st (assoc initial-state :c 1 :mem [0x41])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-C))
    (is (= 1 (:pc new-st)))
    (is (= (:c ini-st) (:b new-st)))))

(deftest mov-m-a-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :a 0x42 :h 0x00 :l 0x03 :mem [0x77 0x00 0x00 0x00])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :MOV-M-A))
      (is (= 1 (:pc new-st)))
      (is (= 0x42 (get-in new-st [:mem 0x03]))))))

(deftest mvi-a-test
  (let [ini-st (assoc initial-state :mem [0x3e 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MVI-A))
    (is (= 2 (:pc new-st)))
    (is (= (:a new-st) 0x42))))

(deftest nop-test
  (let [ini-st (assoc initial-state :mem [0x00])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest ora-c-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0xb1) (assoc :a 0x33 :c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:pc new-st)))
    (is (= 0x3f (:a new-st)))))

(deftest pchl-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0xe9) (assoc :h 0x41 :l 0x3e))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= :PCHL (:op op)))
    (is (= 0x413e (:pc new-st)))))

(deftest pop-b-test
  (let [ini-st (assoc initial-state :sp 0x02 :mem [0xc1 0x00 0x11 0x22])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :POP-B))
    (is (= 1 (:pc new-st)))
    (is (= 4 (:sp new-st)))
    (is (= 0x22 (:b new-st)))
    (is (= 0x11 (:c new-st)))))

(deftest pop-psw-test
  (let [ini-st (assoc initial-state :sp 0x01 :mem [0xf1 2r00011111 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :POP-PSW))
    (is (= 1 (:pc new-st)))
    (is (= 3 (:sp new-st)))
    (is (every? #{0} (vals (:cc ini-st))))
    (is (every? #{1} (vals (:cc new-st))))
    (is (= 0x42 (:a new-st)))))

(deftest push-b-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :b 0x11 :c 0x22 :sp 0x03 :mem [0xc5 0x00 0x00 0x00])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :PUSH-B))
      (is (= 1 (:pc new-st)))
      (is (= 1 (:sp new-st)))
      (is (= 0x22 (get-in new-st [:mem (:sp new-st)])))
      (is (= 0x11 (get-in new-st [:mem (inc (:sp new-st))]))))))

(deftest push-psw-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc initial-state :a 0x11 :sp 3 :mem [0xf5 0x00 0x00 0x00] :cc {:z 1 :s 1 :p 1 :cy 1 :ac 1})
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :PUSH-PSW))
      (is (= 1 (:pc new-st)))
      (is (= 1 (:sp new-st)))
      (is (= 2r00000000 (get-in ini-st [:mem (:sp new-st)])))
      (is (= 2r00011111 (get-in new-st [:mem (:sp new-st)])))
      (is (= 0x11 (get-in new-st [:mem (inc (:sp new-st))]))))))

(deftest ral-test
  (let [ini-st (assoc initial-state :a 0xb5 :mem [0x17])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAL))
    (is (= 1 (:pc new-st)))
    (is (= 0x6a (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))

(deftest rar-test
  (let [ini-st (-> initial-state (assoc :a 0x6a :mem [0x1f]) (assoc-in [:cc :cy] 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAR))
    (is (= 1 (:pc new-st)))
    (is (= 0xb5 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest rlc-test
  (let [ini-st (assoc initial-state :a 0xf2 :mem [0x07])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RLC))
    (is (= 1 (:pc new-st)))
    (is (= 0xe5 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))

(deftest rrc-test
  (let [ini-st (assoc initial-state :a 0xf2 :mem [0x0f])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RRC))
    (is (= 1 (:pc new-st)))
    (is (= 0x79 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest sbb-test
  (doseq [[opcode reg] (map vector [0x98 0x99 0x9a 0x9b 0x9c 0x9d] [:b :c :d :e :h :l])]
    (let [ini-st (-> initial-state
                     (assoc-in [:mem 0] opcode)
                     (assoc-in [:cc :cy] 1)
                     (assoc :a 0x4 reg 0x2))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0x1 (:a new-st)) (str "sbb :a and " reg))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 0 (get-in new-st [:cc :s])))
      (is (= 0 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy]))))))

(deftest shld-test
  (binding [*protect-mem* false]
    (let [ini-st (-> initial-state (assoc :h 0xae :l 0x29 :mem [0x22 0x03 0x00 0x00 0x00]))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :SHLD))
      (is (= 3 (:pc new-st)))
      (is (= 0x29 (get-in new-st [:mem 3])))
      (is (= 0xae (get-in new-st [:mem 4]))))))

(deftest stax-b-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0x02) (assoc :a 0x11 :b 0x3f :c 0x16))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :STAX-B))
    (is (= 1 (:pc new-st)))
    (is (= 0x11 (get-in new-st [:mem 0x3f16])))))

(deftest sub-test
  (doseq [[opcode reg] (map vector [0x90 0x91 0x92 0x93 0x94 0x95] [:b :c :d :e :h :l])]
    (let [ini-st (-> initial-state (assoc-in [:mem 0] opcode) (assoc :a 0x3e reg 0x3e))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0 (:a new-st)) (str "sub :a and " reg))
      (is (= 1 (get-in new-st [:cc :z])))
      (is (= 0 (get-in new-st [:cc :s])))
      (is (= 1 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy]))))))

(deftest xchg-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0xeb) (assoc :h 0 :l 0xff :d 0x33 :e 0x55))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:pc new-st)))
    (is (= 0 (:d new-st)))
    (is (= 0xff (:e new-st)))
    (is (= 0x33 (:h new-st)))
    (is (= 0x55 (:l new-st)))))

(deftest xthl-test
  (let [ini-st (-> initial-state (assoc :mem [0xe3 0xf0 0x0d]) (assoc :h 0x0b :l 0x3c :sp 0x01))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :XTHL))
    (is (= 1 (:pc new-st)))
    (is (= 0x3c (get-in new-st [:mem (:sp new-st)])))
    (is (= 0x0b (get-in new-st [:mem (inc (:sp new-st))])))
    (is (= 0x0d (:h new-st)))
    (is (= 0xf0 (:l new-st)))))

(deftest xra-b-test
  (let [ini-st (-> initial-state (assoc-in [:mem 0] 0xa8) (assoc :a 0xff :b 2r10101010))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:pc new-st)))
    (is (= 2r01010101 (:a new-st)))))
