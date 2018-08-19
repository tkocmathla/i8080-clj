(ns i8080-clj.disassemble-test
  (:require
    [clojure.test :refer :all]
    [i8080-clj.core :refer :all]))

(deftest parity-test
  (is (= 2r10) 0)
  (is (= 2r11) 1)
  (is (= 2r101) 1)
  (is (= 2r100) 0))

;; ----------------------------------------------------------------------------

(deftest add-test
  (doseq [[opcode reg] (map vector [0x80 0x81 0x82 0x83 0x84 0x85] [:b :c :d :e :h :l])]
    (let [ini-st (assoc initial-state :a 3 reg 6 :mem [opcode])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 9 (:a new-st)) (str "add :a and " reg))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 0 (get-in new-st [:cc :s])))
      (is (= 1 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy])))
      (is (= 0 (get-in new-st [:cc :ac]))))))

(deftest adc-test
  (doseq [[opcode reg] (map vector [0x88 0x89 0x8a 0x8b 0x8c 0x8d] [:b :c :d :e :h :l])]
    (let [ini-st (assoc initial-state :a 250 reg 10 :mem [opcode])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 4 (:a new-st)) (str "adc :a and " reg))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 0 (get-in new-st [:cc :s])))
      (is (= 0 (get-in new-st [:cc :p])))
      (is (= 1 (get-in new-st [:cc :cy])))
      (is (= 0 (get-in new-st [:cc :ac]))))))

(deftest cmp-test
  (doseq [[opcode reg] (map vector [0xb8 0xb9 0xba 0xbb 0xbc 0xbd] [:b :c :d :e :h :l])]
    (let [ini-st (assoc initial-state :a 0x05 reg 0xa :mem [opcode])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:pc new-st)))
      (is (= 0 (get-in new-st [:cc :z])))
      (is (= 1 (get-in new-st [:cc :s])))
      (is (= 0 (get-in new-st [:cc :p])))
      (is (= 0 (get-in new-st [:cc :cy])))
      (is (= 0 (get-in new-st [:cc :ac]))))))

(deftest dad-b-test
  (let [ini-st (assoc initial-state :b 0x01 :c 0x01 :h 0x01 :l 0xff :mem [0x09])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAD-B))
    (is (= 1 (:pc new-st)))
    (is (= 0x03 (new-st :h)))
    (is (= 0x00 (new-st :l)))))

(deftest dcr-b-test
  (let [ini-st (assoc initial-state :b 0x00 :mem [0x05])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCR-B))
    (is (= 1 (:pc new-st)))
    (is (= 0xff (new-st :b)))
    (is (= 1 (get-in new-st [:cc :s])))))

(deftest inx-test
  (let [ini-st (assoc initial-state :e 0xff :mem [0x13])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-D))
    (is (= 1 (:pc new-st)))
    (is (= 0x01 (new-st :d)))
    (is (= 0x00 (new-st :e)))))

(deftest jmp-test
  (let [ini-st (assoc initial-state :mem [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest ldax-d-test
  (let [ini-st (assoc initial-state :d 0x00 :e 0x03 :mem [0x1a 0x00 0x00 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LDAX-D))
    (is (= 1 (:pc new-st)))
    (is (= 0x42 (:a new-st)))))

(deftest lxi-d-test
  (let [ini-st (assoc initial-state :e 1 :mem [0x11 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-D))
    (is (= 3 (:pc new-st)))
    (is (= 0x01 (:e ini-st)))
    (is (= 0x02 (:d new-st)))))

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
  (let [ini-st (assoc initial-state :a 0x42 :h 0x00 :l 0x03 :mem [0x77 0x00 0x00 0x00])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-M-A))
    (is (= 1 (:pc new-st)))
    (is (= 0x42 (get-in new-st [:mem 0x03])))))

(deftest mvi-b-test
  (let [ini-st (assoc initial-state :c 1 :mem [0x06 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MVI-B))
    (is (= 2 (:pc new-st)))
    (is (= (:b new-st) 0x42))))

(deftest nop-test
  (let [ini-st (assoc initial-state :mem [0x00])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

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
  (let [ini-st (assoc initial-state :b 0x11 :c 0x22 :sp 0x03 :mem [0xc5 0x00 0x00 0x00])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :PUSH-B))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:sp new-st)))
    (is (= 0x22 (get-in new-st [:mem (:sp new-st)])))
    (is (= 0x11 (get-in new-st [:mem (inc (:sp new-st))])))))

(deftest push-psw-test
  (let [ini-st (assoc initial-state :a 0x11 :sp 3 :mem [0xf5 0x00 0x00 0x00] :cc {:z 1 :s 1 :p 1 :cy 1 :ac 1})
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :PUSH-PSW))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:sp new-st)))
    (is (= 2r00000000 (get-in ini-st [:mem (:sp new-st)])))
    (is (= 2r00011111 (get-in new-st [:mem (:sp new-st)])))
    (is (= 0x11 (get-in new-st [:mem (inc (:sp new-st))])))))

(deftest ral-test
  (let [ini-st (-> initial-state (assoc :a 2 :mem [0x17]) (assoc-in [:cc :cy] 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAL))
    (is (= 1 (:pc new-st)))
    (is (= 5 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest rar-test
  (let [ini-st (-> initial-state (assoc :a 2 :mem [0x1f]) (assoc-in [:cc :cy] 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAR))
    (is (= 1 (:pc new-st)))
    (is (= 0x81 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest rlc-test
  (let [ini-st (assoc initial-state :a 0x80 :mem [0x07])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RLC))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))

(deftest rrc-test
  (let [ini-st (assoc initial-state :a 1 :mem [0x0f])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RRC))
    (is (= 1 (:pc new-st)))
    (is (= 0x80 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))
