(ns i8080-clj.disassemble-test
  (:require
    [clojure.test :refer :all]
    [i8080-clj.core :refer [initial-state]]
    [i8080-clj.disassemble :as dis]))

(deftest jmp-test
  (let [ini-st (assoc initial-state :mem [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest lxi-d-test
  (let [ini-st (assoc initial-state :e 1 :mem [0x11 0x01 0x02])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :LXI-D))
    (is (= 3 (:pc new-st)))
    (is (= 0x01 (:e ini-st)))
    (is (= 0x02 (:d new-st)))))

(deftest lxi-sp-test
  (let [ini-st (assoc initial-state :mem [0x31 0x01 0x02])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :LXI-SP))
    (is (= 3 (:pc new-st)))
    ; 0x02 (hi) 0x01 (lo) == 513
    (is (= 513 (:sp new-st)))))

(deftest mov-b-b-test
  (let [ini-st (assoc initial-state :mem [0x40])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :MOV-B-B))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest mov-b-c-test
  (let [ini-st (assoc initial-state :c 1 :mem [0x41])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :MOV-B-C))
    (is (= 1 (:pc new-st)))
    (is (= (:c ini-st) (:b new-st)))))

(deftest mvi-b-test
  (let [ini-st (assoc initial-state :c 1 :mem [0x06 0x42])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :MVI-B))
    (is (= 2 (:pc new-st)))
    (is (= (:b new-st) 0x42))))

(deftest nop-test
  (let [ini-st (assoc initial-state :mem [0x00])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest ral-test
  (let [ini-st (-> initial-state (assoc :a 2 :mem [0x17]) (assoc-in [:cc :cy] 1))
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :RAL))
    (is (= 1 (:pc new-st)))
    (is (= 5 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest rar-test
  (let [ini-st (-> initial-state (assoc :a 2 :mem [0x1f]) (assoc-in [:cc :cy] 1))
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :RAR))
    (is (= 1 (:pc new-st)))
    (is (= 0x81 (:a new-st)))
    (is (= 0 (get-in new-st [:cc :cy])))))

(deftest rlc-test
  (let [ini-st (assoc initial-state :a 0x80 :mem [0x07])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :RLC))
    (is (= 1 (:pc new-st)))
    (is (= 1 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))

(deftest rrc-test
  (let [ini-st (assoc initial-state :a 1 :mem [0x0f])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :RRC))
    (is (= 1 (:pc new-st)))
    (is (= 0x80 (:a new-st)))
    (is (= 1 (get-in new-st [:cc :cy])))))
