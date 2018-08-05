(ns i8080-clj.disassemble-test
  (:require
    [clojure.test :refer :all]
    [i8080-clj.core :refer [initial-state]]
    [i8080-clj.disassemble :as dis]))

(deftest nop-test
  (let [ini-st (assoc initial-state :mem [0x00])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest jmp-test
  (let [ini-st (assoc initial-state :mem [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

(deftest mov-b-b-test
  (let [ini-st (assoc initial-state :mem [0x40])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :MOV-B-B))
    (is (= 1 (:pc new-st)))
    (is (= (dissoc ini-st :pc) (dissoc new-st :pc)))))

; TODO write these automatically
(deftest mov-b-c-test
  (let [ini-st (assoc initial-state :c 1 :mem [0x41])
        op (dis/disassemble-op ini-st)
        new-st (dis/execute-op ini-st op)]
    (is (= (:op op) :MOV-B-C))
    (is (= 1 (:pc new-st)))
    (is (= (:c ini-st) (:b new-st)))))
