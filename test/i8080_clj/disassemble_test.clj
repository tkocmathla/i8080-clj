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
  (doseq [[opcode reg] (map vector [0x80 0x81 0x82 0x83 0x84 0x85] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] opcode) (assoc :cpu/a 0x6c reg 0x2e))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x9a (:cpu/a new-st)) (str "add :cpu/a and " reg))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 1 (:cpu/cc/s new-st)))
      (is (= 1 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest adc-test
  (doseq [[opcode reg] (map vector [0x88 0x89 0x8a 0x8b 0x8c 0x8d] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] opcode) (assoc :cpu/a 0x42 reg 0x3d))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x7f (:cpu/a new-st)) (str "adc :cpu/a and " reg))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 0 (:cpu/cc/s new-st)))
      (is (= 0 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest adi-test
  (let [ini-st (-> cpu
                   (assoc-in [:cpu/mem 0] 0xc6)
                   (assoc-in [:cpu/mem 1] 0x42)
                   (assoc :cpu/a 0x14))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 2 (:cpu/pc new-st)))
    (is (= 0x56 (:cpu/a new-st)))
    (is (= 0 (:cpu/cc/z new-st)))
    (is (= 0 (:cpu/cc/s new-st)))
    (is (= 1 (:cpu/cc/p new-st)))
    (is (= 0 (:cpu/cc/cy new-st)))))

(deftest ana-c-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0xa1) (assoc :cpu/a 0xfc :cpu/c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xc (:cpu/a new-st)))))

(deftest cma-test
  (let [ini-st (assoc cpu :cpu/a 0x51 :cpu/mem [0x2f])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xae (:cpu/a new-st)))))

(deftest cmp-test
  (doseq [[opcode reg] (map vector [0xb8 0xb9 0xba 0xbb 0xbc 0xbd] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (assoc cpu :cpu/a 0x05 reg 0xa :cpu/mem [opcode])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 1 (:cpu/cc/s new-st)))
      (is (= 0 (:cpu/cc/p new-st))))))

(deftest daa-test
  (let [ini-st (assoc cpu :cpu/a 0x9b :cpu/mem [0x27])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAA))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest dad-b-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/b 0x03 :cpu/c 0x39 :cpu/h 0x0a :cpu/l 0x17 :cpu/mem [0x09])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-B))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x0d (:cpu/h new-st)))
      (is (= 0x50 (:cpu/l new-st))))))

(deftest dad-d-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/d 0x03 :cpu/e 0x39 :cpu/h 0x0a :cpu/l 0x17 :cpu/mem [0x19])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-D))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x0d (:cpu/h new-st)))
      (is (= 0x50 (:cpu/l new-st))))))

(deftest dad-h-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/h 0x0a :cpu/l 0x17 :cpu/mem [0x29])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DAD-H))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x14 (:cpu/h new-st)))
      (is (= 0x2e (:cpu/l new-st))))))

(deftest dcr-b-test
  (let [ini-st (assoc cpu :cpu/b 0x02 :cpu/mem [0x05])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCR-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/b new-st)))
    (is (= 0 (:cpu/cc/s new-st)))
    (is (= 0 (:cpu/cc/p new-st)))))

(deftest dcr-m-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/h 0x00 :cpu/l 0x01 :cpu/mem [0x35 0x20])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :DCR-M))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x1f (opfns/byte-at-hl new-st)))
      (is (= 0 (:cpu/cc/p new-st))))))

(deftest dcx-h-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0x2b) (assoc :cpu/h 0x98 :cpu/l 0x00))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCX-H))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x97 (:cpu/h new-st)))
    (is (= 0xff (:cpu/l new-st)))))

(deftest inr-test
  (let [ini-st (assoc cpu :cpu/c 0x99 :cpu/mem [0x0c])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INR-C))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x9a (:cpu/c new-st)))))

(deftest inx-test
  (let [ini-st (assoc cpu :cpu/d 0x38 :cpu/e 0xff :cpu/mem [0x13])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-D))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x39 (:cpu/d new-st)))
    (is (= 0x00 (:cpu/e new-st)))))

(deftest inx-sp-test
  (let [ini-st (assoc cpu :cpu/sp 0xffff :cpu/mem [0x33])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-SP))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0 (:cpu/sp new-st)))))

(deftest jmp-test
  (let [ini-st (assoc cpu :cpu/mem [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest ldax-d-test
  (let [ini-st (-> cpu
                   (assoc-in [:cpu/mem 0] 0x1a)
                   (assoc-in [:cpu/mem 0x938b] 0x42)
                   (assoc :cpu/d 0x93 :cpu/e 0x8b))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LDAX-D))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x42 (:cpu/a new-st)))))

(deftest lhld-test
  (let [ini-st (assoc cpu :cpu/mem [0x2a 0x03 0x00 0xff 0x03])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LHLD))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x03 (:cpu/h new-st)))
    (is (= 0xff (:cpu/l new-st)))))

(deftest lxi-d-test
  (let [ini-st (assoc cpu :cpu/e 3 :cpu/mem [0x11 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-D))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x01 (:cpu/e new-st)))
    (is (= 0x02 (:cpu/d new-st)))))

(deftest lxi-h-test
  (let [ini-st (assoc cpu :cpu/mem [0x21 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-H))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x01 (:cpu/l new-st)))
    (is (= 0x02 (:cpu/h new-st)))))

(deftest lxi-sp-test
  (let [ini-st (assoc cpu :cpu/mem [0x31 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-SP))
    (is (= 3 (:cpu/pc new-st)))
    ; 0x02 (hi) 0x01 (lo) == 513
    (is (= 513 (:cpu/sp new-st)))))

(deftest mov-b-b-test
  (let [ini-st (assoc cpu :cpu/mem [0x40])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest mov-b-c-test
  (let [ini-st (assoc cpu :cpu/c 1 :cpu/mem [0x41])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-C))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (:cpu/c ini-st) (:cpu/b new-st)))))

(deftest mov-m-a-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/a 0x42 :cpu/h 0x00 :cpu/l 0x03 :cpu/mem [0x77 0x00 0x00 0x00])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :MOV-M-A))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x42 (get-in new-st [:cpu/mem 0x03]))))))

(deftest mvi-a-test
  (let [ini-st (assoc cpu :cpu/mem [0x3e 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MVI-A))
    (is (= 2 (:cpu/pc new-st)))
    (is (= (:cpu/a new-st) 0x42))))

(deftest nop-test
  (let [ini-st (assoc cpu :cpu/mem [0x00])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest ora-c-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0xb1) (assoc :cpu/a 0x33 :cpu/c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x3f (:cpu/a new-st)))))

(deftest pchl-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0xe9) (assoc :cpu/h 0x41 :cpu/l 0x3e))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= :PCHL (:op op)))
    (is (= 0x413e (:cpu/pc new-st)))))

(deftest pop-b-test
  (let [ini-st (assoc cpu :cpu/sp 0x02 :cpu/mem [0xc1 0x00 0x11 0x22])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :POP-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 4 (:cpu/sp new-st)))
    (is (= 0x22 (:cpu/b new-st)))
    (is (= 0x11 (:cpu/c new-st)))))

; FIXME adapt cc flags to new naming convention
;(deftest pop-psw-test
;  (let [ini-st (assoc cpu :cpu/sp 0x01 :cpu/mem [0xf1 2r00011111 0x42])
;        op (disassemble-op ini-st)
;        new-st (execute-op ini-st op)]
;    (is (= (:op op) :POP-PSW))
;    (is (= 1 (:cpu/pc new-st)))
;    (is (= 3 (:cpu/sp new-st)))
;    (is (every? #{0} (vals (:cc ini-st))))
;    (is (every? #{1} (vals (:cc new-st))))
;    (is (= 0x42 (:cpu/a new-st)))))

(deftest push-b-test
  (binding [*protect-mem* false]
    (let [ini-st (assoc cpu :cpu/b 0x11 :cpu/c 0x22 :cpu/sp 0x03 :cpu/mem [0xc5 0x00 0x00 0x00])
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :PUSH-B))
      (is (= 1 (:cpu/pc new-st)))
      (is (= 1 (:cpu/sp new-st)))
      (is (= 0x22 (get-in new-st [:cpu/mem (:cpu/sp new-st)])))
      (is (= 0x11 (get-in new-st [:cpu/mem (inc (:cpu/sp new-st))]))))))

; FIXME adapt cc flags to new naming convention
;(deftest push-psw-test
;  (binding [*protect-mem* false]
;    (let [ini-st (assoc cpu :cpu/a 0x11 :cpu/sp 3 :cpu/mem [0xf5 0x00 0x00 0x00] :cc {:z 1 :s 1 :p 1 :cy 1 :ac 1})
;          op (disassemble-op ini-st)
;          new-st (execute-op ini-st op)]
;      (is (= (:op op) :PUSH-PSW))
;      (is (= 1 (:cpu/pc new-st)))
;      (is (= 1 (:cpu/sp new-st)))
;      (is (= 2r00000000 (get-in ini-st [:cpu/mem (:cpu/sp new-st)])))
;      (is (= 2r00011111 (get-in new-st [:cpu/mem (:cpu/sp new-st)])))
;      (is (= 0x11 (get-in new-st [:cpu/mem (inc (:cpu/sp new-st))]))))))

(deftest ral-test
  (let [ini-st (assoc cpu :cpu/a 0xb5 :cpu/mem [0x17])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAL))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x6a (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest rar-test
  (let [ini-st (-> cpu (assoc :cpu/a 0x6a :cpu/mem [0x1f] :cpu/cc/cy 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAR))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xb5 (:cpu/a new-st)))
    (is (= 0 (:cpu/cc/cy new-st)))))

(deftest rlc-test
  (let [ini-st (assoc cpu :cpu/a 0xf2 :cpu/mem [0x07])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RLC))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xe5 (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest rrc-test
  (let [ini-st (assoc cpu :cpu/a 0xf2 :cpu/mem [0x0f])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RRC))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x79 (:cpu/a new-st)))
    (is (= 0 (:cpu/cc/cy new-st)))))

(deftest sbb-test
  (doseq [[opcode reg] (map vector [0x98 0x99 0x9a 0x9b 0x9c 0x9d] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> cpu
                     (assoc-in [:cpu/mem 0] opcode)
                     (assoc :cpu/a 0x4 reg 0x2 :cpu/cc/cy 1))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x1 (:cpu/a new-st)) (str "sbb :cpu/a and " reg))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 0 (:cpu/cc/s new-st)))
      (is (= 0 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest shld-test
  (binding [*protect-mem* false]
    (let [ini-st (-> cpu (assoc :cpu/h 0xae :cpu/l 0x29 :cpu/mem [0x22 0x03 0x00 0x00 0x00]))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= (:op op) :SHLD))
      (is (= 3 (:cpu/pc new-st)))
      (is (= 0x29 (get-in new-st [:cpu/mem 3])))
      (is (= 0xae (get-in new-st [:cpu/mem 4]))))))

(deftest stax-b-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0x02) (assoc :cpu/a 0x11 :cpu/b 0x3f :cpu/c 0x16))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :STAX-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x11 (get-in new-st [:cpu/mem 0x3f16])))))

(deftest sub-test
  (doseq [[opcode reg] (map vector [0x90 0x91 0x92 0x93 0x94 0x95] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] opcode) (assoc :cpu/a 0x3e reg 0x3e))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0 (:cpu/a new-st)) (str "sub :cpu/a and " reg))
      (is (= 1 (:cpu/cc/z new-st)))
      (is (= 0 (:cpu/cc/s new-st)))
      (is (= 1 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest xchg-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0xeb) (assoc :cpu/h 0 :cpu/l 0xff :cpu/d 0x33 :cpu/e 0x55))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0 (:cpu/d new-st)))
    (is (= 0xff (:cpu/e new-st)))
    (is (= 0x33 (:cpu/h new-st)))
    (is (= 0x55 (:cpu/l new-st)))))

(deftest xthl-test
  (let [ini-st (-> cpu (assoc :cpu/mem [0xe3 0xf0 0x0d]) (assoc :cpu/h 0x0b :cpu/l 0x3c :cpu/sp 0x01))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :XTHL))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x3c (get-in new-st [:cpu/mem (:cpu/sp new-st)])))
    (is (= 0x0b (get-in new-st [:cpu/mem (inc (:cpu/sp new-st))])))
    (is (= 0x0d (:cpu/h new-st)))
    (is (= 0xf0 (:cpu/l new-st)))))

(deftest xra-b-test
  (let [ini-st (-> cpu (assoc-in [:cpu/mem 0] 0xa8) (assoc :cpu/a 0xff :cpu/b 2r10101010))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 2r01010101 (:cpu/a new-st)))))
