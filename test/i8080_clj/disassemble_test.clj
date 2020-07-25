(ns i8080-clj.disassemble-test
  (:require
    [clojure.test :refer [is deftest use-fixtures]]
    [i8080-clj.api :refer [get-args disassemble-op execute-op]]
    [i8080-clj.core :refer [cpu]]
    [i8080-clj.opfns :refer [get-byte parity write-byte write-bytes] :as opfns]))

(use-fixtures :once (fn [f] (binding [opfns/*protect-mem* false] (f))))

(deftest parity-test
  (is (= (parity 2r10) 0))
  (is (= (parity 2r11) 1))
  (is (= (parity 2r101) 1))
  (is (= (parity 2r100) 0)))

;; ----------------------------------------------------------------------------

(deftest add-test
  (doseq [[opcode reg] (map vector [0x80 0x81 0x82 0x83 0x84 0x85] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> (cpu) (write-byte 0 opcode) (assoc :cpu/a 0x6c reg 0x2e))
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
    (let [ini-st (-> (cpu) (write-byte 0 opcode) (assoc :cpu/a 0x42 reg 0x3d))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0x7f (:cpu/a new-st)) (str "adc :cpu/a and " reg))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 0 (:cpu/cc/s new-st)))
      (is (= 0 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest adi-test
  (let [ini-st (-> (cpu)
                   (write-byte 0 0xc6)
                   (write-byte 1 0x42)
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
  (let [ini-st (-> (cpu) (write-byte 0 0xa1) (assoc :cpu/a 0xfc :cpu/c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xc (:cpu/a new-st)))))

(deftest cma-test
  (let [ini-st (-> (cpu) (write-byte 0 0x2f) (assoc :cpu/a 0x51))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xae (:cpu/a new-st)))))

(deftest cmp-test
  (doseq [[opcode reg] (map vector [0xb8 0xb9 0xba 0xbb 0xbc 0xbd] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> (cpu) (write-byte 0 opcode) (assoc :cpu/a 0x05 reg 0xa))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0 (:cpu/cc/z new-st)))
      (is (= 1 (:cpu/cc/s new-st)))
      (is (= 0 (:cpu/cc/p new-st))))))

(deftest daa-test
  (let [ini-st (-> (cpu) (write-byte 0 0x27) (assoc :cpu/a 0x9b))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAA))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest dad-b-test
  (let [ini-st (-> (cpu) (write-byte 0 0x9) (assoc :cpu/b 0x03 :cpu/c 0x39 :cpu/h 0x0a :cpu/l 0x17))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAD-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x0d (:cpu/h new-st)))
    (is (= 0x50 (:cpu/l new-st)))))

(deftest dad-d-test
  (let [ini-st (-> (cpu) (write-byte 0 0x19) (assoc :cpu/d 0x03 :cpu/e 0x39 :cpu/h 0x0a :cpu/l 0x17))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAD-D))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x0d (:cpu/h new-st)))
    (is (= 0x50 (:cpu/l new-st)))))

(deftest dad-h-test
  (let [ini-st (-> (cpu) (write-byte 0 0x29) (assoc :cpu/h 0x0a :cpu/l 0x17))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DAD-H))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x14 (:cpu/h new-st)))
    (is (= 0x2e (:cpu/l new-st)))))

(deftest dcr-b-test
  (let [ini-st (-> (cpu) (write-byte 0 0x5) (assoc :cpu/b 0x02))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCR-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/b new-st)))
    (is (= 0 (:cpu/cc/s new-st)))
    (is (= 0 (:cpu/cc/p new-st)))))

(deftest dcr-m-test
  (let [ini-st (-> (cpu) (write-byte 0 0x35) (write-byte 1 0x20) (assoc :cpu/h 0x00 :cpu/l 0x01))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCR-M))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x1f (opfns/byte-at-hl new-st)))
    (is (= 0 (:cpu/cc/p new-st)))))

(deftest dcx-h-test
  (let [ini-st (-> (cpu) (write-byte 0 0x2b) (assoc :cpu/h 0x98 :cpu/l 0x00))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :DCX-H))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x97 (:cpu/h new-st)))
    (is (= 0xff (:cpu/l new-st)))))

(deftest ei-test
  (let [ini-st (write-byte (cpu) 0 0xfb)
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :EI))
    (is (true? (:cpu/int-enable? new-st)))))

(deftest inr-test
  (let [ini-st (-> (cpu) (write-byte 0 0x0c) (assoc :cpu/c 0x99))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INR-C))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x9a (:cpu/c new-st)))))

(deftest inx-test
  (let [ini-st (-> (cpu) (write-byte 0 0x13) (assoc :cpu/d 0x38 :cpu/e 0xff))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-D))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x39 (:cpu/d new-st)))
    (is (= 0x00 (:cpu/e new-st)))))

(deftest inx-sp-test
  (let [ini-st (-> (cpu) (write-byte 0 0x33) (assoc :cpu/sp 0xffff))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :INX-SP))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0 (:cpu/sp new-st)))))

(deftest jmp-test
  (let [ini-st (write-bytes (cpu) 0 [0xc3 0x05 0x00 0xff 0xff 0xff])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :JMP))
    (is (= 5 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest ldax-d-test
  (let [ini-st (-> (cpu)
                   (write-byte 0 0x1a)
                   (write-byte 0x938b 0x42)
                   (assoc :cpu/d 0x93 :cpu/e 0x8b))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LDAX-D))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x42 (:cpu/a new-st)))))

(deftest lhld-test
  (let [ini-st (write-bytes (cpu) 0 [0x2a 0x03 0x00 0xff 0x03])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LHLD))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x03 (:cpu/h new-st)))
    (is (= 0xff (:cpu/l new-st)))))

(deftest lxi-d-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0x11 0x01 0x02]) (assoc :cpu/e 3))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-D))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x01 (:cpu/e new-st)))
    (is (= 0x02 (:cpu/d new-st)))))

(deftest lxi-h-test
  (let [ini-st (write-bytes (cpu) 0 [0x21 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-H))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x01 (:cpu/l new-st)))
    (is (= 0x02 (:cpu/h new-st)))))

(deftest lxi-sp-test
  (let [ini-st (write-bytes (cpu) 0 [0x31 0x01 0x02])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :LXI-SP))
    (is (= 3 (:cpu/pc new-st)))
    ; 0x02 (hi) 0x01 (lo) == 513
    (is (= 513 (:cpu/sp new-st)))))

(deftest mov-b-b-test
  (let [ini-st (write-byte (cpu) 0 0x40)
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest mov-b-c-test
  (let [ini-st (-> (cpu) (write-byte 0 0x41) (assoc :cpu/c 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-B-C))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (:cpu/c ini-st) (:cpu/b new-st)))))

(deftest mov-m-a-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0x77 0x00 0x00 0x00]) (assoc :cpu/a 0x42 :cpu/h 0x00 :cpu/l 0x03))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MOV-M-A))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x42 (get-in new-st [:cpu/mem 0x03])))))

(deftest mvi-a-test
  (let [ini-st (write-bytes (cpu) 0 [0x3e 0x42])
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :MVI-A))
    (is (= 2 (:cpu/pc new-st)))
    (is (= (:cpu/a new-st) 0x42))))

(deftest nop-test
  (let [ini-st (write-byte (cpu) 0 0)
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :NOP))
    (is (= 1 (:cpu/pc new-st)))
    (is (= (dissoc ini-st :cpu/pc) (dissoc new-st :cpu/pc)))))

(deftest ora-c-test
  (let [ini-st (-> (cpu) (write-byte 0 0xb1) (assoc :cpu/a 0x33 :cpu/c 0xf))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x3f (:cpu/a new-st)))))

(deftest pchl-test
  (let [ini-st (-> (cpu) (write-byte 0 0xe9) (assoc :cpu/h 0x41 :cpu/l 0x3e))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= :PCHL (:op op)))
    (is (= 0x413e (:cpu/pc new-st)))))

(deftest pop-b-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0xc1 0x00 0x11 0x22]) (assoc :cpu/sp 0x02))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :POP-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 4 (:cpu/sp new-st)))
    (is (= 0x22 (:cpu/b new-st)))
    (is (= 0x11 (:cpu/c new-st)))))

(deftest pop-psw-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0xf1 2r00011111 0x42]) (assoc :cpu/sp 0x01))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :POP-PSW))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 3 (:cpu/sp new-st)))
    (is (= 0 (:cpu/cc/z ini-st)))
    (is (= 0 (:cpu/cc/s ini-st)))
    (is (= 0 (:cpu/cc/p ini-st)))
    (is (= 0 (:cpu/cc/cy ini-st)))
    (is (= 0 (:cpu/cc/ac ini-st)))
    (is (= 1 (:cpu/cc/z new-st)))
    (is (= 1 (:cpu/cc/s new-st)))
    (is (= 1 (:cpu/cc/p new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))
    (is (= 1 (:cpu/cc/ac new-st)))
    (is (= 0x42 (:cpu/a new-st)))))

(deftest push-b-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0xc5 0x00 0x00 0x00]) (assoc :cpu/b 0x11 :cpu/c 0x22 :cpu/sp 0x03))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :PUSH-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/sp new-st)))
    (is (= 0x22 (get-byte new-st (:cpu/sp new-st))))
    (is (= 0x11 (get-byte new-st (inc (:cpu/sp new-st)))))))

(deftest push-psw-test
  (let [ini-st (-> (cpu)
                   (write-bytes 0 [0xf5 0x00 0x00 0x00])
                   (assoc :cpu/a 0x11 :cpu/sp 3 :cpu/cc/z 1 :cpu/cc/s 1 :cpu/cc/p 1 :cpu/cc/cy 1 :cpu/cc/ac 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :PUSH-PSW))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 1 (:cpu/sp new-st)))
    (is (= 2r00011111 (get-in new-st [:cpu/mem 1])))
    (is (= 0x11 (get-in new-st [:cpu/mem 2])))))

(deftest ral-test
  (let [ini-st (-> (cpu) (write-byte 0 0x17) (assoc :cpu/a 0xb5))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAL))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x6a (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest rar-test
  (let [ini-st (-> (cpu) (write-byte 0 0x1f) (assoc :cpu/a 0x6a :cpu/cc/cy 1))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RAR))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xb5 (:cpu/a new-st)))
    (is (= 0 (:cpu/cc/cy new-st)))))

(deftest rlc-test
  (let [ini-st (-> (cpu) (write-byte 0 0x07) (assoc :cpu/a 0xf2))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RLC))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0xe5 (:cpu/a new-st)))
    (is (= 1 (:cpu/cc/cy new-st)))))

(deftest rrc-test
  (let [ini-st (-> (cpu) (write-byte 0 0x0f) (assoc :cpu/a 0xf2))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :RRC))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x79 (:cpu/a new-st)))
    (is (= 0 (:cpu/cc/cy new-st)))))

(deftest sbb-test
  (doseq [[opcode reg] (map vector [0x98 0x99 0x9a 0x9b 0x9c 0x9d] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> (cpu)
                     (write-byte 0 opcode)
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
  (let [ini-st (-> (cpu) (write-bytes 0 [0x22 0x03 0x00 0x00 0x00]) (assoc :cpu/h 0xae :cpu/l 0x29))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :SHLD))
    (is (= 3 (:cpu/pc new-st)))
    (is (= 0x29 (get-byte new-st 3)))
    (is (= 0xae (get-byte new-st 4)))))

(deftest stax-b-test
  (let [ini-st (-> (cpu) (write-byte 0 0x02) (assoc :cpu/a 0x11 :cpu/b 0x3f :cpu/c 0x16))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :STAX-B))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x11 (get-byte new-st 0x3f16)))))

(deftest sub-test
  (doseq [[opcode reg] (map vector [0x90 0x91 0x92 0x93 0x94 0x95] [:cpu/b :cpu/c :cpu/d :cpu/e :cpu/h :cpu/l])]
    (let [ini-st (-> (cpu) (write-byte 0 opcode) (assoc :cpu/a 0x3e reg 0x3e))
          op (disassemble-op ini-st)
          new-st (execute-op ini-st op)]
      (is (= 1 (:cpu/pc new-st)))
      (is (= 0 (:cpu/a new-st)) (str "sub :cpu/a and " reg))
      (is (= 1 (:cpu/cc/z new-st)))
      (is (= 0 (:cpu/cc/s new-st)))
      (is (= 1 (:cpu/cc/p new-st)))
      (is (= 0 (:cpu/cc/cy new-st))))))

(deftest xchg-test
  (let [ini-st (-> (cpu) (write-byte 0 0xeb) (assoc :cpu/h 0 :cpu/l 0xff :cpu/d 0x33 :cpu/e 0x55))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0 (:cpu/d new-st)))
    (is (= 0xff (:cpu/e new-st)))
    (is (= 0x33 (:cpu/h new-st)))
    (is (= 0x55 (:cpu/l new-st)))))

(deftest xthl-test
  (let [ini-st (-> (cpu) (write-bytes 0 [0xe3 0xf0 0x0d]) (assoc :cpu/h 0x0b :cpu/l 0x3c :cpu/sp 0x01))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= (:op op) :XTHL))
    (is (= 1 (:cpu/pc new-st)))
    (is (= 0x3c (get-byte new-st (:cpu/sp new-st))))
    (is (= 0x0b (get-byte new-st (inc (:cpu/sp new-st)))))
    (is (= 0x0d (:cpu/h new-st)))
    (is (= 0xf0 (:cpu/l new-st)))))

(deftest xra-b-test
  (let [ini-st (-> (cpu) (write-byte 0 0xa8) (assoc :cpu/a 0xff :cpu/b 2r10101010))
        op (disassemble-op ini-st)
        new-st (execute-op ini-st op)]
    (is (= 1 (:cpu/pc new-st)))
    (is (= 2r01010101 (:cpu/a new-st)))))
