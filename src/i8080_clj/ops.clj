(ns i8080-clj.ops
  (:use [i8080-clj.opfns]))

(def advance-pc?
  (comp not
        #{:JMP :JNZ :JZ :JNC :JC :JPO :JPE :JP :JM
          :CALL :CZ :CNZ :RZ :RNZ :CNC :CC :RNC :RC :CPO :CPE :RPO :RPE :CP :CM :RP :RM
          :RST
          :PCHL}))

(def ops
  {0x00 {:op :NOP, :size 1, :f identity}
   0x01 {:op :LXI-B, :size 3, :f (partial lxi :b :c)}
   0x02 {:op :STAX-B, :size 1}
   0x03 {:op :INX-B, :size 1}
   0x04 {:op :INR-B, :size 1}
   0x05 {:op :DCR-B, :size 1}
   0x06 {:op :MVI-B, :size 2, :f (partial mvi :b)}
   0x07 {:op :RLC, :size 1}
   0x08 {:op nil, :size 1}
   0x09 {:op :DAD-B, :size 1}
   0x0a {:op :LDAX-B, :size 1}
   0x0b {:op :DCX-B, :size 1}
   0x0c {:op :INR-C, :size 1}
   0x0d {:op :DCR-C, :size 1}
   0x0e {:op :MVI-C, :size 2, :f (partial mvi :c)}
   0x0f {:op :RRC, :size 1}
   0x10 {:op nil, :size 1}
   0x11 {:op :LXI-D, :size 3, :f (partial lxi :d :e)}
   0x12 {:op :STAX-D, :size 1}
   0x13 {:op :INX-D, :size 1}
   0x14 {:op :INR-D, :size 1}
   0x15 {:op :DCR-D, :size 1}
   0x16 {:op :MVI-D, :size 2, :f (partial mvi :d)}
   0x17 {:op :RAL, :size 1}
   0x18 {:op nil, :size 1}
   0x19 {:op :DAD-D, :size 1}
   0x1a {:op :LDAX-D, :size 1}
   0x1b {:op :DCX-D, :size 1}
   0x1c {:op :INR-E, :size 1}
   0x1d {:op :DCR-E, :size 1}
   0x1e {:op :MVI-E, :size 2, :f (partial mvi :e)}
   0x1f {:op :RAR, :size 1}
   0x20 {:op :RIM, :size 1}
   0x21 {:op :LXI-H, :size 3, :f (partial lxi :h :l)}
   0x22 {:op :SHLD, :size 3}
   0x23 {:op :INX-H, :size 1}
   0x24 {:op :INR-H, :size 1}
   0x25 {:op :DCR-H, :size 1}
   0x26 {:op :MVI-H, :size 2, :f (partial mvi :h)}
   0x27 {:op :DAA, :size 1}
   0x28 {:op nil, :size 1}
   0x29 {:op :DAD-H, :size 1}
   0x2a {:op :LHLD, :size 3}
   0x2b {:op :DCX-H, :size 1}
   0x2c {:op :INR-L, :size 1}
   0x2d {:op :DCR-L, :size 1}
   0x2e {:op :MVI-L, :size 2, :f (partial mvi :l)}
   0x2f {:op :CMA, :size 1}
   0x30 {:op :SIM, :size 1}
   0x31 {:op :LXI-SP, :size 3, :f (fn [state b1 b2] (assoc state :sp (| (<< b2 8) b1)))}
   0x32 {:op :STA, :size 3}
   0x33 {:op :INX-SP, :size 1}
   0x34 {:op :INR-M, :size 1}
   0x35 {:op :DCR-M, :size 1}
   0x36 {:op :MVI-M, :size 2}
   0x37 {:op :STC, :size 1}
   0x38 {:op nil, :size 1}
   0x39 {:op :DAD-SP, :size 1}
   0x3a {:op :LDA, :size 3}
   0x3b {:op :DCX-SP, :size 1}
   0x3c {:op :INR-A, :size 1}
   0x3d {:op :DCR-A, :size 1}
   0x3e {:op :MVI-A, :size 2, :f (partial mvi :a)}
   0x3f {:op :CMC, :size 1}
   0x40 {:op :MOV-B-B, :size 1, :f (partial mov :b :b)}
   0x41 {:op :MOV-B-C, :size 1, :f (partial mov :c :b)}
   0x42 {:op :MOV-B-D, :size 1, :f (partial mov :d :b)}
   0x43 {:op :MOV-B-E, :size 1, :f (partial mov :e :b)}
   0x44 {:op :MOV-B-H, :size 1, :f (partial mov :h :b)}
   0x45 {:op :MOV-B-L, :size 1, :f (partial mov :l :b)}
   0x46 {:op :MOV-B-M, :size 1}
   0x47 {:op :MOV-B-A, :size 1, :f (partial mov :a :b)}
   0x48 {:op :MOV-C-B, :size 1, :f (partial mov :b :c)}
   0x49 {:op :MOV-C-C, :size 1, :f (partial mov :c :c)}
   0x4a {:op :MOV-C-D, :size 1, :f (partial mov :d :c)}
   0x4b {:op :MOV-C-E, :size 1, :f (partial mov :e :c)}
   0x4c {:op :MOV-C-H, :size 1, :f (partial mov :h :c)}
   0x4d {:op :MOV-C-L, :size 1, :f (partial mov :l :c)}
   0x4e {:op :MOV-C-M, :size 1}
   0x4f {:op :MOV-C-A, :size 1, :f (partial mov :a :c)}
   0x50 {:op :MOV-D-B, :size 1, :f (partial mov :b :d)}
   0x51 {:op :MOV-D-C, :size 1, :f (partial mov :b :d)}
   0x52 {:op :MOV-D-D, :size 1, :f (partial mov :d :d)}
   0x53 {:op :MOV-D-E, :size 1, :f (partial mov :e :d)}
   0x54 {:op :MOV-D-H, :size 1, :f (partial mov :h :d)}
   0x55 {:op :MOV-D-L, :size 1, :f (partial mov :l :d)}
   0x56 {:op :MOV-D-M, :size 1}
   0x57 {:op :MOV-D-A, :size 1, :f (partial mov :a :d)}
   0x58 {:op :MOV-E-B, :size 1, :f (partial mov :b :e)}
   0x59 {:op :MOV-E-C, :size 1, :f (partial mov :c :e)}
   0x5a {:op :MOV-E-D, :size 1, :f (partial mov :d :e)}
   0x5b {:op :MOV-E-E, :size 1, :f (partial mov :e :e)}
   0x5c {:op :MOV-E-H, :size 1, :f (partial mov :h :e)}
   0x5d {:op :MOV-E-L, :size 1, :f (partial mov :l :e)}
   0x5e {:op :MOV-E-M, :size 1}
   0x5f {:op :MOV-E-A, :size 1, :f (partial mov :a :e)}
   0x60 {:op :MOV-H-B, :size 1, :f (partial mov :b :h)}
   0x61 {:op :MOV-H-C, :size 1, :f (partial mov :c :h)}
   0x62 {:op :MOV-H-D, :size 1, :f (partial mov :d :h)}
   0x63 {:op :MOV-H-E, :size 1, :f (partial mov :e :h)}
   0x64 {:op :MOV-H-H, :size 1, :f (partial mov :h :h)}
   0x65 {:op :MOV-H-L, :size 1, :f (partial mov :l :h)}
   0x66 {:op :MOV-H-M, :size 1}
   0x67 {:op :MOV-H-A, :size 1, :f (partial mov :a :h)}
   0x68 {:op :MOV-L-B, :size 1, :f (partial mov :b :l)}
   0x69 {:op :MOV-L-C, :size 1, :f (partial mov :c :l)}
   0x6a {:op :MOV-L-D, :size 1, :f (partial mov :d :l)}
   0x6b {:op :MOV-L-E, :size 1, :f (partial mov :e :l)}
   0x6c {:op :MOV-L-H, :size 1, :f (partial mov :h :l)}
   0x6d {:op :MOV-L-L, :size 1, :f (partial mov :l :l)}
   0x6e {:op :MOV-L-M, :size 1}
   0x6f {:op :MOV-L-A, :size 1, :f (partial mov :a :l)}
   0x70 {:op :MOV-M-B, :size 1}
   0x71 {:op :MOV-M-C, :size 1}
   0x72 {:op :MOV-M-D, :size 1}
   0x73 {:op :MOV-M-E, :size 1}
   0x74 {:op :MOV-M-H, :size 1}
   0x75 {:op :MOV-M-L, :size 1}
   0x76 {:op :HLT, :size 1}
   0x77 {:op :MOV-M-A, :size 1}
   0x78 {:op :MOV-A-B, :size 1, :f (partial mov :b :a)}
   0x79 {:op :MOV-A-C, :size 1, :f (partial mov :c :a)}
   0x7a {:op :MOV-A-D, :size 1, :f (partial mov :d :a)}
   0x7b {:op :MOV-A-E, :size 1, :f (partial mov :e :a)}
   0x7c {:op :MOV-A-H, :size 1, :f (partial mov :h :a)}
   0x7d {:op :MOV-A-L, :size 1, :f (partial mov :l :a)}
   0x7e {:op :MOV-A-M, :size 1}
   0x7f {:op :MOV-A-A, :size 1, :f (partial mov :a :a)}

   ; add registers
   0x80 {:op :ADD-B, :size 1, :f (partial add :b)}
   0x81 {:op :ADD-C, :size 1, :f (partial add :c)}
   0x82 {:op :ADD-D, :size 1, :f (partial add :d)}
   0x83 {:op :ADD-E, :size 1, :f (partial add :e)}
   0x84 {:op :ADD-H, :size 1, :f (partial add :h)}
   0x85 {:op :ADD-L, :size 1, :f (partial add :l)}
   0x86 {:op :ADD-M, :size 1, :f add-m}
   0x87 {:op :ADD-A, :size 1, :f (partial add :a)}

   ; add registers with carry
   0x88 {:op :ADC-B, :size 1, :f (partial adc :b)}
   0x89 {:op :ADC-C, :size 1, :f (partial adc :c)}
   0x8a {:op :ADC-D, :size 1, :f (partial adc :d)}
   0x8b {:op :ADC-E, :size 1, :f (partial adc :e)}
   0x8c {:op :ADC-H, :size 1, :f (partial adc :h)}
   0x8d {:op :ADC-L, :size 1, :f (partial adc :l)}
   0x8e {:op :ADC-M, :size 1, :f adc-m}
   0x8f {:op :ADC-A, :size 1, :f (partial adc :a)}

   ; subtract registers
   0x90 {:op :SUB-B, :size 1, :f (partial sub :b)}
   0x91 {:op :SUB-C, :size 1, :f (partial sub :c)}
   0x92 {:op :SUB-D, :size 1, :f (partial sub :d)}
   0x93 {:op :SUB-E, :size 1, :f (partial sub :e)}
   0x94 {:op :SUB-H, :size 1, :f (partial sub :h)}
   0x95 {:op :SUB-L, :size 1, :f (partial sub :l)}
   0x96 {:op :SUB-M, :size 1, :f sub-m}
   0x97 {:op :SUB-A, :size 1, :f (partial sub :a)}

   ; subtract registers with borrow
   0x98 {:op :SBB-B, :size 1, :f (partial sbb :b)}
   0x99 {:op :SBB-C, :size 1, :f (partial sbb :c)}
   0x9a {:op :SBB-D, :size 1, :f (partial sbb :d)}
   0x9b {:op :SBB-E, :size 1, :f (partial sbb :e)}
   0x9c {:op :SBB-H, :size 1, :f (partial sbb :h)}
   0x9d {:op :SBB-L, :size 1, :f (partial sbb :l)}
   0x9e {:op :SBB-M, :size 1, :f sbb-m}
   0x9f {:op :SBB-A, :size 1, :f (partial sbb :a)}

   ; boolean and
   0xa0 {:op :ANA-B, :size 1, :f (partial ana :b)}
   0xa1 {:op :ANA-C, :size 1, :f (partial ana :c)}
   0xa2 {:op :ANA-D, :size 1, :f (partial ana :d)}
   0xa3 {:op :ANA-E, :size 1, :f (partial ana :e)}
   0xa4 {:op :ANA-H, :size 1, :f (partial ana :h)}
   0xa5 {:op :ANA-L, :size 1, :f (partial ana :l)}
   0xa6 {:op :ANA-M, :size 1, :f ana-m}
   0xa7 {:op :ANA-A, :size 1, :f (partial ana :a)}

   ; boolean xor
   0xa8 {:op :XRA-B, :size 1, :f (partial xra :b)}
   0xa9 {:op :XRA-C, :size 1, :f (partial xra :c)}
   0xaa {:op :XRA-D, :size 1, :f (partial xra :d)}
   0xab {:op :XRA-E, :size 1, :f (partial xra :e)}
   0xac {:op :XRA-H, :size 1, :f (partial xra :h)}
   0xad {:op :XRA-L, :size 1, :f (partial xra :l)}
   0xae {:op :XRA-M, :size 1, :f xra-m}
   0xaf {:op :XRA-A, :size 1, :f (partial xra :a)}

   ; boolean or
   0xb0 {:op :ORA-B, :size 1, :f (partial ora :b)}
   0xb1 {:op :ORA-C, :size 1, :f (partial ora :c)}
   0xb2 {:op :ORA-D, :size 1, :f (partial ora :d)}
   0xb3 {:op :ORA-E, :size 1, :f (partial ora :e)}
   0xb4 {:op :ORA-H, :size 1, :f (partial ora :h)}
   0xb5 {:op :ORA-L, :size 1, :f (partial ora :l)}
   0xb6 {:op :ORA-M, :size 1, :f ora-m}
   0xb7 {:op :ORA-A, :size 1, :f (partial ora :a)}

   ; boolean not
   0xb8 {:op :CMP-B, :size 1, :f (partial cmp :b)}
   0xb9 {:op :CMP-C, :size 1, :f (partial cmp :c)}
   0xba {:op :CMP-D, :size 1, :f (partial cmp :d)}
   0xbb {:op :CMP-E, :size 1, :f (partial cmp :e)}
   0xbc {:op :CMP-H, :size 1, :f (partial cmp :h)}
   0xbd {:op :CMP-L, :size 1, :f (partial cmp :l)}
   0xbe {:op :CMP-M, :size 1, :f cmp-m}
   0xbf {:op :CMP-A, :size 1, :f (partial cmp :a)}

   0xc0 {:op :RNZ, :size 1}
   0xc1 {:op :POP-B, :size 1}
   ; jump if not zero
   0xc2 {:op :JNZ, :size 3, :f (partial jmp (comp zero? :z :cc))}
   ; jump
   0xc3 {:op :JMP, :size 3, :f (partial jmp (constantly true))}
   0xc4 {:op :CNZ, :size 3}
   0xc5 {:op :PUSH-B, :size 1}
   ; add immediate
   0xc6 {:op :ADI, :size 2, :f adi}
   0xc7 {:op :RST-0, :size 1}
   0xc8 {:op :RZ, :size 1}
   0xc9 {:op :RET, :size 1}
   ; jump if zero
   0xca {:op :JZ, :size 3, :f (partial jmp (comp pos? :z :cc))}
   0xcb {:op nil, :size 1}
   0xcc {:op :CZ, :size 3}
   0xcd {:op :CALL, :size 3}
   ; add immediate with carry
   0xce {:op :ACI, :size 2, :f aci}
   0xcf {:op :RST-1, :size 1}
   0xd0 {:op :RNC, :size 1}
   0xd1 {:op :POP-D, :size 1}
   ; jump if not carry
   0xd2 {:op :JNC, :size 3, :f (partial jmp (comp zero? :cy :cc))}
   0xd3 {:op :OUT, :size 2}
   0xd4 {:op :CNC, :size 3}
   0xd5 {:op :PUSH-D, :size 1}
   ; subtract immediate with carry
   0xd6 {:op :SUI, :size 2, :f sui}
   0xd7 {:op :RST-2, :size 1}
   0xd8 {:op :RC, :size 1}
   0xd9 {:op nil, :size 1}
   ; jump if carry
   0xda {:op :JC, :size 3, :f (comp pos? :cy :cc)}
   0xdb {:op :IN, :size 2}
   0xdc {:op :CC, :size 3}
   0xdd {:op nil, :size 1}
   ; subtract immediate
   0xde {:op :SBI, :size 2, :f sbi}
   0xdf {:op :RST-3, :size 1}
   0xe0 {:op :RPO, :size 1}
   0xe1 {:op :POP-H, :size 1}
   ; jump if odd parity
   0xe2 {:op :JPO, :size 3, :f (comp zero? :p :cc)}
   0xe3 {:op :XTHL, :size 1}
   0xe4 {:op :CPO, :size 3}
   0xe5 {:op :PUSH-H, :size 1}
   0xe6 {:op :ANI, :size 2}
   0xe7 {:op :RST-4, :size 1}
   0xe8 {:op :RPE, :size 1}
   0xe9 {:op :PCHL, :size 1}
   ; jump if even parity
   0xea {:op :JPE, :size 3, :f (comp pos? :p :cc)}
   0xeb {:op :XCHG, :size 1}
   0xec {:op :CPE, :size 3}
   0xed {:op nil, :size 1}
   0xee {:op :XRI, :size 2}
   0xef {:op :RST-5, :size 1}
   0xf0 {:op :RP, :size 1}
   0xf1 {:op :POP-PSW, :size 1}
   ; jump if sign positive
   0xf2 {:op :JP, :size 3, :f (comp zero? :s :cc)}
   0xf3 {:op :DI, :size 1}
   0xf4 {:op :CP, :size 3}
   0xf5 {:op :PUSH-PSW, :size 1}
   0xf6 {:op :ORI, :size 2}
   0xf7 {:op :RST-6, :size 1}
   0xf8 {:op :RM, :size 1}
   0xf9 {:op :SPHL, :size 1}
   ; jump if sign negative
   0xfa {:op :JM, :size 3, :f (comp pos? :s :cc)}
   0xfb {:op :EI, :size 1}
   0xfc {:op :CM, :size 3}
   0xfd {:op nil, :size 1}
   0xfe {:op :CPI, :size 2}
   0xff {:op :RST-7, :size 1}})


; This generates the above opcode map (needs some post-processing)
;
;(->> (slurp "http://www.emulator101.com/8080-by-opcode.html")
;     hc/parse
;     hc/as-hickory
;     (hs/select (hs/tag :td))
;     (map (comp first :content))
;     (partition 5)
;     (map (fn [[code op size]]
;            [(symbol (string/trim code)) {:op (keyword (string/replace op " " "-")), :size (read-string size)}]))
;     (into (sorted-map)))
