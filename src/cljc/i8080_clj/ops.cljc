(ns i8080-clj.ops
  (:require [i8080-clj.opfns :as f]))

(def ops
  {0x00 {:op :NOP, :size 1, :cycles 4, :f identity}
   0x01 {:op :LXI-B, :size 3, :cycles 10, :f (partial f/lxi :cpu/b :cpu/c)}
   0x02 {:op :STAX-B, :size 1, :cycles 7, :f (partial f/stax :cpu/b :cpu/c)}
   ; NB changed cycles to match js8080
   0x03 {:op :INX-B, :size 1, :cycles 6, :f (partial f/inx :cpu/b :cpu/c)}
   0x04 {:op :INR-B, :size 1, :cycles 5, :f (partial f/inr :cpu/b)}
   0x05 {:op :DCR-B, :size 1, :cycles 5, :f (partial f/dcr :cpu/b)}
   0x06 {:op :MVI-B, :size 2, :cycles 7, :f (partial f/mvi :cpu/b)}
   0x07 {:op :RLC, :size 1, :cycles 4, :f f/rlc}
   0x08 {:op nil, :size 1, :cycles 4, :f identity}
   ; NB changed cycles to match js8080
   0x09 {:op :DAD-B, :size 1, :cycles 11, :f f/dad-b}
   0x0a {:op :LDAX-B, :size 1, :cycles 7, :f (partial f/ldax :cpu/b :cpu/c)}
   ; NB changed cycles to match js8080
   0x0b {:op :DCX-B, :size 1, :cycles 6, :f (partial f/dcx :cpu/b :cpu/c)}
   0x0c {:op :INR-C, :size 1, :cycles 5, :f (partial f/inr :cpu/c)}
   0x0d {:op :DCR-C, :size 1, :cycles 5, :f (partial f/dcr :cpu/c)}
   0x0e {:op :MVI-C, :size 2, :cycles 7, :f (partial f/mvi :cpu/c)}
   0x0f {:op :RRC, :size 1, :cycles 4, :f f/rrc}
   0x10 {:op nil, :size 1, :cycles 4, :f identity}
   0x11 {:op :LXI-D, :size 3, :cycles 10, :f (partial f/lxi :cpu/d :cpu/e)}
   0x12 {:op :STAX-D, :size 1, :cycles 7, :f (partial f/stax :cpu/d :cpu/e)}
   ; NB changed cycles to match js8080
   0x13 {:op :INX-D, :size 1, :cycles 6, :f (partial f/inx :cpu/d :cpu/e)}
   0x14 {:op :INR-D, :size 1, :cycles 5, :f (partial f/inr :cpu/d)}
   0x15 {:op :DCR-D, :size 1, :cycles 5, :f (partial f/dcr :cpu/d)}
   0x16 {:op :MVI-D, :size 2, :cycles 7, :f (partial f/mvi :cpu/d)}
   0x17 {:op :RAL, :size 1, :cycles 4, :f f/ral}
   0x18 {:op nil, :size 1, :cycles 4, :f identity}
   ; NB changed cycles to match js8080
   0x19 {:op :DAD-D, :size 1, :cycles 11, :f f/dad-d}
   0x1a {:op :LDAX-D, :size 1, :cycles 7, :f (partial f/ldax :cpu/d :cpu/e)}
   ; NB changed cycles to match js8080
   0x1b {:op :DCX-D, :size 1, :cycles 6, :f (partial f/dcx :cpu/d :cpu/e)}
   0x1c {:op :INR-E, :size 1, :cycles 5, :f (partial f/inr :cpu/e)}
   0x1d {:op :DCR-E, :size 1, :cycles 5, :f (partial f/dcr :cpu/d)}
   0x1e {:op :MVI-E, :size 2, :cycles 7, :f (partial f/mvi :cpu/e)}
   0x1f {:op :RAR, :size 1, :cycles 4, :f f/rar}
   0x20 {:op :NOP, :size 1, :cycles 4, :f identity}
   0x21 {:op :LXI-H, :size 3, :cycles 10, :f (partial f/lxi :cpu/h :cpu/l)}
   0x22 {:op :SHLD, :size 3, :cycles 16, :f f/shld}
   ; NB changed cycles to match js8080
   0x23 {:op :INX-H, :size 1, :cycles 6, :f (partial f/inx :cpu/h :cpu/l)}
   0x24 {:op :INR-H, :size 1, :cycles 5, :f (partial f/inr :cpu/h)}
   0x25 {:op :DCR-H, :size 1, :cycles 5}
   0x26 {:op :MVI-H, :size 2, :cycles 7, :f (partial f/mvi :cpu/h)}
   0x27 {:op :DAA, :size 1, :cycles 4, :f f/daa}
   0x28 {:op nil, :size 1, :cycles 4, :f identity}
   ; NB changed cycles to match js8080
   0x29 {:op :DAD-H, :size 1, :cycles 11, :f f/dad-h}
   0x2a {:op :LHLD, :size 3, :cycles 16, :f f/lhld}
   0x2b {:op :DCX-H, :size 1, :cycles 6, :f (partial f/dcx :cpu/h :cpu/l)}
   0x2c {:op :INR-L, :size 1, :cycles 5, :f (partial f/inr :cpu/l)}
   0x2d {:op :DCR-L, :size 1, :cycles 5}
   0x2e {:op :MVI-L, :size 2, :cycles 7, :f (partial f/mvi :cpu/l)}
   0x2f {:op :CMA, :size 1, :cycles 4, :f f/cma}
   0x30 {:op :SIM, :size 1, :cycles 4, :f identity}
   0x31 {:op :LXI-SP, :size 3, :cycles 10, :f f/lxi-sp}
   0x32 {:op :STA, :size 3, :cycles 13, :f f/sta}
   ; NB changed cycles to match js8080
   0x33 {:op :INX-SP, :size 1, :cycles 6, :f f/inx-sp}
   0x34 {:op :INR-M, :size 1, :cycles 10, :f f/inr-m}
   0x35 {:op :DCR-M, :size 1, :cycles 10, :f f/dcr-m}
   0x36 {:op :MVI-M, :size 2, :cycles 10, :f f/byte-to-hl}
   0x37 {:op :STC, :size 1, :cycles 4, :f (fn [state] (assoc state :cpu/cc/cy 1))}
   0x38 {:op nil, :size 1, :cycles 4, :f identity}
   ; NB changed cycles to match js8080
   0x39 {:op :DAD-SP, :size 1, :cycles 11, :f f/dad-sp}
   0x3a {:op :LDA, :size 3, :cycles 13, :f f/lda}
   ; NB changed cycles to match js8080
   0x3b {:op :DCX-SP, :size 1, :cycles 6, :f f/dcx-sp}
   0x3c {:op :INR-A, :size 1, :cycles 5, :f (partial f/inr :cpu/a)}
   0x3d {:op :DCR-A, :size 1, :cycles 5, :f (partial f/dcr :cpu/a)}
   0x3e {:op :MVI-A, :size 2, :cycles 7, :f (partial f/mvi :cpu/a)}
   0x3f {:op :CMC, :size 1, :cycles 4, :f (fn [state] (assoc state :cpu/cc/cy 0))}
   0x40 {:op :MOV-B-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/b)}
   0x41 {:op :MOV-B-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/b)}
   0x42 {:op :MOV-B-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/b)}
   0x43 {:op :MOV-B-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/b)}
   0x44 {:op :MOV-B-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/b)}
   0x45 {:op :MOV-B-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/b)}
   0x46 {:op :MOV-B-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/b)}
   0x47 {:op :MOV-B-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/b)}
   0x48 {:op :MOV-C-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/c)}
   0x49 {:op :MOV-C-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/c)}
   0x4a {:op :MOV-C-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/c)}
   0x4b {:op :MOV-C-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/c)}
   0x4c {:op :MOV-C-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/c)}
   0x4d {:op :MOV-C-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/c)}
   0x4e {:op :MOV-C-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/c)}
   0x4f {:op :MOV-C-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/c)}
   0x50 {:op :MOV-D-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/d)}
   0x51 {:op :MOV-D-C, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/d)}
   0x52 {:op :MOV-D-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/d)}
   0x53 {:op :MOV-D-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/d)}
   0x54 {:op :MOV-D-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/d)}
   0x55 {:op :MOV-D-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/d)}
   0x56 {:op :MOV-D-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/d)}
   0x57 {:op :MOV-D-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/d)}
   0x58 {:op :MOV-E-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/e)}
   0x59 {:op :MOV-E-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/e)}
   0x5a {:op :MOV-E-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/e)}
   0x5b {:op :MOV-E-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/e)}
   0x5c {:op :MOV-E-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/e)}
   0x5d {:op :MOV-E-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/e)}
   0x5e {:op :MOV-E-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/e)}
   0x5f {:op :MOV-E-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/e)}
   0x60 {:op :MOV-H-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/h)}
   0x61 {:op :MOV-H-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/h)}
   0x62 {:op :MOV-H-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/h)}
   0x63 {:op :MOV-H-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/h)}
   0x64 {:op :MOV-H-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/h)}
   0x65 {:op :MOV-H-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/h)}
   0x66 {:op :MOV-H-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/h)}
   0x67 {:op :MOV-H-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/h)}
   0x68 {:op :MOV-L-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/l)}
   0x69 {:op :MOV-L-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/l)}
   0x6a {:op :MOV-L-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/l)}
   0x6b {:op :MOV-L-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/l)}
   0x6c {:op :MOV-L-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/l)}
   0x6d {:op :MOV-L-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/l)}
   0x6e {:op :MOV-L-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/l)}
   0x6f {:op :MOV-L-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/l)}
   0x70 {:op :MOV-M-B, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/b)}
   0x71 {:op :MOV-M-C, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/c)}
   0x72 {:op :MOV-M-D, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/d)}
   0x73 {:op :MOV-M-E, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/e)}
   0x74 {:op :MOV-M-H, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/h)}
   0x75 {:op :MOV-M-L, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/l)}
   0x76 {:op :HLT, :size 1, :cycles 7}
   0x77 {:op :MOV-M-A, :size 1, :cycles 7, :f (partial f/mov-to-m :cpu/a)}
   0x78 {:op :MOV-A-B, :size 1, :cycles 5, :f (partial f/mov :cpu/b :cpu/a)}
   0x79 {:op :MOV-A-C, :size 1, :cycles 5, :f (partial f/mov :cpu/c :cpu/a)}
   0x7a {:op :MOV-A-D, :size 1, :cycles 5, :f (partial f/mov :cpu/d :cpu/a)}
   0x7b {:op :MOV-A-E, :size 1, :cycles 5, :f (partial f/mov :cpu/e :cpu/a)}
   0x7c {:op :MOV-A-H, :size 1, :cycles 5, :f (partial f/mov :cpu/h :cpu/a)}
   0x7d {:op :MOV-A-L, :size 1, :cycles 5, :f (partial f/mov :cpu/l :cpu/a)}
   0x7e {:op :MOV-A-M, :size 1, :cycles 7, :f (partial f/mov-from-m :cpu/a)}
   0x7f {:op :MOV-A-A, :size 1, :cycles 5, :f (partial f/mov :cpu/a :cpu/a)}

   ; add registers
   0x80 {:op :ADD-B, :size 1, :cycles 4, :f (partial f/add :cpu/b)}
   0x81 {:op :ADD-C, :size 1, :cycles 4, :f (partial f/add :cpu/c)}
   0x82 {:op :ADD-D, :size 1, :cycles 4, :f (partial f/add :cpu/d)}
   0x83 {:op :ADD-E, :size 1, :cycles 4, :f (partial f/add :cpu/e)}
   0x84 {:op :ADD-H, :size 1, :cycles 4, :f (partial f/add :cpu/h)}
   0x85 {:op :ADD-L, :size 1, :cycles 4, :f (partial f/add :cpu/l)}
   0x86 {:op :ADD-M, :size 1, :cycles 7, :f f/add-m}
   0x87 {:op :ADD-A, :size 1, :cycles 4, :f (partial f/add :cpu/a)}

   ; add registers with carry
   0x88 {:op :ADC-B, :size 1, :cycles 4, :f (partial f/adc :cpu/b)}
   0x89 {:op :ADC-C, :size 1, :cycles 4, :f (partial f/adc :cpu/c)}
   0x8a {:op :ADC-D, :size 1, :cycles 4, :f (partial f/adc :cpu/d)}
   0x8b {:op :ADC-E, :size 1, :cycles 4, :f (partial f/adc :cpu/e)}
   0x8c {:op :ADC-H, :size 1, :cycles 4, :f (partial f/adc :cpu/h)}
   0x8d {:op :ADC-L, :size 1, :cycles 4, :f (partial f/adc :cpu/l)}
   0x8e {:op :ADC-M, :size 1, :cycles 7, :f f/adc-m}
   0x8f {:op :ADC-A, :size 1, :cycles 4, :f (partial f/adc :cpu/a)}

   ; subtract registers
   0x90 {:op :SUB-B, :size 1, :cycles 4, :f (partial f/sub :cpu/b)}
   0x91 {:op :SUB-C, :size 1, :cycles 4, :f (partial f/sub :cpu/c)}
   0x92 {:op :SUB-D, :size 1, :cycles 4, :f (partial f/sub :cpu/d)}
   0x93 {:op :SUB-E, :size 1, :cycles 4, :f (partial f/sub :cpu/e)}
   0x94 {:op :SUB-H, :size 1, :cycles 4, :f (partial f/sub :cpu/h)}
   0x95 {:op :SUB-L, :size 1, :cycles 4, :f (partial f/sub :cpu/l)}
   0x96 {:op :SUB-M, :size 1, :cycles 7, :f f/sub-m}
   0x97 {:op :SUB-A, :size 1, :cycles 4, :f (partial f/sub :cpu/a)}

   ; subtract registers with borrow
   0x98 {:op :SBB-B, :size 1, :cycles 4, :f (partial f/sbb :cpu/b)}
   0x99 {:op :SBB-C, :size 1, :cycles 4, :f (partial f/sbb :cpu/c)}
   0x9a {:op :SBB-D, :size 1, :cycles 4, :f (partial f/sbb :cpu/d)}
   0x9b {:op :SBB-E, :size 1, :cycles 4, :f (partial f/sbb :cpu/e)}
   0x9c {:op :SBB-H, :size 1, :cycles 4, :f (partial f/sbb :cpu/h)}
   0x9d {:op :SBB-L, :size 1, :cycles 4, :f (partial f/sbb :cpu/l)}
   0x9e {:op :SBB-M, :size 1, :cycles 7, :f f/sbb-m}
   0x9f {:op :SBB-A, :size 1, :cycles 4, :f (partial f/sbb :cpu/a)}

   ; boolean and
   0xa0 {:op :ANA-B, :size 1, :cycles 4, :f (partial f/ana :cpu/b)}
   0xa1 {:op :ANA-C, :size 1, :cycles 4, :f (partial f/ana :cpu/c)}
   0xa2 {:op :ANA-D, :size 1, :cycles 4, :f (partial f/ana :cpu/d)}
   0xa3 {:op :ANA-E, :size 1, :cycles 4, :f (partial f/ana :cpu/e)}
   0xa4 {:op :ANA-H, :size 1, :cycles 4, :f (partial f/ana :cpu/h)}
   0xa5 {:op :ANA-L, :size 1, :cycles 4, :f (partial f/ana :cpu/l)}
   0xa6 {:op :ANA-M, :size 1, :cycles 7, :f f/ana-m}
   0xa7 {:op :ANA-A, :size 1, :cycles 4, :f (partial f/ana :cpu/a)}

   ; boolean xor
   0xa8 {:op :XRA-B, :size 1, :cycles 4, :f (partial f/xra :cpu/b)}
   0xa9 {:op :XRA-C, :size 1, :cycles 4, :f (partial f/xra :cpu/c)}
   0xaa {:op :XRA-D, :size 1, :cycles 4, :f (partial f/xra :cpu/d)}
   0xab {:op :XRA-E, :size 1, :cycles 4, :f (partial f/xra :cpu/e)}
   0xac {:op :XRA-H, :size 1, :cycles 4, :f (partial f/xra :cpu/h)}
   0xad {:op :XRA-L, :size 1, :cycles 4, :f (partial f/xra :cpu/l)}
   0xae {:op :XRA-M, :size 1, :cycles 7, :f f/xra-m}
   0xaf {:op :XRA-A, :size 1, :cycles 4, :f (partial f/xra :cpu/a)}

   ; boolean or
   0xb0 {:op :ORA-B, :size 1, :cycles 4, :f (partial f/ora :cpu/b)}
   0xb1 {:op :ORA-C, :size 1, :cycles 4, :f (partial f/ora :cpu/c)}
   0xb2 {:op :ORA-D, :size 1, :cycles 4, :f (partial f/ora :cpu/d)}
   0xb3 {:op :ORA-E, :size 1, :cycles 4, :f (partial f/ora :cpu/e)}
   0xb4 {:op :ORA-H, :size 1, :cycles 4, :f (partial f/ora :cpu/h)}
   0xb5 {:op :ORA-L, :size 1, :cycles 4, :f (partial f/ora :cpu/l)}
   0xb6 {:op :ORA-M, :size 1, :cycles 7, :f f/ora-m}
   0xb7 {:op :ORA-A, :size 1, :cycles 4, :f (partial f/ora :cpu/a)}

   ; compare
   0xb8 {:op :CMP-B, :size 1, :cycles 4, :f (partial f/cmp :cpu/b)}
   0xb9 {:op :CMP-C, :size 1, :cycles 4, :f (partial f/cmp :cpu/c)}
   0xba {:op :CMP-D, :size 1, :cycles 4, :f (partial f/cmp :cpu/d)}
   0xbb {:op :CMP-E, :size 1, :cycles 4, :f (partial f/cmp :cpu/e)}
   0xbc {:op :CMP-H, :size 1, :cycles 4, :f (partial f/cmp :cpu/h)}
   0xbd {:op :CMP-L, :size 1, :cycles 4, :f (partial f/cmp :cpu/l)}
   0xbe {:op :CMP-M, :size 1, :cycles 7, :f f/cmp-m}
   0xbf {:op :CMP-A, :size 1, :cycles 4, :f (partial f/cmp :cpu/a)}

   ; return if not zero
   0xc0 {:op :RNZ, :size 1, :cycles 11, :f (partial f/ret (comp zero? :cpu/cc/z))}
   0xc1 {:op :POP-B, :size 1, :cycles 10, :f f/pop-b}
   ; jump if not zero
   0xc2 {:op :JNZ, :size 3, :cycles 10, :f (partial f/jmp (comp zero? :cpu/cc/z))}
   ; jump
   0xc3 {:op :JMP, :size 3, :cycles 10, :f (partial f/jmp (constantly true))}
   ; call if not zero
   0xc4 {:op :CNZ, :size 3, :cycles 17, :f (partial f/call (comp zero? :cpu/cc/z))}
   0xc5 {:op :PUSH-B, :size 1, :cycles 11, :f f/push-b}
   ; add immediate
   0xc6 {:op :ADI, :size 2, :cycles 7, :f f/adi}
   0xc7 {:op :RST-0, :size 1, :cycles 11}
   ; return if zero
   0xc8 {:op :RZ, :size 1, :cycles 11, :f (partial f/ret (comp pos? :cpu/cc/z))}
   ; return
   0xc9 {:op :RET, :size 1, :cycles 10, :f (partial f/ret identity)}
   ; jump if zero
   0xca {:op :JZ, :size 3, :cycles 10, :f (partial f/jmp (comp pos? :cpu/cc/z))}
   0xcb {:op nil, :size 1, :cycles 10}
   ; call if zero
   0xcc {:op :CZ, :size 3, :cycles 10, :f (partial f/call (comp pos? :cpu/cc/z))}
   ; call
   0xcd {:op :CALL, :size 3, :cycles 17, :f (partial f/call identity)}
   ; add immediate with carry
   0xce {:op :ACI, :size 2, :cycles 7, :f f/aci}
   0xcf {:op :RST-1, :size 1, :cycles 11}
   ; return if not carry
   0xd0 {:op :RNC, :size 1, :cycles 11, :f (partial f/ret (comp zero? :cpu/cc/cy))}
   0xd1 {:op :POP-D, :size 1, :cycles 10, :f f/pop-d}
   ; jump if not carry
   0xd2 {:op :JNC, :size 3, :cycles 10, :f (partial f/jmp (comp zero? :cpu/cc/cy))}
   0xd3 {:op :OUT, :size 2, :cycles 10, :f (fn [state _] state)}
   ; call if not carry
   0xd4 {:op :CNC, :size 3, :cycles 17, :f (partial f/call (comp zero? :cpu/cc/cy))}
   0xd5 {:op :PUSH-D, :size 1, :cycles 11, :f f/push-d}
   ; subtract immediate with carry
   0xd6 {:op :SUI, :size 2, :cycles 7, :f f/sui}
   0xd7 {:op :RST-2, :size 1, :cycles 11}
   ; return if carry
   0xd8 {:op :RC, :size 1, :cycles 11, :f (partial f/ret (comp pos? :cpu/cc/cy))}
   0xd9 {:op nil, :size 1, :cycles 10}
   ; jump if carry
   0xda {:op :JC, :size 3, :cycles 10, :f (partial f/jmp (comp pos? :cpu/cc/cy))}
   0xdb {:op :IN, :size 2, :cycles 10, :f (fn [state _] state)}
   ; call if carry
   0xdc {:op :CC, :size 3, :cycles 10, :f (partial f/call (comp pos? :cpu/cc/cy))}
   0xdd {:op nil, :size 1, :cycles 17}
   ; subtract immediate
   0xde {:op :SBI, :size 2, :cycles 7, :f f/sbi}
   0xdf {:op :RST-3, :size 1, :cycles 11}
   ; return if odd parity
   0xe0 {:op :RPO, :size 1, :cycles 11, :f (partial f/ret (comp zero? :cpu/cc/p))}
   0xe1 {:op :POP-H, :size 1, :cycles 10, :f f/pop-h}
   ; jump if odd parity
   0xe2 {:op :JPO, :size 3, :cycles 10, :f (partial f/jmp (comp zero? :cpu/cc/p))}
   0xe3 {:op :XTHL, :size 1, :cycles 18, :f f/xthl}
   ; call if odd parity
   0xe4 {:op :CPO, :size 3, :cycles 17, :f (partial f/call (comp zero? :cpu/cc/p))}
   0xe5 {:op :PUSH-H, :size 1, :cycles 11, :f f/push-h}
   0xe6 {:op :ANI, :size 2, :cycles 7, :f f/ani}
   0xe7 {:op :RST-4, :size 1, :cycles 11}
   ; return if even parity
   0xe8 {:op :RPE, :size 1, :cycles 11, :f (partial f/ret (comp pos? :cpu/cc/p))}
   0xe9 {:op :PCHL, :size 1, :cycles 5, :f f/pchl}
   ; jump if even parity
   0xea {:op :JPE, :size 3, :cycles 10, :f (partial f/jmp (comp pos? :cpu/cc/p))}
   0xeb {:op :XCHG, :size 1, :cycles 5, :f f/xchg}
   ; call if even parity
   0xec {:op :CPE, :size 3, :cycles 17, :f (partial f/call (comp pos? :cpu/cc/p))}
   0xed {:op nil, :size 1, :cycles 17}
   0xee {:op :XRI, :size 2, :cycles 7, :f f/xri}
   0xef {:op :RST-5, :size 1, :cycles 11}
   ; return if sign positive
   0xf0 {:op :RP, :size 1, :cycles 11, :f (partial f/ret (comp zero? :cpu/cc/s))}
   0xf1 {:op :POP-PSW, :size 1, :cycles 10, :f f/pop-psw}
   ; jump if sign positive
   0xf2 {:op :JP, :size 3, :cycles 10, :f (partial f/jmp (comp zero? :cpu/cc/s))}
   ; disable interrupt
   0xf3 {:op :DI, :size 1, :cycles 4, :f (fn [state] (assoc state :cpu/int-enable? false))}
   ; call if sign positive
   0xf4 {:op :CP, :size 3, :cycles 17, :f (partial f/call (comp zero? :cpu/cc/s))}
   0xf5 {:op :PUSH-PSW, :size 1, :cycles 11, :f f/push-psw}
   0xf6 {:op :ORI, :size 2, :cycles 7, :f f/ori}
   0xf7 {:op :RST-6, :size 1, :cycles 11}
   ; return if sign negative
   0xf8 {:op :RM, :size 1, :cycles 11, :f (partial f/ret (comp pos? :cpu/cc/s))}
   0xf9 {:op :SPHL, :size 1, :cycles 5}
   ; jump if sign negative
   0xfa {:op :JM, :size 3, :cycles 10, :f (partial f/jmp (comp pos? :cpu/cc/s))}
   ; enable interrupt
   0xfb {:op :EI, :size 1, :cycles 4, :f (fn [state] (assoc state :cpu/int-enable? true))}
   ; call if sign negative
   0xfc {:op :CM, :size 3, :cycles 17, :f (partial f/call (comp pos? :cpu/cc/s))}
   0xfd {:op nil, :size 1, :cycles 17}
   0xfe {:op :CPI, :size 2, :cycles 7, :f f/cpi}
   0xff {:op :RST-7, :size 1, :cycles 11}})


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
