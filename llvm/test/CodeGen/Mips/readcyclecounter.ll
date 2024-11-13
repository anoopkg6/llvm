;RUN: llc -mtriple=mipsel-linux-gnu -mcpu=mips32r2 < %s | FileCheck %s --check-prefix=MIPSEL
;RUN: llc -mtriple=mips64el-linux-gnuabi64 -mcpu=mips64r6 < %s | FileCheck %s --check-prefix=MIPS64EL

define i64 @test_readcyclecounter() nounwind {
; MIPSEL-LABEL: test_readcyclecounter:
; MIPSEL:       # %bb.0: # %entry
; MIPSEL-NEXT:    .set push
; MIPSEL-NEXT:    .set mips32r2
; MIPSEL-NEXT:    rdhwr $3, $hwr_cc
; MIPSEL-NEXT:    .set pop
; MIPSEL-NEXT:    move $2, $3
; MIPSEL-NEXT:    jr $ra
; MIPSEL-NEXT:    addiu $3, $zero, 0
;
; MIPS64EL-LABEL: test_readcyclecounter:
; MIPS64EL:       # %bb.0: # %entry
; MIPS64EL-NEXT:    .set push
; MIPS64EL-NEXT:    .set mips32r2
; MIPS64EL-NEXT:    rdhwr $3, $hwr_cc
; MIPS64EL-NEXT:    .set pop
; MIPS64EL-NEXT:    jr $ra
; MIPS64EL-NEXT:    move $2, $3

entry:
  %tmp0 = call i64 @llvm.readcyclecounter()
  ret i64 %tmp0
}

