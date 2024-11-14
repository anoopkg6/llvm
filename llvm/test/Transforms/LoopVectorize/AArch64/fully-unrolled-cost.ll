; REQUIRES: asserts
; RUN: opt < %s -mcpu=neoverse-v2 -passes=loop-vectorize -debug-only=loop-vectorize -disable-output -S 2>&1 | FileCheck %s

target triple="aarch64--linux-gnu"

define i64 @test(ptr %a, ptr %b) #0 {
; CHECK: LV: Checking a loop in 'test'
; CHECK: LV: Found an estimated cost of 1 for VF 8 For instruction:   %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
; CHECK: LV: Found an estimated cost of 1 for VF 8 For instruction:   %exitcond.not = icmp eq i64 %indvars.iv.next, 16
; CHECK: LV: Vector loop of width 8 costs: 3.
; CHECK-NOT: LV: Found an estimated cost of 1 for VF 16 For instruction:   %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
; CHECK-NOT: LV: Found an estimated cost of 1 for VF 16 For instruction:   %exitcond.not = icmp eq i64 %indvars.iv.next, 16
; CHECK: LV: Vector loop of width 16 costs: 3.
; CHECK: LV: Selecting VF: 16
entry:
  br label %for.body

for.cond.cleanup:                                 ; preds = %for.body
  %add.lcssa = phi i64 [ %add, %for.body ]
  ret i64 %add.lcssa

for.body:                                         ; preds = %entry, %for.body
  %indvars.iv = phi i64 [ 0, %entry ], [ %indvars.iv.next, %for.body ]
  %sum.09 = phi i64 [ 0, %entry ], [ %add, %for.body ]
  %arrayidx = getelementptr inbounds i8, ptr %a, i64 %indvars.iv
  %0 = load i8, ptr %arrayidx, align 1
  %conv = zext i8 %0 to i64
  %arrayidx2 = getelementptr inbounds i8, ptr %b, i64 %indvars.iv
  %1 = load i8, ptr %arrayidx2, align 1
  %conv3 = zext i8 %1 to i64
  %mul = mul nuw nsw i64 %conv3, %conv
  %add = add i64 %mul, %sum.09
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond.not = icmp eq i64 %indvars.iv.next, 16
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

attributes #0 = { vscale_range(1, 16) "target-features"="+sve" }
