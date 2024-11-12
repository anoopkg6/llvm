// RUN: %clang_cc1 -triple s390x-linux-gnu -emit-llvm -o - %s \
// RUN: | FileCheck %s

__fp16 f(__fp16 a, __fp16 b, __fp16 c, __fp16 d) {
    return a * b + c * d;
}

// CHECK-LABEL: define dso_local half @f(half noundef %a, half noundef %b, half noundef %c, half noundef %d) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %a.addr = alloca half, align 2
// CHECK-NEXT:    %b.addr = alloca half, align 2
// CHECK-NEXT:    %c.addr = alloca half, align 2
// CHECK-NEXT:    %d.addr = alloca half, align 2
// CHECK-NEXT:    store half %a, ptr %a.addr, align 2
// CHECK-NEXT:    store half %b, ptr %b.addr, align 2
// CHECK-NEXT:    store half %c, ptr %c.addr, align 2
// CHECK-NEXT:    store half %d, ptr %d.addr, align 2
// CHECK-NEXT:    %0 = load half, ptr %a.addr, align 2
// CHECK-NEXT:    %conv = fpext half %0 to float
// CHECK-NEXT:    %1 = load half, ptr %b.addr, align 2
// CHECK-NEXT:    %conv1 = fpext half %1 to float
// CHECK-NEXT:    %mul = fmul float %conv, %conv1
// CHECK-NEXT:    %2 = load half, ptr %c.addr, align 2
// CHECK-NEXT:    %conv2 = fpext half %2 to float
// CHECK-NEXT:    %3 = load half, ptr %d.addr, align 2
// CHECK-NEXT:    %conv3 = fpext half %3 to float
// CHECK-NEXT:    %mul4 = fmul float %conv2, %conv3
// CHECK-NEXT:    %add = fadd float %mul, %mul4
// CHECK-NEXT:    %4 = fptrunc float %add to half
// CHECK-NEXT:    ret half %4
// CHECK-NEXT:  }

