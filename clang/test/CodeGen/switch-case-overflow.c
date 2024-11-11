// RUN: split-file %s %t
// RUN: python %t/gen.py %t/switch-overflow.c %t/tmp.c && %clang_cc1 -emit-llvm %t/tmp.c -o - | FileCheck %t/tmp.c

//--- gen.py

import sys
file = sys.argv[1]
out = sys.argv[2]
with open(file) as f:
  text = f.read()
  replacement = ''
  for i in range(0, 32000):
    replacement += "  case {}:\n".format(i + 1500)
  text = text.replace("INSERT_CASES_HERE\n", replacement)
  with open(out, 'w') as of:
    of.write(text)

//--- switch-overflow.c

// Check this doesn't cause the compiler to crash
void foo() {
  // CHECK-LABEL: @foo
  // CHECK-NOT: switch{{ }}
  // CHECK-NOT: br{{ }}

  // 1337 does not match a switch case
  switch (1337) {
INSERT_CASES_HERE
    break;
  }

  // 2000 matches a switch case
  switch(2000) {
INSERT_CASES_HERE
    break;
  }
}
