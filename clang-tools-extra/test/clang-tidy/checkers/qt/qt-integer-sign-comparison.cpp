// RUN: %check_clang_tidy -std=c++17 %s qt-integer-sign-comparison %t

// The code that triggers the check
#define MAX_MACRO(a, b) (a < b) ? b : a

unsigned int FuncParameters(int bla) {
    unsigned int result;
    if (result == bla)
        return 0;
// CHECK-MESSAGES: :[[@LINE-2]]:9: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]
// CHECK-FIXES: if (q20::cmp_equal(result ,  bla))

    return 1;
}

template <typename T>
void TemplateFuncParameter(T val) {
    unsigned long uL = 0;
    if (val >= uL)
        return;
// CHECK-MESSAGES-NOT: warning:
}

template <typename T1, typename T2>
int TemplateFuncParameters(T1 val1, T2 val2) {
    if (val1 >= val2)
        return 0;
// CHECK-MESSAGES-NOT: warning:
    return 1;
}

int AllComparisons() {
    unsigned int uVar = 42;
    unsigned short uArray[2] = {0, 1};

    int sVar = -42;
    short sArray[2] = {-1, -2};

    if ((int)uVar < sVar)
        return 0;
// CHECK-MESSAGES: :[[@LINE-2]]:9: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]
// CHECK-FIXES: if (q20::cmp_less(uVar ,  sVar))

    (uVar != sVar) ? uVar = sVar
                   : sVar = uVar;
// CHECK-MESSAGES: :[[@LINE-2]]:6: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]
// CHECK-FIXES: (q20::cmp_not_equal(uVar ,  sVar)) ? uVar = sVar

    while (uArray[0] <= sArray[0])
        return 0;
// CHECK-MESSAGES: :[[@LINE-2]]:12: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]
// CHECK-FIXES: while (q20::cmp_less_equal(uArray[0] ,  sArray[0]))

    if (uArray[1] > sArray[1])
        return 0;
// CHECK-MESSAGES: :[[@LINE-2]]:9: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]
// CHECK-FIXES: if (q20::cmp_greater(uArray[1] ,  sArray[1]))

    MAX_MACRO(uVar, sArray[0]);
// CHECK-MESSAGES: :[[@LINE-1]]:15: warning: comparison between 'signed' and 'unsigned' integers [qt-integer-sign-comparison]

    FuncParameters(uVar);
    TemplateFuncParameter(sVar);
    TemplateFuncParameters(uVar, sVar);

    return 0;
}
