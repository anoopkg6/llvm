// RUN: %clang_cc1 -std=c++20 -Wno-everything -Wunsafe-buffer-usage \
// RUN:            -fsafe-buffer-usage-suggestions \
// RUN:            -verify %s

// CHECK-NOT: [-Wunsafe-buffer-usage]


void foo(unsigned idx) {
  int buffer[10];         // expected-warning{{'buffer' is an unsafe buffer that does not perform bounds checks}}
                          // expected-note@-1{{change type of 'buffer' to 'std::array' to label it for hardening}}
  buffer[idx] = 0;        // expected-note{{used in buffer access here}}
}

int global_buffer[10];    // expected-warning{{'global_buffer' is an unsafe buffer that does not perform bounds checks}}
void foo2(unsigned idx) {
  global_buffer[idx] = 0;        // expected-note{{used in buffer access here}}
}

struct Foo {
  int member_buffer[10];
  int x;
};

void foo2(Foo& f, unsigned idx) {
  f.member_buffer[idx] = 0; // expected-warning{{unsafe buffer access}}
}

void constant_idx_safe(unsigned idx) {
  int buffer[10];
  buffer[9] = 0;
}

void constant_idx_safe0(unsigned idx) {
  int buffer[10];
  buffer[0] = 0;
}

int array[10]; // expected-warning 4{{'array' is an unsafe buffer that does not perform bounds checks}}

int foo(int x) {
  if(x < 3) 
    return ++x;
  else
    return x + 5; 
};

void masked_idx1(unsigned long long idx, Foo f) {
  // Bitwise and operation
  array[idx & 5] = 10; // no warning
  array[idx & 11 & 5] = 3; // no warning
  array[idx & 11] = 20; // expected-note{{used in buffer access here}}
  array[(idx & 9) | 8];
  array[idx &=5];
  array[f.x & 5];
}

void masked_idx2(unsigned long long idx, unsigned long long idx2) {
  array[idx] = 30; // expected-note{{used in buffer access here}}
  
  // Remainder operation
  array[idx % 10];
  array[10 % idx]; // expected-note{{used in buffer access here}}
  array[9 % idx];
  array[idx % idx2]; // expected-note{{used in buffer access here}}
  array[idx %= 10];
  array[idx % foo(5)];// expected-note{{used in buffer access here}}
}

void masked_idx3(unsigned long long idx) {
  // Left shift operation <<
  array[2 << 1];
  array[8 << 1]; // expected-note{{used in buffer access here}}
  array[2 << idx]; // expected-note{{used in buffer access here}}
  array[idx << 2]; // expected-note{{used in buffer access here}}
  
  // Right shift operation >>
  array[16 >> 1];
  array[8 >> idx];  
  array[idx >> 63];
  array[(idx + idx) >> 3]; // expected-note{{used in buffer access here}}
}

typedef unsigned long long uint64_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

void type_conversions(uint64_t idx1, uint32_t idx2, uint8_t idx3) {
  array[(uint32_t)idx1 & 3];
  array[idx2 & 3];
  array[(uint32_t)idx1 + idx2]; // expected-note{{used in buffer access here}}
  array[idx3 & 3];
  array[idx1 + idx2]; // expected-note{{used in buffer access here}}
  array[idx2 + idx1]; // expected-note{{used in buffer access here}}
}

int array2[5]; // expected-warning{{'array2' is an unsafe buffer that does not perform bounds checks}}

void masked_idx_safe(unsigned long long idx) {
  array2[6 & 5]; // no warning
  array2[6 & idx & (idx + 1) & 5]; // no warning
  array2[6 & idx & 5 & (idx + 1) | 4]; // no warning 
  array2[2 + foo(foo(1))]; // expected-note{{used in buffer access here}}
}


void constant_idx_unsafe(unsigned idx) {
  int buffer[10];       // expected-warning{{'buffer' is an unsafe buffer that does not perform bounds checks}}
                        // expected-note@-1{{change type of 'buffer' to 'std::array' to label it for hardening}}
  buffer[10] = 0;       // expected-note{{used in buffer access here}}
}
