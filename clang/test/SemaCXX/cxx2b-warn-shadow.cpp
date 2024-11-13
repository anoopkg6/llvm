// RUN: %clang_cc1 -verify -fsyntax-only -std=c++2b -Wshadow-all %s

namespace GH95707 {
struct Foo {
  int a; // expected-note {{previous declaration is here}}

  void f1(this auto &self, int a) { self.a = a; }
  void f2(int a) { } // expected-warning {{declaration shadows a field of 'GH95707::Foo'}}
  void f3() {
    (void)[&](this auto &self, int x) { };
  }
};
} // namespace GH95707
