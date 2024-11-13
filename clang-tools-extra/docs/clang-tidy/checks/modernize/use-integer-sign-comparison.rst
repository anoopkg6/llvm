.. title:: clang-tidy - modernize-use-integer-sign-comparison

modernize-use-integer-sign-comparison
=====================================

Replace comparisons between signed and unsigned integers with their safe
C++20 ``std::cmp_*`` alternative, if available.


Examples of fixes created by the check:

.. code-block:: c++

  uint func(int a, uint b) {
    return a == b;
  }

becomes

.. code-block:: c++

  #include <utility>

  uint func(int a, uint b) {
    return (std::cmp_equal(result, bla))
  }

Options
-------

.. option:: IncludeStyle

  A string specifying which include-style is used, `llvm` or `google`.
  Default is `llvm`.
