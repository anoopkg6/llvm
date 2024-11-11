//===-- Implementation of lsearch -------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "src/search/lsearch.h"
#include "src/__support/CPP/cstddef.h" // cpp::byte
#include "src/__support/common.h"
#include "src/__support/macros/config.h"
#include "src/string/memory_utils/inline_memcpy.h" // inline_memcpy

namespace LIBC_NAMESPACE_DECL {
LLVM_LIBC_FUNCTION(void *, lsearch,
                   (void *key, void *base, size_t *nmemb, size_t size,
                    int (*compar)(void *, void *))) {
  cpp::byte *next = reinterpret_cast<cpp::byte *>(base);
  cpp::byte *end = next + (*nmemb * size);
  while (next < end) {
    if (compar(key, next) == 0) {
      return next;
    }
    next += size;
  }

  inline_memcpy(next, key, size);
  *nmemb += 1;
  return next;
}

} // namespace LIBC_NAMESPACE_DECL
