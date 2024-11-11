//===-- Unittests for lsearch ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "src/search/lsearch.h"
#include "test/UnitTest/Test.h"

int compar(void *a, void *b) {
  return *reinterpret_cast<int *>(a) - *reinterpret_cast<int *>(b);
}

TEST(LlvmLibcLsearchTest, SearchHead) {
  int list[4] = {1, 2, 3, 4};
  size_t len = 3; // intentionally 3 and not all 4
  int key = 1;
  void *ret = LIBC_NAMESPACE::lsearch(&key, list, &len, sizeof(int), compar);

  ASSERT_TRUE(ret == &list[0]);           // head must be returned
  ASSERT_EQ(key, 1);                      // `key` must not be changed
  ASSERT_EQ(len, static_cast<size_t>(3)); // `len` must not be changed
  // `list` must not be changed
  ASSERT_EQ(list[0], 1);
  ASSERT_EQ(list[1], 2);
  ASSERT_EQ(list[2], 3);
  ASSERT_EQ(list[3], 4);
}

TEST(LlvmLibcLsearchTest, SearchMiddle) {
  int list[4] = {1, 2, 3, 4};
  size_t len = 3; // intentionally 3 and not all 4
  int key = 2;
  void *ret = LIBC_NAMESPACE::lsearch(&key, list, &len, sizeof(int), compar);

  ASSERT_TRUE(ret == &list[1]); // ptr to second element must be returned
  ASSERT_EQ(key, 2);            // `key` must not be changed
  ASSERT_EQ(len, static_cast<size_t>(3)); // `len` must not be changed
  // `list` must not be changed
  ASSERT_EQ(list[0], 1);
  ASSERT_EQ(list[1], 2);
  ASSERT_EQ(list[2], 3);
  ASSERT_EQ(list[3], 4);
}

TEST(LlvmLibcLsearchTest, SearchTail) {
  int list[4] = {1, 2, 3, 4};
  size_t len = 3; // intentionally 3 and not all 4
  int key = 3;
  void *ret = LIBC_NAMESPACE::lsearch(&key, list, &len, sizeof(int), compar);

  ASSERT_TRUE(ret == &list[2]); // ptr to last element must be returned
  ASSERT_EQ(key, 3);            // `key` must not be changed
  ASSERT_EQ(len, static_cast<size_t>(3)); // `len` must not be changed
  // `list` must not be changed
  ASSERT_EQ(list[0], 1);
  ASSERT_EQ(list[1], 2);
  ASSERT_EQ(list[2], 3);
  ASSERT_EQ(list[3], 4);
}

TEST(LlvmLibcLsearchTest, SearchNonExistent) {
  int list[4] = {1, 2, 3, 4};
  size_t len = 3; // intentionally 3 and not all 4
  int key = 5;
  void *ret = LIBC_NAMESPACE::lsearch(&key, list, &len, sizeof(int), compar);

  ASSERT_TRUE(ret == &list[3]);           // ptr past tail must be returned
  ASSERT_EQ(key, 5);                      // `key` must not be changed
  ASSERT_EQ(len, static_cast<size_t>(4)); // `len` must be incremented one
  // `list` must not be changed
  ASSERT_EQ(list[0], 1);
  ASSERT_EQ(list[1], 2);
  ASSERT_EQ(list[2], 3);
  // `5` must be appended to list (replacing the dummy 4)
  ASSERT_EQ(list[3], 5);
}
