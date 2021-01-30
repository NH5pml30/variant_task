#include "variant.h"

#include "gtest/gtest.h"

// no tests because they were not mine

TEST(variant, dummy) {
  variant<int, float, std::string, bool> v = "abc";
  v = false;
  v = 24;
  v = 2.3f;
}
