#include <gtest/gtest.h>

#include <string>

#include "tests/utils/cpp_test_runner.hpp"

using lyra::test::CppTestRunner;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(CodegenTest, SimpleInitialBlock) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        a = 42;
      end
    endmodule
  )";

  auto result = CppTestRunner::RunFromSource(code, {"a"});
  ASSERT_TRUE(result.Success()) << result.ErrorMessage();
  EXPECT_EQ(result.ReadVariable("a"), 42);
}

TEST(CodegenTest, MultipleVariables) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 10;
        b = 20;
        c = a + b;
      end
    endmodule
  )";

  auto result = CppTestRunner::RunFromSource(code, {"a", "b", "c"});
  ASSERT_TRUE(result.Success()) << result.ErrorMessage();
  EXPECT_EQ(result.ReadVariable("a"), 10);
  EXPECT_EQ(result.ReadVariable("b"), 20);
  EXPECT_EQ(result.ReadVariable("c"), 30);
}
