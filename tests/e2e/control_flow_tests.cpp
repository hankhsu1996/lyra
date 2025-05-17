#include <gtest/gtest.h>
#include <lyra/driver/driver.hpp>
#include <string>

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(ControlFlowTest, IfElseWithLiteralCondition) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        if (1)
          a = 42;
        else
          a = 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}

TEST(ControlFlowTest, TernaryWithLiteralCondition) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        a = 1 ? 42 : 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}

TEST(ControlFlowTest, TernaryWithVariableCondition) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        b = 1;
        a = b ? 42 : 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}

TEST(ControlFlowTest, NestedTernary) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        b = 2;
        a = b == 1 ? 1 : (b == 2 ? 2 : 0);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 2);
}

TEST(ControlFlowTest, TernaryWithComplexExpression) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        b = 1;
        c = 1;
        a = (b * c == 1) ? 42 : 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}
