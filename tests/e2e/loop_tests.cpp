#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(LoopTest, BasicWhileCounter) {
  std::string code = R"(
    module Test;
      int counter;
      int sum;
      initial begin
        counter = 0;
        sum = 0;
        while (counter < 5) begin
          sum = sum + counter;
          counter = counter + 1;
        end
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("counter").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 10);  // 0+1+2+3+4 = 10
}

TEST(LoopTest, WhileWithFalseCondition) {
  std::string code = R"(
    module Test;
      int x;
      initial begin
        x = 10;
        while (0) begin
          x = 20;
        end
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("x").AsInt64(), 10);  // Should remain unchanged
}

TEST(LoopTest, WhileWithVariableCondition) {
  std::string code = R"(
    module Test;
      int i;
      int result;
      initial begin
        i = 3;
        result = 0;
        while (i > 0) begin
          result = result + i;
          i = i - 1;
        end
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 0);
  EXPECT_EQ(result.ReadVariable("result").AsInt64(), 6);  // 3+2+1 = 6
}

TEST(LoopTest, NestedWhileLoops) {
  std::string code = R"(
    module Test;
      int i;
      int j;
      int sum;
      initial begin
        i = 0;
        sum = 0;
        while (i < 3) begin
          j = 0;
          while (j < 2) begin
            sum = sum + 1;
            j = j + 1;
          end
          i = i + 1;
        end
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 3);
  EXPECT_EQ(result.ReadVariable("j").AsInt64(), 2);
  // 3 outer loops × 2 inner loops
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 6);
}
