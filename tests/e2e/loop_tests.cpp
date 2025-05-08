#include <gtest/gtest.h>
#include <lyra/driver/driver.hpp>
#include <string>

using Driver = lyra::driver::Driver;

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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 3);
  EXPECT_EQ(result.ReadVariable("j").AsInt64(), 2);
  // 3 outer loops × 2 inner loops
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 6);
}

TEST(LoopTest, BasicDoWhileCounter) {
  std::string code = R"(
    module Test;
      int counter;
      int sum;
      initial begin
        counter = 0;
        sum = 0;
        do begin
          sum = sum + counter;
          counter = counter + 1;
        end while (counter < 5);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("counter").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 10);  // 0+1+2+3+4 = 10
}

TEST(LoopTest, DoWhileWithFalseCondition) {
  std::string code = R"(
    module Test;
      int x;
      initial begin
        x = 10;
        do begin
          x = 20;
        end while (0);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("x").AsInt64(), 20);  // Should execute once
}

TEST(LoopTest, DoWhileWithVariableCondition) {
  std::string code = R"(
    module Test;
      int i;
      int result;
      initial begin
        i = 3;
        result = 0;
        do begin
          result = result + i;
          i = i - 1;
        end while (i > 0);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 0);
  EXPECT_EQ(result.ReadVariable("result").AsInt64(), 6);  // 3+2+1 = 6
}

TEST(LoopTest, NestedDoWhileLoops) {
  std::string code = R"(
    module Test;
      int i;
      int j;
      int sum;
      initial begin
        i = 0;
        sum = 0;
        do begin
          j = 0;
          do begin
            sum = sum + 1;
            j = j + 1;
          end while (j < 2);
          i = i + 1;
        end while (i < 3);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 3);
  EXPECT_EQ(result.ReadVariable("j").AsInt64(), 2);
  // 3 outer loops × 2 inner loops
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 6);
}

TEST(LoopTest, ForeverLoop) {
  std::string code = R"(
    module Test;
      int counter;
      initial begin
        counter = 0;
        forever begin
          counter = counter + 1;
          if (counter >= 5) $finish;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("counter").AsInt64(), 5);
}
