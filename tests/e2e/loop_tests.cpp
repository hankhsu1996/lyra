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

TEST(LoopTest, BasicForCounter) {
  std::string code = R"(
    module Test;
      int i;
      int sum;
      initial begin
        sum = 0;
        for (i = 0; i < 5; i = i + 1) begin
          sum = sum + i;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 10);  // 0+1+2+3+4
}

TEST(LoopTest, ForWithLoopVarDeclaration) {
  std::string code = R"(
    module Test;
      int sum;
      initial begin
        sum = 0;
        for (int i = 0; i < 4; i = i + 1) begin
          sum = sum + i;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 6);  // 0+1+2+3
}

TEST(LoopTest, ForWithMultipleInitializers) {
  std::string code = R"(
    module Test;
      int i, j, sum;
      initial begin
        sum = 0;
        for (i = 0, j = 3; i < 3; i = i + 1, j = j + 1) begin
          sum = sum + i + j;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  // i=0..2, j=3..5 → sum = (0+3)+(1+4)+(2+5) = 15
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 15);
}

TEST(LoopTest, ForWithoutCondition) {
  std::string code = R"(
    module Test;
      int i;
      initial begin
        for (i = 0; ; i = i + 1) begin
          if (i == 3) $finish;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 3);
}

TEST(LoopTest, ForWithoutInitializer) {
  std::string code = R"(
    module Test;
      int i;
      int sum;
      initial begin
        i = 1;
        sum = 0;
        for (; i <= 3; i = i + 1) begin
          sum = sum + i;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 4);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 6);  // 1+2+3
}

TEST(LoopTest, ForWithoutStep) {
  std::string code = R"(
    module Test;
      int i;
      int x;
      initial begin
        x = 0;
        for (i = 0; i < 1;) begin
          x = x + 1;
          i = i + 1;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("x").AsInt64(), 1);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 1);
}

TEST(LoopTest, NestedForLoops) {
  std::string code = R"(
    module Test;
      int i, j, product;
      initial begin
        product = 1;
        for (i = 1; i <= 2; i = i + 1) begin
          for (j = 1; j <= 2; j = j + 1) begin
            product = product * j;
          end
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  // j = 1×2 in both outer loops → 2×2 = 4
  EXPECT_EQ(result.ReadVariable("product").AsInt64(), 4);
}

TEST(LoopTest, BreakInWhile) {
  std::string code = R"(
    module Test;
      int i;
      initial begin
        i = 0;
        while (1) begin
          if (i == 3) break;
          i = i + 1;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 3);
}

TEST(LoopTest, ContinueInWhileWithoutModOrLogicalOr) {
  std::string code = R"(
    module Test;
      int i, sum;
      initial begin
        i = 0;
        sum = 0;
        while (i < 5) begin
          i = i + 1;
          if (i == 2)
            continue;
          if (i == 4)
            continue;
          sum = sum + i;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 9);  // 1 + 3 + 5
}

TEST(LoopTest, BreakInDoWhile) {
  std::string code = R"(
    module Test;
      int x;
      initial begin
        x = 0;
        do begin
          x = x + 1;
          if (x == 2) break;
        end while (1);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("x").AsInt64(), 2);
}

TEST(LoopTest, ContinueInDoWhileWithoutModOrLogicalOr) {
  std::string code = R"(
    module Test;
      int i, sum;
      initial begin
        i = 0;
        sum = 0;
        do begin
          i = i + 1;
          if (i == 2)
            continue;
          if (i == 4)
            continue;
          sum = sum + i;
        end while (i < 5);
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("sum").AsInt64(), 9);  // 1 + 3 + 5
}

TEST(LoopTest, BreakInFor) {
  std::string code = R"(
    module Test;
      int i;
      initial begin
        for (i = 0; i < 10; i = i + 1) begin
          if (i == 4) break;
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("i").AsInt64(), 4);
}

TEST(LoopTest, BreakInnerWhileLoop) {
  std::string code = R"(
    module Test;
      int i, j, count;
      initial begin
        count = 0;
        for (i = 0; i < 2; i = i + 1) begin
          j = 0;
          while (j < 3) begin
            if (j == 1) break;
            count = count + 1;
            j = j + 1;
          end
        end
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("count").AsInt64(), 2);
}
