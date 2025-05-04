#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

// Basic arithmetic operations
TEST(BinaryOpsTest, ArithmeticAddition) {
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
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 30);
}

TEST(BinaryOpsTest, ArithmeticSubtraction) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 30;
        b = 12;
        c = a - b;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 18);
}

TEST(BinaryOpsTest, ArithmeticMultiplication) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 5;
        b = 7;
        c = a * b;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 35);
}

// Type promotion tests
TEST(BinaryOpsTest, TypePromotionBitToInt) {
  std::string code = R"(
    module Test;
      bit a;
      int b, c;
      initial begin
        a = 1;
        b = 10;
        c = a + b;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 11);
}

TEST(BinaryOpsTest, TypePromotionIntToLongInt) {
  std::string code = R"(
    module Test;
      int a;
      longint b, c;
      initial begin
        a = 100;
        b = 1000;
        c = a + b;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 1100);
}

TEST(BinaryOpsTest, TypePromotionBitToLongInt) {
  std::string code = R"(
    module Test;
      bit a;
      longint b, c;
      initial begin
        a = 1;
        b = 1000;
        c = a + b;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 1001);
}

// Integer comparison operations
TEST(BinaryOpsTest, ComparisonEqual) {
  std::string code = R"(
    module Test;
      int a, b;
      bit c, d;
      initial begin
        a = 10;
        b = 10;
        c = (a == b);
        d = (a == 11);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), false);
}

TEST(BinaryOpsTest, ComparisonNotEqual) {
  std::string code = R"(
    module Test;
      int a, b;
      bit c, d;
      initial begin
        a = 10;
        b = 11;
        c = (a != b);
        d = (a != 10);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), false);
}

TEST(BinaryOpsTest, ComparisonLessThan) {
  std::string code = R"(
    module Test;
      int a, b;
      bit c, d;
      initial begin
        a = 5;
        b = 10;
        c = (a < b);
        d = (b < a);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), false);
}

TEST(BinaryOpsTest, ComparisonLessThanEqual) {
  std::string code = R"(
    module Test;
      int a, b, c;
      bit d, e, f;
      initial begin
        a = 5;
        b = 10;
        c = 5;
        d = (a <= b);
        e = (a <= c);
        f = (b <= a);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("e").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("f").AsBool(), false);
}

TEST(BinaryOpsTest, ComparisonGreaterThan) {
  std::string code = R"(
    module Test;
      int a, b;
      bit c, d;
      initial begin
        a = 15;
        b = 10;
        c = (a > b);
        d = (b > a);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), false);
}

TEST(BinaryOpsTest, ComparisonGreaterThanEqual) {
  std::string code = R"(
    module Test;
      int a, b, c;
      bit d, e, f;
      initial begin
        a = 15;
        b = 10;
        c = 15;
        d = (a >= b);
        e = (a >= c);
        f = (b >= a);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("d").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("e").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("f").AsBool(), false);
}

// String comparison operations
TEST(BinaryOpsTest, StringEqualComparison) {
  std::string code = R"(
    module Test;
      string a, b, c;
      bit r1, r2;
      initial begin
        a = "hello";
        b = "hello";
        c = "world";
        r1 = (a == b);
        r2 = (a == c);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("r1").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("r2").AsBool(), false);
}

TEST(BinaryOpsTest, StringNotEqualComparison) {
  std::string code = R"(
    module Test;
      string a, b, c;
      bit r1, r2;
      initial begin
        a = "hello";
        b = "hello";
        c = "world";
        r1 = (a != c);
        r2 = (a != b);
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("r1").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("r2").AsBool(), false);
}

// Mixed expressions with multiple operations
TEST(BinaryOpsTest, ComplexExpression) {
  std::string code = R"(
    module Test;
      int a, b, c, d;
      initial begin
        a = 5;
        b = 10;
        c = 3;
        d = a + b * c;  // 5 + (10 * 3) = 35
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("d").AsInt64(), 35);
}
