#include <gtest/gtest.h>
#include <string>

#include "driver/driver.hpp"

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

// Basic unary operations
TEST(UnaryOpsTest, UnaryPlus) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 10;
        b = +a;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 10);
}

TEST(UnaryOpsTest, UnaryMinus) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 10;
        b = -a;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), -10);
}

TEST(UnaryOpsTest, LogicalNot) {
  std::string code = R"(
    module Test;
      int a, zero;
      bit r1, r2;
      initial begin
        a = 10;
        zero = 0;
        r1 = !a;
        r2 = !zero;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("r1").AsBool(), false);
  EXPECT_EQ(result.ReadVariable("r2").AsBool(), true);
}

TEST(UnaryOpsTest, BitwiseNot) {
  std::string code = R"(
    module Test;
      int a;
      int b;
      initial begin
        a = 10;  // 1010 in binary
        b = ~a;  // Should be -11 in 32-bit int
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), ~10);
}

// Since we can't use bit vectors, we'll test with single bits and integers
TEST(UnaryOpsTest, LogicalOperations) {
  std::string code = R"(
    module Test;
      bit b0, b1;
      int zero, nonzero;
      bit r1, r2, r3, r4;
      initial begin
        b0 = 0;
        b1 = 1;
        zero = 0;
        nonzero = 42;

        // Logical NOT with different types
        r1 = !b0;      // !0 = 1
        r2 = !b1;      // !1 = 0
        r3 = !zero;    // !0 = 1
        r4 = !nonzero; // !42 = 0
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("r1").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("r2").AsBool(), false);
  EXPECT_EQ(result.ReadVariable("r3").AsBool(), true);
  EXPECT_EQ(result.ReadVariable("r4").AsBool(), false);
}

TEST(UnaryOpsTest, MixedTypesWithUnaryOps) {
  std::string code = R"(
    module Test;
      bit b;
      int i;
      longint l;
      int result1, result2;
      initial begin
        b = 1;
        i = 100;
        l = 1000;

        result1 = -i + b;      // -100 + 1 = -99
        result2 = -(-i + l);   // -(-100 + 1000) = -900
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("result1").AsInt64(), -99);
  EXPECT_EQ(result.ReadVariable("result2").AsInt64(), -900);
}

// For compound expressions with priorities
TEST(UnaryOpsTest, CompoundUnaryExpressions) {
  std::string code = R"(
    module Test;
      int a, b, c, d;
      initial begin
        a = 5;
        b = 3;
        c = -a + b * 2;  // -5 + 6 = 1
        d = -(a + b);    // -(5 + 3) = -8
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 1);
  EXPECT_EQ(result.ReadVariable("d").AsInt64(), -8);
}

// Tests for increment/decrement operators
TEST(UnaryOpsTest, PreIncrement) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 5;
        b = ++a;  // b = 6, a = 6
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 6);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 6);
}

TEST(UnaryOpsTest, PostIncrement) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 5;
        b = a++;  // b = 5, a = 6
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 6);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 5);
}

TEST(UnaryOpsTest, PreDecrement) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 5;
        b = --a;  // b = 4, a = 4
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 4);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 4);
}

TEST(UnaryOpsTest, PostDecrement) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 5;
        b = a--;  // b = 5, a = 4
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 4);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 5);
}

TEST(UnaryOpsTest, NestedIncrementDecrement) {
  std::string code = R"(
    module Test;
      int a, b, c, d;
      initial begin
        a = 5;
        b = 10;
        c = ++a + b--;  // a=6, b=9, c=16
        d = --a + ++b;  // a=5, b=10, d=15
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 10);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 16);
  EXPECT_EQ(result.ReadVariable("d").AsInt64(), 15);
}

// Tests for reduction operators with primitive types
// Since we can't use bit vectors, we'll use integers with specific bit patterns

TEST(UnaryOpsTest, ReductionOperatorsSimple) {
  std::string code = R"(
    module Test;
      int a, b, c, d, e, f;
      bit r_and, r_nand, r_or, r_nor, r_xor, r_xnor;
      initial begin
        a = 15;    // 0b1111 - all bits in lower 4 bits are 1
        b = 0;     // 0b0000 - all bits are 0
        c = 10;    // 0b1010 - mixed 1s and 0s (even number of 1s)
        d = 11;    // 0b1011 - mixed 1s and 0s (odd number of 1s)

        // Reduction AND: 1 if all bits are 1
        r_and = &a;    // Should be 0 (not all 32 bits are 1)

        // Reduction NAND: 0 if all bits are 1, else 1
        r_nand = ~&a;  // Should be 1 (not all 32 bits are 1)

        // Reduction OR: 1 if any bit is 1
        r_or = |a;     // Should be 1 (at least one bit is 1)

        // Reduction NOR: 0 if any bit is 1, else 1
        r_nor = ~|b;   // Should be 1 (all bits are 0)

        // Reduction XOR: 1 if odd number of bits are 1
        r_xor = ^d;    // For 11 (0b1011), should be 1 (odd number of 1s)

        // Reduction XNOR: 1 if even number of bits are 1 (or all 0s)
        r_xnor = ~^c;  // For 10 (0b1010), should be 1 (even number of 1s)
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  // These expectations depend on how your implementation handles reduction
  // operators You may need to adjust these based on your exact implementation
  EXPECT_EQ(
      result.ReadVariable("r_and").AsBool(), false);  // Not all 32 bits are 1
  EXPECT_EQ(
      result.ReadVariable("r_nand").AsBool(), true);  // Not all 32 bits are 1
  EXPECT_EQ(
      result.ReadVariable("r_or").AsBool(), true);  // At least one bit is 1
  EXPECT_EQ(
      result.ReadVariable("r_nor").AsBool(), true);  // All bits in 0 are 0
  EXPECT_EQ(
      result.ReadVariable("r_xor").AsBool(), true);  // Odd number of 1s in 11
  EXPECT_EQ(
      result.ReadVariable("r_xnor").AsBool(), true);  // Even number of 1s in 10
}
