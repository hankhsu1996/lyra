#include <gtest/gtest.h>
#include <lyra/driver/driver.hpp>
#include <string>

using Driver = lyra::driver::Driver;

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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("d").AsInt64(), 35);
}

// Bitwise operation tests
TEST(BinaryOpsTest, BitwiseOperations) {
  std::string code = R"(
    module Test;
      int a, b, c, d, e, f, g;
      initial begin
        a = 5;  // 0101
        b = 3;  // 0011
        c = a & b;   // 0001 = 1
        d = a | b;   // 0111 = 7
        e = a ^ b;   // 0110 = 6
        f = a ~^ b;  // 1001 = -7 (XNOR)
        g = a ^~ b;  // 1001 = -7 (XNOR, same as ~^)
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 1);   // AND
  EXPECT_EQ(result.ReadVariable("d").AsInt64(), 7);   // OR
  EXPECT_EQ(result.ReadVariable("e").AsInt64(), 6);   // XOR
  EXPECT_EQ(result.ReadVariable("f").AsInt64(), -7);  // XNOR (~^)
  EXPECT_EQ(result.ReadVariable("g").AsInt64(), -7);  // XNOR (^~)
}

// Logical operation tests
TEST(BinaryOpsTest, LogicalOperations) {
  std::string code = R"(
    module Test;
      int a, b, c, d, e, f, g, h;
      bit r1, r2, r3, r4, r5, r6;
      initial begin
        a = 5;   // non-zero
        b = 0;   // zero
        c = -1;  // non-zero
        d = 0;   // zero

        // Logical AND tests
        r1 = a && b;  // 1 && 0 = 0
        r2 = a && c;  // 1 && 1 = 1
        r3 = b && d;  // 0 && 0 = 0

        // Logical OR tests
        r4 = a || b;  // 1 || 0 = 1
        r5 = b || d;  // 0 || 0 = 0
        r6 = a || c;  // 1 || 1 = 1
      end
    endmodule
  )";

  auto result = Driver::RunFromSource(code);

  // AND: non-zero && zero
  EXPECT_EQ(result.ReadVariable("r1").AsBool(), false);

  // AND: non-zero && non-zero
  EXPECT_EQ(result.ReadVariable("r2").AsBool(), true);

  // AND: zero && zero
  EXPECT_EQ(result.ReadVariable("r3").AsBool(), false);

  // OR: non-zero || zero
  EXPECT_EQ(result.ReadVariable("r4").AsBool(), true);

  // OR: zero || zero
  EXPECT_EQ(result.ReadVariable("r5").AsBool(), false);

  // OR: non-zero || non-zero
  EXPECT_EQ(result.ReadVariable("r6").AsBool(), true);
}

// Shift operation tests
TEST(BinaryOpsTest, ShiftOperations) {
  std::string code = R"(
    module Test;
      // Signed values
      int sa, sb, sc;
      int s_arith_right, s_logic_right;
      int s_arith_left, s_logic_left;

      // Unsigned values
      bit [31:0] ua, ub, uc;
      bit [31:0] u_arith_right, u_logic_right;
      bit [31:0] u_arith_left, u_logic_left;

      initial begin
        // Signed values
        sa = -8;
        sb = 2;
        sc = 8;

        // Unsigned values
        ua = 32'h80000000;
        ub = 2;
        uc = 8;

        // Signed arithmetic right shift (should sign-extend)
        s_arith_right = sa >>> sb;

        // Signed logical right shift (should zero-fill)
        s_logic_right = sa >> sb;

        // Signed left shifts
        s_arith_left = sa <<< sb;
        s_logic_left = sa << sb;

        // Unsigned right shifts (always zero-fill)
        u_arith_right = ua >>> ub;
        u_logic_right = ua >> ub;

        // Unsigned left shifts
        u_arith_left = uc <<< ub;
        u_logic_left = uc << ub;
      end
    endmodule
  )";

  auto result = Driver::RunFromSource(code);

  // -8 >>> 2 = -2 (arithmetic shift, sign-extended)
  EXPECT_EQ(result.ReadVariable("s_arith_right").AsInt64(), -2);

  // -8 >> 2 = 1073741822 (logical shift, zero-fill → 0x3FFFFFFE)
  EXPECT_EQ(result.ReadVariable("s_logic_right").AsInt64(), 0x3FFFFFFE);

  // -8 <<< 2 = -32
  EXPECT_EQ(result.ReadVariable("s_arith_left").AsInt64(), -32);

  // -8 << 2 = -32
  EXPECT_EQ(result.ReadVariable("s_logic_left").AsInt64(), -32);

  // 0x80000000 >>> 2 = 0x20000000 (zero-fill)
  EXPECT_EQ(result.ReadVariable("u_arith_right").AsInt64(), 0x20000000);

  // 0x80000000 >> 2 = 0x20000000
  EXPECT_EQ(result.ReadVariable("u_logic_right").AsInt64(), 0x20000000);

  // 8 <<< 2 = 32
  EXPECT_EQ(result.ReadVariable("u_arith_left").AsInt64(), 32);

  // 8 << 2 = 32
  EXPECT_EQ(result.ReadVariable("u_logic_left").AsInt64(), 32);
}
