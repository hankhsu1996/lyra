#include <gtest/gtest.h>
#include <lyra/driver/driver.hpp>
#include <string>

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(VariableDeclarationTest, DeclarationWithoutInitializer) {
  std::string code = R"(
    module Test;
      int result;
      initial begin
        int a;
        a = 123;
        result = a;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("result").AsInt64(), 123);
}

TEST(VariableDeclarationTest, DeclarationWithInitializer) {
  std::string code = R"(
    module Test;
      int result;
      initial begin
        int a = 42;
        result = a;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("result").AsInt64(), 42);
}

TEST(VariableDeclarationTest, MultipleLocalsUsingPreviousValue) {
  std::string code = R"(
    module Test;
      int final_a, final_b, final_c;
      initial begin
        int a = 1;
        int b = a + 2;
        int c = b + 3;
        final_a = a;
        final_b = b;
        final_c = c;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("final_a").AsInt64(), 1);
  EXPECT_EQ(result.ReadVariable("final_b").AsInt64(), 3);
  EXPECT_EQ(result.ReadVariable("final_c").AsInt64(), 6);
}

TEST(VariableDeclarationTest, LocalOverridesModuleVariable) {
  std::string code = R"(
    module Test;
      int x;
      initial begin
        int x = 99;
        // local x should not affect module x
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("x").AsInt64(), 0);
}
