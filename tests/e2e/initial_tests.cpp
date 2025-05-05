#include <gtest/gtest.h>
#include <string>

#include "driver/driver.hpp"

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(InitialTest, AssignConstant) {
  std::string code = R"(
    module Test;
      int a;
      initial a = 42;
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}

TEST(InitialTest, TwoInitialBlocks) {
  std::string code = R"(
    module Test;
      int a, b;
      initial a = 10;
      initial b = 20;
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 10);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 20);
}

TEST(InitialTest, AddAndAssignSequence) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 5;
        b = a + 3;
        c = b + 4;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 8);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 12);
}
