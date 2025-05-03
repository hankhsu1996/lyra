#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

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
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt(), 42);
}

TEST(InitialTest, TwoInitialBlocks) {
  std::string code = R"(
    module Test;
      int a, b;
      initial a = 10;
      initial b = 20;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt(), 10);
  EXPECT_EQ(result.ReadVariable("b").AsInt(), 20);
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
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt(), 5);
  EXPECT_EQ(result.ReadVariable("b").AsInt(), 8);
  EXPECT_EQ(result.ReadVariable("c").AsInt(), 12);
}
