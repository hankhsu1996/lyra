#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(TimingTest, OnlyDelay) {
  std::string code = R"(
    module Test;
      initial #5;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.final_time, 5);
}

TEST(TimingTest, DelayWithInlineAssign) {
  std::string code = R"(
    module Test;
      int a;
      initial #5 a = 1;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.final_time, 5);
}

TEST(TimingTest, DelayThenAssign) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        #5;
        a = 2;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 2);
  EXPECT_EQ(result.final_time, 5);
}

TEST(TimingTest, MultipleDelaysThenAssign) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        #2;
        #3;
        a = 3;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 3);
  EXPECT_EQ(result.final_time, 5);
}

TEST(TimingTest, MixedDelayForms) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 10;
        #2 b = a + 1;
        #3;
        a = b + 1;
      end
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 12);  // 10+1+1
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 11);  // 10+1
  EXPECT_EQ(result.final_time, 5);
}
