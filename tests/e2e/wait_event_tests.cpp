#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(WaitEventTest, AlwaysCombRunsOnceAtTimeZero) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 1;
        b = 2;
      end

      always_comb c = a + b;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 3);
  EXPECT_EQ(result.final_time, 0);
}

TEST(WaitEventTest, AlwaysCombReactsToInputChange) {
  std::string code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 1;
        b = 2;
        #5 a = 3;
      end

      always_comb c = a + b;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 5);
  EXPECT_EQ(result.final_time, 5);
}

TEST(WaitEventTest, WaitUntilChange) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        @(a);
      end

      initial #5 a = 1;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.final_time, 5);
}

TEST(WaitEventTest, WaitThenDelay) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        @(a);
        #2;
      end

      initial #5 a = 1;
    endmodule
  )";
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.final_time, 7);
}
