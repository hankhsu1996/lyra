#include <gtest/gtest.h>
#include <string>

#include "lyra/driver/driver.hpp"

using Driver = lyra::driver::Driver;

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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 3);
  EXPECT_EQ(result.FinalTime(), 0);
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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("c").AsInt64(), 5);
  EXPECT_EQ(result.FinalTime(), 5);
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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 5);
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
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 7);
}

TEST(WaitEventTest, PosedgeTrigger) {
  std::string code = R"(
    module Test;
      int clk, seen;
      initial begin
        @(posedge clk);
        seen = 1;
      end

      initial begin
        clk = 0;
        #5 clk = 1;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("seen").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 5);
}

TEST(WaitEventTest, NegedgeTrigger) {
  std::string code = R"(
    module Test;
      int clk, seen;
      initial begin
        @(negedge clk);
        seen = 1;
      end

      initial begin
        clk = 1;
        #5 clk = 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("seen").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 5);
}

TEST(WaitEventTest, StrictEdgeTrigger) {
  std::string code = R"(
    module Test;
      int clk, seen;
      initial begin
        @(edge clk);
        seen = 1;
      end

      initial begin
        clk = 0;
        #5 clk = 1;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("seen").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 5);
}

TEST(WaitEventTest, AnyChangeStringTrigger) {
  std::string code = R"(
    module Test;
      string s;
      int seen;
      initial begin
        @(s);
        seen = 1;
      end

      initial begin
        s = "abc";
        #3 s = "def";
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("seen").AsInt64(), 1);
  EXPECT_EQ(result.FinalTime(), 3);
}

TEST(WaitEventTest, AlwaysFFPosedge) {
  std::string code = R"(
    module Test;
      int clk, q;
      initial clk = 0;

      always_ff @(posedge clk) q = 42;

      initial begin
        #2 clk = 1;  // posedge
        #2 clk = 0;
        #2 clk = 1;  // posedge
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("q").AsInt64(), 42);
  EXPECT_EQ(result.FinalTime(), 6);
}

TEST(WaitEventTest, AlwaysLatchBehavior) {
  std::string code = R"(
    module Test;
      int a, b, q;
      always_latch begin
        if (a > 0)
          q = a + b;
      end

      initial begin
        a = 1; b = 2;
        #5 a = 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("q").AsInt64(), 3);
  EXPECT_EQ(result.FinalTime(), 5);
}

TEST(WaitEventTest, CorrectPosedgeCount) {
  std::string code = R"(
    module Test;
      bit a;
      int count;

      always_ff @(posedge a) begin
        count = count + 1;
      end

      initial begin
        a = 0;
        count = 0;
        #5 a = 1;  // posedge 1
        #5 a = 0;
        #5 a = 1;  // posedge 2
        #5 a = 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("count").AsInt64(), 2);  // Expect 2 posedges
  EXPECT_EQ(result.FinalTime(), 20);
}
