#include <gtest/gtest.h>
#include <string>

#include "driver/driver.hpp"

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(NonBlockingTest, SimpleNonBlockingAssignment) {
  std::string code = R"(
    module Test;
      int a, b;

      initial begin
        a = 1;
        b <= a + 1;
        a = 5;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 5);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 2);
  EXPECT_EQ(result.final_time, 0);
}

TEST(NonBlockingTest, RegisterBehaviorWithClock) {
  std::string code = R"(
    module Test;
      bit clk;
      int d, q;

      // Clock generator
      initial begin
        clk = 0;
        forever #5 clk = ~clk;
      end

      // Register with non-blocking assignment
      always_ff @(posedge clk)
        q <= d;

      initial begin
        d = 42;
        #12;
        d = 99;
        #10;
        $finish();
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("q").AsInt64(), 99);
  EXPECT_EQ(result.final_time, 22);
}

TEST(NonBlockingTest, RegisterWithEnable) {
  std::string code = R"(
    module Test;
      bit clk, en;
      int d, q;

      // Clock driver
      initial begin
        clk = 0;
        forever #5 clk = ~clk;
      end

      // Register with enable
      always_ff @(posedge clk) begin
        if (en)
          q <= d;
      end

      initial begin
        en = 1;
        d = 42;
        #10;  // clk posedge at t=10, q <= 42

        d = 99;
        #10;  // clk posedge at t=20, q <= 99

        en = 0;
        d = 7;
        #10;  // clk posedge at t=30, no update

        $finish();
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("q").AsInt64(), 99);
  EXPECT_EQ(result.final_time, 30);
}

TEST(NonBlockingTest, BlockingThenNonBlocking) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a = 1;
        b <= a;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 1);
}

TEST(NonBlockingTest, MultipleNonBlockingAssignments) {
  std::string code = R"(
    module Test;
      int a, b;
      initial begin
        a <= 1;
        b <= 2;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 1);
  EXPECT_EQ(result.ReadVariable("b").AsInt64(), 2);
}

TEST(NonBlockingTest, PipelinedNonBlocking) {
  std::string code = R"(
    module Test;
      int d, q1, q2;
      bit clk;

      always_ff @(posedge clk) begin
        q1 <= d;
        q2 <= q1;
      end

      initial begin
        d = 10;
        clk = 0;
        #5 clk = 1;
        #5 clk = 0;
        #5 clk = 1;
        #5 clk = 0;
        $finish();
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("q1").AsInt64(), 10);
  EXPECT_EQ(result.ReadVariable("q2").AsInt64(), 10);
  EXPECT_EQ(result.final_time, 20);
}
