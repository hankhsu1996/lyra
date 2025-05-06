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
