#include <gtest/gtest.h>
#include <lyra/driver/driver.hpp>
#include <string>

using Driver = lyra::driver::Driver;

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(ControlFlowTest, IfElseWithLiteralCondition) {
  std::string code = R"(
    module Test;
      int a;
      initial begin
        if (1)
          a = 42;
        else
          a = 0;
      end
    endmodule
  )";
  auto result = Driver::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}
