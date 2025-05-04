#include <gtest/gtest.h>
#include <string>

#include "simulation/simulate.hpp"

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
  auto result = lyra::RunFromSource(code);
  EXPECT_EQ(result.ReadVariable("a").AsInt64(), 42);
}
