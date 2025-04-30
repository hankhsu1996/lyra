#include <gtest/gtest.h>
#include <string>

#include <core/execution_context.hpp>
#include <core/simulation_preparation.hpp>
#include <core/simulation_scheduler.hpp>
#include <frontend/slang_frontend.hpp>
#include <lowering/ast_to_mir/ast_to_mir.hpp>
#include <lowering/mir_to_lir/mir_to_lir.hpp>
#include <mir/module.hpp>

#include "common/simulate_util.hpp"

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

TEST(InitialTest, AssignConstant) {
  auto code = R"(
    module Test;
      int a;
      initial a = 42;
    endmodule
  )"s;
  auto context = lyra::test::Simulate(code);
  EXPECT_EQ(context->signal_table.Read("a").AsInt(), 42);
}

TEST(InitialTest, TwoInitialBlocks) {
  auto code = R"(
    module Test;
      int a, b;
      initial a = 10;
      initial b = 20;
    endmodule
  )"s;
  auto context = lyra::test::Simulate(code);
  EXPECT_EQ(context->signal_table.Read("a").AsInt(), 10);
  EXPECT_EQ(context->signal_table.Read("b").AsInt(), 20);
}

TEST(InitialTest, AddAndAssignSequence) {
  auto code = R"(
    module Test;
      int a, b, c;
      initial begin
        a = 5;
        b = a + 3;
        c = b + 4;
      end
    endmodule
  )"s;
  auto context = lyra::test::Simulate(code);
  EXPECT_EQ(context->signal_table.Read("a").AsInt(), 5);
  EXPECT_EQ(context->signal_table.Read("b").AsInt(), 8);
  EXPECT_EQ(context->signal_table.Read("c").AsInt(), 12);
}
