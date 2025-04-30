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
  auto ctx = lyra::test::Simulate(code);
  EXPECT_EQ(ctx->signal_table.Read("a").AsInt(), 42);
}
