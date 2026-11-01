// A program the execution backend builds keeps a value larger than the stack
// it was started with off that stack. The machine code is Lyra's own, so the
// host compiler only links it, and any one will do.

#include <array>
#include <gtest/gtest.h>
#include <string>

#include "tests/framework/values_off_the_stack.hpp"

namespace {

TEST(ValuesOffTheStack, TheExecutionBackendKeepsThemOffIt) {
  const std::array<std::string, 2> build_options = {"--backend", "llvm"};
  lyra::test::ExpectValuesKeptOffTheStack(build_options);
}

}  // namespace
