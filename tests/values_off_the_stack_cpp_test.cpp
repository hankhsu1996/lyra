// A program the C++ backend builds keeps a value larger than the stack it was
// started with off that stack. How much of the stack a body then takes is its
// host compiler's as much as Lyra's: clang at its default level reserves stack
// in a coroutine for every value its frame holds, so the program is built with
// clang -- the release Lyra links, so the answer is the same on every machine.

#include <array>
#include <gtest/gtest.h>
#include <string>

#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/values_off_the_stack.hpp"

namespace {

TEST(ValuesOffTheStack, TheCppBackendBuiltByClangKeepsThemOffIt) {
  const auto clangxx = lyra::test::ResolveClangxx();
  ASSERT_FALSE(clangxx.empty());
  const std::array<std::string, 4> build_options = {
      "--backend", "cpp", "--cxx", clangxx.string()};
  lyra::test::ExpectValuesKeptOffTheStack(build_options);
}

}  // namespace
