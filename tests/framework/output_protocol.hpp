#pragma once

#include <cstdint>
#include <map>
#include <string>

#include "tests/framework/test_value.hpp"

namespace lyra::test {

// Parsed output from __LYRA_VAR and __LYRA_TIME__ protocol.
struct ParsedOutput {
  std::string clean;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
};

// Parse __LYRA_VAR and __LYRA_TIME__ output, stripping protocol lines.
// Shared between JIT and LLI backends.
auto ParseLyraVarOutput(const std::string& output) -> ParsedOutput;

}  // namespace lyra::test
