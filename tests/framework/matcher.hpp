#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::test {

struct ExpectedOutput {
  std::optional<std::string> exact;
  std::vector<std::string> contains;
  std::vector<std::string> not_contains;
};

auto StripAnsi(std::string_view s) -> std::string;

auto CheckOutput(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) -> std::optional<std::string>;

}  // namespace lyra::test
