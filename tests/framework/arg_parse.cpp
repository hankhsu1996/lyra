#include "tests/framework/arg_parse.hpp"

#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "tests/framework/test_discovery.hpp"

namespace lyra::test {
namespace {

// Try to extract value from --flag=value or --flag value format
// Returns true if flag was found, stores value in out_value
auto TryParseFlag(
    std::string_view arg, std::string_view next_arg, std::string_view flag_name,
    std::string& out_value, bool& consumed_next) -> bool {
  consumed_next = false;
  auto prefix = std::string("--") + std::string(flag_name);

  // Check --flag=value format
  if (arg.starts_with(prefix + "=")) {
    out_value = std::string(arg.substr(prefix.size() + 1));
    return true;
  }

  // Check --flag value format
  if (arg == prefix && !next_arg.empty()) {
    out_value = std::string(next_arg);
    consumed_next = true;
    return true;
  }

  return false;
}

}  // namespace

auto ParseArgs(std::span<char*> argv)
    -> std::pair<CommandLineArgs, std::vector<char*>> {
  CommandLineArgs args;
  std::vector<char*> remaining;
  remaining.push_back(argv[0]);  // Keep program name

  for (size_t i = 1; i < argv.size(); ++i) {
    std::string_view arg = argv[i];
    std::string_view next_arg = (i + 1 < argv.size()) ? argv[i + 1] : "";
    bool consumed_next = false;

    if (TryParseFlag(arg, next_arg, "suite", args.suite, consumed_next) ||
        TryParseFlag(arg, next_arg, "backend", args.backend, consumed_next) ||
        TryParseFlag(
            arg, next_arg, "test_file", args.test_file, consumed_next)) {
      if (consumed_next) {
        ++i;  // Skip the value we consumed
      }
    } else {
      // Pass through to gtest (--gtest_* flags, etc.)
      remaining.push_back(argv[i]);
    }
  }

  return {args, remaining};
}

}  // namespace lyra::test
