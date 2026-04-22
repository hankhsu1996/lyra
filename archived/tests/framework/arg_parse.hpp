#ifndef TESTS_FRAMEWORK_ARG_PARSE_HPP
#define TESTS_FRAMEWORK_ARG_PARSE_HPP

#include <span>
#include <utility>
#include <vector>

#include "tests/framework/test_discovery.hpp"

namespace lyra::test {

// Parse custom flags (--suite, --backend, --test_file), return remaining args
// for gtest
auto ParseArgs(std::span<char*> argv)
    -> std::pair<CommandLineArgs, std::vector<char*>>;

}  // namespace lyra::test

#endif  // TESTS_FRAMEWORK_ARG_PARSE_HPP
