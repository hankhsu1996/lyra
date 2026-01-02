#pragma once

#include <string>
#include <vector>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

auto LoadTestCasesFromYaml(const std::string& path) -> std::vector<TestCase>;

}  // namespace lyra::test
