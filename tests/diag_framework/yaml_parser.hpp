#pragma once

#include <filesystem>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "tests/diag_framework/expected_diag.hpp"
#include "tests/framework/runner.hpp"

namespace lyra::test {

auto ParseExpectedDiag(const YAML::Node& node) -> ExpectedDiag;
auto ParseExpectedDiagList(const YAML::Node& seq) -> std::vector<ExpectedDiag>;

auto LoadExpectedDiagnostics(const TestCase& tc) -> std::vector<ExpectedDiag>;

auto LoadExpectedDiagnostics(const std::filesystem::path& case_yaml_path)
    -> std::vector<ExpectedDiag>;

}  // namespace lyra::test
