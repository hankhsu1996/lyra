#include "tests/framework/yaml_loader.hpp"

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

// NOLINTNEXTLINE(misc-include-cleaner): yaml.h is the public API
#include <yaml-cpp/yaml.h>

#include "tests/framework/test_case.hpp"

namespace lyra::test {

auto LoadTestCasesFromYaml(const std::string& path) -> std::vector<TestCase> {
  std::vector<TestCase> cases;

  // NOLINTNEXTLINE(misc-include-cleaner): LoadFile is provided by yaml.h
  auto root = YAML::LoadFile(path);
  auto feature = root["feature"].as<std::string>("");

  for (const auto& node : root["cases"]) {
    TestCase tc;
    tc.name = node["name"].as<std::string>();
    tc.feature = feature;

    // Single-file format: sv: |
    if (node["sv"]) {
      tc.sv_code = node["sv"].as<std::string>();
    }

    // Multi-file format: files: [...]
    if (node["files"]) {
      for (const auto& file_node : node["files"]) {
        SourceFile sf;
        sf.name = file_node["name"].as<std::string>();
        sf.content = file_node["content"].as<std::string>();
        tc.files.push_back(std::move(sf));
      }
    }

    // Plusargs: plusargs: ["+VERBOSE", "+COUNT=42"]
    if (node["plusargs"]) {
      for (const auto& arg : node["plusargs"]) {
        tc.plusargs.push_back(arg.as<std::string>());
      }
    }

    // Parse unified expect: block
    if (node["expect"]) {
      const auto& expect = node["expect"];

      // expect.variables: {var: value, ...}
      if (expect["variables"]) {
        for (const auto& pair : expect["variables"]) {
          auto var_name = pair.first.as<std::string>();

          // Try integer first, fall back to double
          try {
            tc.expected_values[var_name] = pair.second.as<int64_t>();
          } catch (const YAML::BadConversion&) {
            tc.expected_values[var_name] = pair.second.as<double>();
          }
        }
      }

      // expect.time: N
      if (expect["time"]) {
        tc.expected_time = expect["time"].as<uint64_t>();
      }

      // expect.output: string OR {contains: [...]}
      if (expect["output"]) {
        ExpectedOutput output;
        if (expect["output"].IsScalar()) {
          // Simple string - exact match
          output.exact = expect["output"].as<std::string>();
        } else {
          // Map with contains/not_contains
          if (expect["output"]["contains"]) {
            for (const auto& item : expect["output"]["contains"]) {
              output.contains.push_back(item.as<std::string>());
            }
          }
          if (expect["output"]["not_contains"]) {
            for (const auto& item : expect["output"]["not_contains"]) {
              output.not_contains.push_back(item.as<std::string>());
            }
          }
        }
        tc.expected_output = std::move(output);
      }
    }

    // Parse skip flags
    if (node["skip_codegen"]) {
      tc.skip_codegen = node["skip_codegen"].as<bool>();
    }
    if (node["skip_interpreter"]) {
      tc.skip_interpreter = node["skip_interpreter"].as<bool>();
    }

    cases.push_back(std::move(tc));
  }

  return cases;
}

}  // namespace lyra::test
