#include "tests/framework/yaml_loader.hpp"

#include <yaml-cpp/yaml.h>

namespace lyra::test {

auto LoadTestCasesFromYaml(const std::string& path) -> std::vector<TestCase> {
  std::vector<TestCase> cases;

  YAML::Node root = YAML::LoadFile(path);
  std::string feature = root["feature"].as<std::string>("");

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

    // Parse unified expect: block
    if (node["expect"]) {
      const auto& expect = node["expect"];

      // expect.variables: {var: value, ...}
      if (expect["variables"]) {
        for (const auto& pair : expect["variables"]) {
          std::string var_name = pair.first.as<std::string>();
          int64_t value = pair.second.as<int64_t>();
          tc.expected_values[var_name] = value;
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
        } else if (expect["output"]["contains"]) {
          // Contains list
          for (const auto& item : expect["output"]["contains"]) {
            output.contains.push_back(item.as<std::string>());
          }
        }
        tc.expected_output = std::move(output);
      }
    }

    // Parse skip_codegen flag (for hierarchy tests that need CLI)
    if (node["skip_codegen"]) {
      tc.skip_codegen = node["skip_codegen"].as<bool>();
    }

    cases.push_back(std::move(tc));
  }

  return cases;
}

}  // namespace lyra::test
