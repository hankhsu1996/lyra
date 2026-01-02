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

    if (node["expect"]) {
      for (const auto& pair : node["expect"]) {
        std::string var_name = pair.first.as<std::string>();
        int64_t value = pair.second.as<int64_t>();
        tc.expected_values[var_name] = value;
      }
    }

    cases.push_back(std::move(tc));
  }

  return cases;
}

}  // namespace lyra::test
