#include "tests/diag_framework/yaml_parser.hpp"

#include <filesystem>
#include <format>
#include <stdexcept>
#include <string>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::test {
namespace {

auto ParseKind(const std::string& s) -> diag::DiagKind {
  if (s == "error") return diag::DiagKind::kError;
  if (s == "unsupported") return diag::DiagKind::kUnsupported;
  if (s == "host_error") return diag::DiagKind::kHostError;
  if (s == "warning") return diag::DiagKind::kWarning;
  throw std::runtime_error(
      std::format("expect.diagnostics: unknown kind '{}'", s));
}

auto ParseCategory(const std::string& s) -> diag::UnsupportedCategory {
  if (s == "type") return diag::UnsupportedCategory::kType;
  if (s == "operation") return diag::UnsupportedCategory::kOperation;
  if (s == "feature") return diag::UnsupportedCategory::kFeature;
  throw std::runtime_error(
      std::format("expect.diagnostics: unknown category '{}'", s));
}

}  // namespace

auto ParseExpectedDiag(const YAML::Node& node) -> ExpectedDiag {
  if (!node || !node.IsMap()) {
    throw std::runtime_error("expect.diagnostics entry must be a map");
  }

  if (!node["code"]) {
    throw std::runtime_error("expect.diagnostics: 'code' is required");
  }
  const auto code_text = node["code"].as<std::string>();
  auto resolved = diag::ParseDiagCode(code_text);
  if (!resolved) {
    throw std::runtime_error(
        std::format("expect.diagnostics: unknown code '{}'", code_text));
  }
  ExpectedDiag out(*resolved);

  if (node["kind"]) {
    out.kind = ParseKind(node["kind"].as<std::string>());
  }

  if (node["category"]) {
    if (out.kind && *out.kind != diag::DiagKind::kUnsupported) {
      throw std::runtime_error(
          "expect.diagnostics: 'category' is only valid when kind=unsupported");
    }
    out.category = ParseCategory(node["category"].as<std::string>());
  }

  if (node["file"]) {
    out.file = node["file"].as<std::string>();
  }

  if (node["line"]) {
    const auto v = node["line"].as<int>();
    if (v <= 0) {
      throw std::runtime_error(
          std::format("expect.diagnostics: 'line' must be > 0 (got {})", v));
    }
    out.line = v;
  }

  return out;
}

auto ParseExpectedDiagList(const YAML::Node& seq) -> std::vector<ExpectedDiag> {
  std::vector<ExpectedDiag> out;
  if (!seq) return out;
  if (!seq.IsSequence()) {
    throw std::runtime_error("expect.diagnostics must be a sequence");
  }
  out.reserve(seq.size());
  for (const auto& item : seq) {
    out.push_back(ParseExpectedDiag(item));
  }
  return out;
}

auto LoadExpectedDiagnostics(const std::filesystem::path& case_yaml_path)
    -> std::vector<ExpectedDiag> {
  YAML::Node root = YAML::LoadFile(case_yaml_path.string());
  const auto& expect = root["expect"];
  if (!expect || !expect.IsMap()) {
    return {};
  }
  return ParseExpectedDiagList(expect["diagnostics"]);
}

auto LoadExpectedDiagnostics(const TestCase& tc) -> std::vector<ExpectedDiag> {
  return LoadExpectedDiagnostics(tc.case_yaml_path);
}

}  // namespace lyra::test
