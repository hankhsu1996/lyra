#include "lyra/config/project_config.hpp"

#include <filesystem>
#include <format>
#include <optional>
#include <string>

#include "lyra/common/diagnostic.hpp"

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-literal-operator"
#elif defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-literal-operator"
#endif
#include "toml.hpp"
#if defined(__clang__)
#pragma clang diagnostic pop
#elif defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

namespace lyra::config {

namespace fs = std::filesystem;

auto FindConfig(const fs::path& start_dir) -> std::optional<fs::path> {
  fs::path dir = fs::absolute(start_dir);

  while (true) {
    fs::path config_path = dir / "lyra.toml";
    if (fs::exists(config_path)) {
      return config_path;
    }

    fs::path parent = dir.parent_path();
    if (parent == dir) {
      // Reached root
      return std::nullopt;
    }
    dir = parent;
  }
}

auto LoadConfig(const fs::path& config_path) -> ProjectConfig {
  ProjectConfig config;
  config.root_dir = config_path.parent_path();

  toml::table tbl;
  try {
    tbl = toml::parse_file(config_path.string());
  } catch (const toml::parse_error& e) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, std::format(
                    "failed to parse {}: {}", config_path.string(), e.what())));
  }

  // [package] section
  auto package = tbl["package"];
  if (!package) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, std::format(
                    "{}: missing [package] section", config_path.string())));
  }

  auto name = package["name"].value<std::string>();
  if (!name) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, std::format(
                    "{}: missing required field 'package.name'",
                    config_path.string())));
  }
  config.name = *name;

  auto top = package["top"].value<std::string>();
  if (!top) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, std::format(
                    "{}: missing required field 'package.top'",
                    config_path.string())));
  }
  config.top = *top;

  // [sources] section
  auto sources = tbl["sources"];
  if (!sources) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, std::format(
                    "{}: missing [sources] section", config_path.string())));
  }

  auto* files_arr = sources["files"].as_array();
  if (files_arr == nullptr || files_arr->empty()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {},
            std::format(
                "{}: missing or empty 'sources.files'", config_path.string())));
  }
  for (const auto& elem : *files_arr) {
    if (auto str = elem.value<std::string>()) {
      // Resolve relative paths against config directory
      fs::path file_path = *str;
      if (file_path.is_relative()) {
        file_path = config.root_dir / file_path;
      }
      config.files.push_back(file_path.string());
    }
  }

  // Optional: incdir
  if (auto* incdir_arr = sources["incdir"].as_array()) {
    for (const auto& elem : *incdir_arr) {
      if (auto str = elem.value<std::string>()) {
        fs::path inc_path = *str;
        if (inc_path.is_relative()) {
          inc_path = config.root_dir / inc_path;
        }
        config.incdir.push_back(inc_path.string());
      }
    }
  }

  // [build] section (optional)
  if (auto build = tbl["build"]) {
    if (auto out_dir = build["out_dir"].value<std::string>()) {
      config.out_dir = *out_dir;
    }
  }

  return config;
}

}  // namespace lyra::config
