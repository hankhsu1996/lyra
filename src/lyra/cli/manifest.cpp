#include "lyra/cli/manifest.hpp"

#include <algorithm>
#include <array>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <toml.hpp>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

namespace {

namespace fs = std::filesystem;

constexpr std::string_view kManifestFileName = "lyra.toml";

// Keys naming a property of one invocation or one machine. They are refused
// with the rule rather than as unrecognized, because whoever wrote one had a
// coherent idea and needs to hear why this file is not its home.
constexpr std::array<std::string_view, 15> kInvocationKeys = {
    "out",        "backend",
    "release",    "cxx",
    "format",     "no_pch",
    "rebuild",    "cache_dir",
    "color",      "no_color",
    "jobs",       "remarks",
    "time_trace", "time_trace_granularity",
    "stats_file"};

auto Contains(std::span<const std::string_view> names, std::string_view name)
    -> bool {
  return std::ranges::find(names, name) != names.end();
}

auto Fail(const fs::path& file, std::string message)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kHostInvalidManifest,
      std::format("{}: {}", file.string(), message));
}

auto CheckKeys(
    const fs::path& file, std::string_view table, const toml::table& node,
    std::span<const std::string_view> known) -> diag::Result<void> {
  for (const auto& entry : node) {
    const std::string_view name = entry.first.str();
    if (Contains(known, name)) {
      continue;
    }
    if (Contains(kInvocationKeys, name)) {
      return Fail(
          file, std::format(
                    "[{}] {}: this names a property of one invocation or one "
                    "machine, not of what the file declares; pass it on the "
                    "command line",
                    table, name));
    }
    return Fail(file, std::format("[{}] {}: unrecognized key", table, name));
  }
  return {};
}

auto ReadStrings(
    const fs::path& file, std::string_view where, const toml::node* node,
    std::vector<std::string>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* array = node->as_array();
  if (array == nullptr) {
    return Fail(file, std::format("{}: expected an array of strings", where));
  }
  for (const auto& element : *array) {
    const auto* text = element.as_string();
    if (text == nullptr) {
      return Fail(file, std::format("{}: expected an array of strings", where));
    }
    out.emplace_back(text->get());
  }
  return {};
}

// A path is resolved here rather than where it is used, because the base is the
// declaring file's directory and nothing downstream knows it. An entry that is
// already absolute is kept as written.
auto ReadPaths(
    const fs::path& file, std::string_view where, const toml::node* node,
    std::vector<std::string>& out) -> diag::Result<void> {
  std::vector<std::string> written;
  if (auto read = ReadStrings(file, where, node, written); !read) {
    return std::unexpected(std::move(read.error()));
  }
  const fs::path base = file.parent_path();
  for (const auto& entry : written) {
    if (entry.find_first_of("*?[") != std::string::npos ||
        entry.find("...") != std::string::npos) {
      return Fail(
          file,
          std::format(
              "{}: '{}' is a pattern. A declaration names its parts, and a "
              "pattern names whatever the filesystem happens to hold -- which "
              "also leaves source order undefined, and source order is "
              "significant. List the files, and use searchdir with searchext "
              "to find a cell by name",
              where, entry));
    }
    out.push_back((base / entry).lexically_normal().string());
  }
  return {};
}

auto ReadString(
    const fs::path& file, std::string_view where, const toml::node* node,
    std::optional<std::string>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* text = node->as_string();
  if (text == nullptr) {
    return Fail(file, std::format("{}: expected a string", where));
  }
  out = text->get();
  return {};
}

auto ReadBool(
    const fs::path& file, std::string_view where, const toml::node* node,
    std::optional<bool>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* flag = node->as_boolean();
  if (flag == nullptr) {
    return Fail(file, std::format("{}: expected true or false", where));
  }
  out = flag->get();
  return {};
}

auto ReadAssertionPolicy(
    const fs::path& file, const toml::node* node,
    std::optional<support::AssertionPolicy>& out) -> diag::Result<void> {
  std::optional<std::string> spelled;
  if (auto read = ReadString(file, "[compile] assertions", node, spelled);
      !read) {
    return std::unexpected(std::move(read.error()));
  }
  if (!spelled) {
    return {};
  }
  if (*spelled == "check") {
    out = support::AssertionPolicy::kCheck;
    return {};
  }
  if (*spelled == "skip") {
    out = support::AssertionPolicy::kSkip;
    return {};
  }
  return Fail(
      file,
      std::format(
          "[compile] assertions: '{}' is not one of check, skip", *spelled));
}

struct SourceSetField {
  std::string_view key;
  diag::Result<void> (*read)(
      const fs::path&, std::string_view, const toml::node*,
      std::vector<std::string>&);
  std::vector<std::string> SourceSet::* member;
};

constexpr std::array<SourceSetField, 7> kSourceSetFields = {
    {{.key = "files", .read = ReadPaths, .member = &SourceSet::files},
     {.key = "incdir", .read = ReadPaths, .member = &SourceSet::incdir},
     {.key = "defines", .read = ReadStrings, .member = &SourceSet::defines},
     {.key = "undefines", .read = ReadStrings, .member = &SourceSet::undefines},
     {.key = "searchdir", .read = ReadPaths, .member = &SourceSet::searchdir},
     {.key = "searchext", .read = ReadStrings, .member = &SourceSet::searchext},
     {.key = "dpi", .read = ReadPaths, .member = &SourceSet::dpi}}};

// A table that holds a source set takes the set's keys beside its own.
auto ReadSourceSet(
    const fs::path& file, std::string_view table_name, const toml::table& table,
    std::span<const std::string_view> own_keys, SourceSet& out)
    -> diag::Result<void> {
  std::vector<std::string_view> known(own_keys.begin(), own_keys.end());
  for (const SourceSetField& field : kSourceSetFields) {
    known.push_back(field.key);
  }
  if (auto ok = CheckKeys(file, table_name, table, known); !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  for (const SourceSetField& field : kSourceSetFields) {
    if (auto ok = field.read(
            file, std::format("[{}] {}", table_name, field.key),
            table.get(field.key), out.*field.member);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }
  return {};
}

// LRM 5.6: a simple identifier.
auto IsSimpleIdentifier(std::string_view text) -> bool {
  const auto is_letter = [](char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
  };
  const auto is_digit = [](char c) { return c >= '0' && c <= '9'; };
  return !text.empty() && is_letter(text.front()) &&
         std::ranges::all_of(text, [&](char c) {
           return is_letter(c) || is_digit(c) || c == '$';
         });
}

auto ReadLibrary(
    const fs::path& file, const toml::table& table, DeclaredLibrary& out)
    -> diag::Result<void> {
  static constexpr std::array<std::string_view, 1> kKeys = {"name"};
  if (auto ok = ReadSourceSet(file, "library", table, kKeys, out.sources);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  std::optional<std::string> name;
  if (auto ok = ReadString(file, "[library] name", table.get("name"), name);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  out.name = name.value_or("");
  return {};
}

auto ReadDesign(
    const fs::path& file, const toml::table& table, DeclaredDesign& out)
    -> diag::Result<void> {
  static constexpr std::array<std::string_view, 2> kKeys = {"top", "params"};
  if (auto ok = ReadSourceSet(file, "design", table, kKeys, out.sources); !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  if (auto ok = ReadStrings(file, "[design] top", table.get("top"), out.top);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  if (auto ok =
          ReadStrings(file, "[design] params", table.get("params"), out.params);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  return {};
}

}  // namespace

auto FindManifest(const fs::path& start) -> ManifestSearch {
  std::error_code ec;
  fs::path dir = fs::absolute(start, ec);
  if (ec) {
    dir = start;
  }
  dir = dir.lexically_normal();
  const fs::path started = dir;
  while (true) {
    if (fs::exists(dir / kManifestFileName, ec)) {
      return ManifestFound{.path = dir / kManifestFileName};
    }
    // A repository boundary ends the search: a declaration above a repository's
    // root belongs to whatever contains that repository.
    if (fs::exists(dir / ".git", ec)) {
      return ManifestAbsent{.started = started, .stopped = dir};
    }
    const fs::path parent = dir.parent_path();
    if (parent.empty() || parent == dir) {
      return ManifestAbsent{.started = started, .stopped = dir};
    }
    dir = parent;
  }
}

auto LoadManifest(const fs::path& path) -> diag::Result<Manifest> {
  const toml::parse_result parsed = toml::parse_file(path.string());
  if (!parsed) {
    const auto& error = parsed.error();
    return Fail(
        path, std::format(
                  "line {}, column {}: {}", error.source().begin.line,
                  error.source().begin.column, error.description()));
  }

  const toml::table& root = parsed.table();
  static constexpr std::array<std::string_view, 3> kTables = {
      "library", "design", "compile"};
  for (const auto& entry : root) {
    const std::string_view name = entry.first.str();
    if (!Contains(kTables, name)) {
      if (Contains(kInvocationKeys, name)) {
        return Fail(
            path,
            std::format(
                "{}: this names a property of one invocation or one machine, "
                "not of what the file declares; pass it on the command line",
                name));
      }
      return Fail(path, std::format("{}: unrecognized table", name));
    }
    if (entry.second.as_table() == nullptr) {
      return Fail(path, std::format("{}: expected a table", name));
    }
  }

  Manifest manifest;
  manifest.path = path;
  if (const auto* library = root.get_as<toml::table>("library");
      library != nullptr) {
    if (auto ok = ReadLibrary(path, *library, manifest.library); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  if (const auto* design = root.get_as<toml::table>("design");
      design != nullptr) {
    if (auto ok = ReadDesign(path, *design, manifest.design); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  if (const auto* compile = root.get_as<toml::table>("compile");
      compile != nullptr) {
    static constexpr std::array<std::string_view, 4> kKeys = {
        "std", "timescale", "single_unit", "assertions"};
    if (auto ok = CheckKeys(path, "compile", *compile, kKeys); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadString(
            path, "[compile] std", compile->get("std"),
            manifest.language_version);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadString(
            path, "[compile] timescale", compile->get("timescale"),
            manifest.timescale);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadBool(
            path, "[compile] single_unit", compile->get("single_unit"),
            manifest.single_unit);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadAssertionPolicy(
            path, compile->get("assertions"), manifest.assertions);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  // Checked last, so a misspelled table or key is reported as the mistake it is
  // rather than as a missing name. A file with no name is the shape this one is
  // not: a bag of options, which the command line already carries better.
  if (manifest.library.name.empty()) {
    return Fail(path, "[library] name: a library has to say what it is called");
  }
  if (!IsSimpleIdentifier(manifest.library.name)) {
    return Fail(
        path, std::format(
                  "[library] name: '{}' is not an identifier, which a "
                  "library's name has to be (LRM 33.3.1)",
                  manifest.library.name));
  }

  return manifest;
}

}  // namespace lyra::cli
