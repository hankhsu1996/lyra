#include "lyra/cli/manifest.hpp"

#include <algorithm>
#include <array>
#include <expected>
#include <filesystem>
#include <format>
#include <map>
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
  static constexpr std::array<std::string_view, 2> kKeys = {
      "name", "export_incdir"};
  if (auto ok = ReadSourceSet(file, "library", table, kKeys, out.sources);
      !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  if (auto ok = ReadPaths(
          file, "[library] export_incdir", table.get("export_incdir"),
          out.export_incdir);
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

// Each entry names a library and the directory its declaration is in. They are
// handed back in the order the file writes them, which the parser does not
// keep: it holds a table sorted by key.
auto ReadDependencies(
    const fs::path& file, const toml::table& table,
    std::vector<DeclaredDependency>& out) -> diag::Result<void> {
  struct Written {
    toml::source_position at;
    DeclaredDependency dependency;
  };
  std::vector<Written> written;
  for (const auto& entry : table) {
    const std::string name{entry.first.str()};
    if (!IsSimpleIdentifier(name)) {
      return Fail(
          file, std::format(
                    "[dependencies] {}: '{}' is not an identifier, which a "
                    "library's name has to be (LRM 33.3.1)",
                    name, name));
    }
    const auto* spec = entry.second.as_table();
    if (spec == nullptr) {
      return Fail(
          file, std::format(
                    "[dependencies] {}: expected a table saying where the "
                    "library is declared, as in {{ path = \"../{}\" }}",
                    name, name));
    }
    static constexpr std::array<std::string_view, 1> kKeys = {"path"};
    const std::string table_name = std::format("dependencies.{}", name);
    if (auto ok = CheckKeys(file, table_name, *spec, kKeys); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    std::optional<std::string> directory;
    if (auto ok = ReadString(
            file, std::format("[{}] path", table_name), spec->get("path"),
            directory);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (!directory) {
      return Fail(
          file, std::format(
                    "[{}] path: a dependency has to say which directory "
                    "declares it",
                    table_name));
    }
    written.push_back(
        {.at = entry.second.source().begin,
         .dependency = {
             .name = name,
             .manifest = (file.parent_path() / *directory / kManifestFileName)
                             .lexically_normal()}});
  }
  std::ranges::sort(written, {}, [](const Written& entry) {
    return std::pair{entry.at.line, entry.at.column};
  });
  for (Written& entry : written) {
    out.push_back(std::move(entry.dependency));
  }
  return {};
}

auto LoadManifest(const fs::path& path) -> diag::Result<Manifest>;

auto Spelled(const std::optional<std::string>& setting) -> std::string {
  return setting ? std::format("'{}'", *setting) : "none";
}

// The libraries a build reaches, gathered by following each declaration's
// dependencies, a library's own before the library.
class ReachedLibraries {
 public:
  explicit ReachedLibraries(const Manifest& root) : root_(&root) {
    declared_by_.emplace(root.library.name, Canonical(root.path));
  }

  auto Follow(const Manifest& from) -> diag::Result<void> {
    following_.push_back(from.library.name);
    for (const DeclaredDependency& dependency : from.dependencies) {
      if (auto ok = Reach(from, dependency); !ok) {
        return std::unexpected(std::move(ok.error()));
      }
    }
    following_.pop_back();
    return {};
  }

  auto Take() -> std::vector<Manifest> {
    return std::move(reached_);
  }

 private:
  static auto Canonical(const fs::path& path) -> fs::path {
    std::error_code ec;
    fs::path canonical = fs::weakly_canonical(path, ec);
    return ec ? path : canonical;
  }

  auto Reach(const Manifest& from, const DeclaredDependency& dependency)
      -> diag::Result<void> {
    const std::string where = std::format("[dependencies] {}", dependency.name);
    const fs::path file = Canonical(dependency.manifest);
    if (const auto met = declared_by_.find(dependency.name);
        met != declared_by_.end()) {
      if (met->second != file) {
        return Fail(
            from.path,
            std::format(
                "{}: this names the library declared by {}, and the build "
                "already holds a library of that name, declared by {}",
                where, file.string(), met->second.string()));
      }
      if (const auto at = std::ranges::find(following_, dependency.name);
          at != following_.end()) {
        std::string chain;
        for (auto link = at; link != following_.end(); ++link) {
          chain += *link + " -> ";
        }
        return Fail(
            from.path, std::format(
                           "{}: a library cannot depend on itself ({}{})",
                           where, chain, dependency.name));
      }
      return {};
    }

    std::error_code ec;
    if (!fs::exists(dependency.manifest, ec)) {
      return Fail(
          from.path,
          std::format(
              "{}: there is no {}", where, dependency.manifest.string()));
    }
    auto loaded = LoadManifest(dependency.manifest);
    if (!loaded) {
      return std::unexpected(std::move(loaded.error()));
    }
    if (loaded->library.name != dependency.name) {
      return Fail(
          from.path, std::format(
                         "{}: {} declares library '{}'", where,
                         dependency.manifest.string(), loaded->library.name));
    }
    if (auto ok = CheckReadableBesideTheRoot(*loaded); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    declared_by_.emplace(dependency.name, file);
    if (auto ok = Follow(*loaded); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    reached_.push_back(*std::move(loaded));
    return {};
  }

  // The front end reads one build under one language version and one default
  // time scale.
  [[nodiscard]] auto CheckReadableBesideTheRoot(const Manifest& library) const
      -> diag::Result<void> {
    if (library.language_version != root_->language_version) {
      return Fail(
          library.path,
          std::format(
              "[compile] std: library '{}' is declared for {} and {} for {}; "
              "reading the libraries of one build under different language "
              "versions is not yet supported",
              library.library.name, Spelled(library.language_version),
              root_->path.string(), Spelled(root_->language_version)));
    }
    if (library.timescale != root_->timescale) {
      return Fail(
          library.path,
          std::format(
              "[compile] timescale: library '{}' is declared for {} and {} "
              "for {}; giving the libraries of one build different default "
              "time scales is not yet supported",
              library.library.name, Spelled(library.timescale),
              root_->path.string(), Spelled(root_->timescale)));
    }
    return {};
  }

  const Manifest* root_;
  std::vector<Manifest> reached_;
  std::map<std::string, fs::path> declared_by_;
  std::vector<std::string> following_;
};

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
  static constexpr std::array<std::string_view, 4> kTables = {
      "library", "design", "dependencies", "compile"};
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

  if (const auto* dependencies = root.get_as<toml::table>("dependencies");
      dependencies != nullptr) {
    if (auto ok = ReadDependencies(path, *dependencies, manifest.dependencies);
        !ok) {
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

auto LoadDeclarations(const fs::path& path) -> diag::Result<Declarations> {
  auto root = LoadManifest(path);
  if (!root) {
    return std::unexpected(std::move(root.error()));
  }
  ReachedLibraries reached(*root);
  if (auto ok = reached.Follow(*root); !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  return Declarations{.dependencies = reached.Take(), .root = *std::move(root)};
}

}  // namespace lyra::cli
