#include "lyra/cli/manifest.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
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

#include <slang/text/SourceLocation.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/diag_message.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/support/assertion_policy.hpp"
#include "lyra/support/simple_identifier.hpp"

namespace lyra::cli {

namespace {

namespace fs = std::filesystem;

constexpr std::string_view kManifestFileName = "lyra.toml";

// Keys naming a property of one invocation or one machine. They are refused
// with the rule rather than as unrecognized, because whoever wrote one had a
// coherent idea and needs to hear why this file is not its home.
constexpr std::array<std::string_view, 14> kInvocationKeys = {
    "out",
    "backend",
    "release",
    "cxx",
    "format",
    "no_pch",
    "rebuild",
    "cache_dir",
    "color",
    "no_color",
    "jobs",
    "time_trace",
    "time_trace_granularity",
    "stats_file"};

auto Contains(std::span<const std::string_view> names, std::string_view name)
    -> bool {
  return std::ranges::find(names, name) != names.end();
}

// The option a key naming a property of one invocation stands for.
auto OptionSpelling(std::string_view key) -> std::string {
  std::string option = "--";
  for (const char c : key) {
    option += c == '_' ? '-' : c;
  }
  return option;
}

// One declaration's text, as the source manager holds it, so a place in it is
// shown the way a place in the design is: by file, line and column, with the
// line under it.
class ManifestText {
 public:
  ManifestText(fs::path path, slang::SourceBuffer buffer)
      : path_(std::move(path)), buffer_(buffer) {
    std::string_view text = buffer.data;
    // The source manager ends every buffer with a terminating zero, which is
    // no part of the file.
    if (text.ends_with('\0')) {
      text.remove_suffix(1);
    }
    text_ = text;
    line_starts_.push_back(0);
    for (std::size_t at = 0; at < text_.size(); ++at) {
      if (text_[at] == '\n') {
        line_starts_.push_back(at + 1);
      }
    }
  }

  [[nodiscard]] auto Path() const -> const fs::path& {
    return path_;
  }

  [[nodiscard]] auto Text() const -> std::string_view {
    return text_;
  }

  // The piece of this text the parser placed a node or a key at.
  [[nodiscard]] auto SpanOf(const toml::source_region& region) const
      -> diag::SourceSpan {
    const slang::SourceLocation begin = LocationOf(region.begin);
    const slang::SourceLocation end = LocationOf(region.end);
    if (!begin.valid()) {
      return {};
    }
    return frontend::SpanOf(
        slang::SourceRange{begin, end.valid() ? end : begin});
  }

 private:
  // The parser counts lines and columns from one, and a column in characters
  // rather than bytes; a position it could not place is no location.
  [[nodiscard]] auto LocationOf(const toml::source_position& position) const
      -> slang::SourceLocation {
    if (!position || position.line > line_starts_.size()) {
      return {};
    }
    std::size_t at = line_starts_[position.line - 1];
    for (toml::source_index column = 1;
         column < position.column && at < text_.size(); ++column) {
      ++at;
      while (at < text_.size() &&
             (static_cast<unsigned char>(text_[at]) & 0xC0U) == 0x80U) {
        ++at;
      }
    }
    return slang::SourceLocation{buffer_.id, at};
  }

  fs::path path_;
  slang::SourceBuffer buffer_;
  std::string_view text_;
  std::vector<std::size_t> line_starts_;
};

template <typename... Args>
auto Refusal(
    diag::SourceSpan at, base::DiagFormatFor<Args...> format, Args&&... args)
    -> diag::Diagnostic {
  return diag::Make(
      at, diag::DiagCode::kHostInvalidManifest, format,
      std::forward<Args>(args)...);
}

template <typename... Args>
auto Refusal(
    const ManifestText& file, const toml::source_region& at,
    base::DiagFormatFor<Args...> format, Args&&... args) -> diag::Diagnostic {
  return Refusal(file.SpanOf(at), format, std::forward<Args>(args)...);
}

template <typename... Args>
auto Fail(
    const ManifestText& file, const toml::source_region& at,
    base::DiagFormatFor<Args...> format, Args&&... args)
    -> std::unexpected<diag::Diagnostic> {
  return std::unexpected(
      Refusal(file, at, format, std::forward<Args>(args)...));
}

auto CheckKeys(
    const ManifestText& file, std::string_view table, const toml::table& node,
    std::span<const std::string_view> known) -> diag::Result<void> {
  for (const auto& entry : node) {
    const std::string_view name = entry.first.str();
    if (Contains(known, name)) {
      continue;
    }
    if (Contains(kInvocationKeys, name)) {
      return std::unexpected(
          Refusal(
              file, entry.first.source(),
              "'[{}] {}' names a property of one invocation or one machine, "
              "not of what the file declares",
              table, name)
              .WithNote(
                  "pass '{}' on the command line instead",
                  OptionSpelling(name)));
    }
    return Fail(
        file, entry.first.source(), "'[{}] {}' is not a recognized key", table,
        name);
  }
  return {};
}

auto ReadStrings(
    const ManifestText& file, std::string_view where, const toml::node* node,
    std::vector<std::string>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* array = node->as_array();
  if (array == nullptr) {
    return Fail(file, node->source(), "'{}' is not an array of strings", where);
  }
  for (const auto& element : *array) {
    const auto* text = element.as_string();
    if (text == nullptr) {
      return Fail(
          file, element.source(), "'{}' is not an array of strings", where);
    }
    out.emplace_back(text->get());
  }
  return {};
}

// A path is resolved here rather than where it is used, because the base is the
// declaring file's directory and nothing downstream knows it. An entry that is
// already absolute is kept as written.
auto ReadPaths(
    const ManifestText& file, std::string_view where, const toml::node* node,
    std::vector<std::string>& out) -> diag::Result<void> {
  std::vector<std::string> written;
  if (auto read = ReadStrings(file, where, node, written); !read) {
    return std::unexpected(std::move(read.error()));
  }
  const fs::path base = file.Path().parent_path();
  for (std::size_t i = 0; i < written.size(); ++i) {
    const std::string& entry = written[i];
    // A pattern names whatever the filesystem happens to hold, which also
    // leaves source order undefined, and source order is significant.
    if (entry.find_first_of("*?[") != std::string::npos ||
        entry.find("...") != std::string::npos) {
      return std::unexpected(
          Refusal(
              file, (*node->as_array())[i].source(),
              "'{}' names '{}', which is a pattern; which files it reads, and "
              "in what order, would be whatever the filesystem holds",
              where, entry)
              .WithNote(
                  "list the files, and find a cell by name with 'searchdir' "
                  "and 'searchext'"));
    }
    out.push_back((base / entry).lexically_normal().string());
  }
  return {};
}

auto ReadString(
    const ManifestText& file, std::string_view where, const toml::node* node,
    std::optional<std::string>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* text = node->as_string();
  if (text == nullptr) {
    return Fail(file, node->source(), "'{}' is not a string", where);
  }
  out = text->get();
  return {};
}

auto ReadBool(
    const ManifestText& file, std::string_view where, const toml::node* node,
    std::optional<bool>& out) -> diag::Result<void> {
  if (node == nullptr) {
    return {};
  }
  const auto* flag = node->as_boolean();
  if (flag == nullptr) {
    return Fail(file, node->source(), "'{}' is neither true nor false", where);
  }
  out = flag->get();
  return {};
}

auto ReadAssertionPolicy(
    const ManifestText& file, const toml::node* node,
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
      file, node->source(),
      "'{}' given to '[compile] assertions' is neither 'check' nor 'skip'",
      *spelled);
}

struct SourceSetField {
  std::string_view key;
  diag::Result<void> (*read)(
      const ManifestText&, std::string_view, const toml::node*,
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
    const ManifestText& file, std::string_view table_name,
    const toml::table& table, std::span<const std::string_view> own_keys,
    SourceSet& out) -> diag::Result<void> {
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

auto ReadLibrary(
    const ManifestText& file, const toml::table& table, DeclaredLibrary& out)
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
    const ManifestText& file, const toml::table& table, DeclaredDesign& out)
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
    const ManifestText& file, const toml::table& table,
    std::vector<DeclaredDependency>& out) -> diag::Result<void> {
  struct Written {
    toml::source_position at;
    DeclaredDependency dependency;
  };
  std::vector<Written> written;
  for (const auto& entry : table) {
    const std::string name{entry.first.str()};
    // A library's name is an identifier (LRM 33.3.1).
    if (!support::IsSimpleIdentifier(name)) {
      return Fail(
          file, entry.first.source(),
          "'[dependencies] {}' is not an identifier, which a library's name "
          "has to be",
          name);
    }
    const auto* spec = entry.second.as_table();
    if (spec == nullptr) {
      return std::unexpected(
          Refusal(
              file, entry.second.source(),
              "'[dependencies] {}' is not a table saying where the library is "
              "declared",
              name)
              .WithNote(
                  "write it as '{}'",
                  std::format("{} = {{ path = \"../{}\" }}", name, name)));
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
          file, entry.first.source(),
          "'[{}]' has no 'path', and a dependency has to say which directory "
          "declares it",
          table_name);
    }
    written.push_back(
        {.at = entry.second.source().begin,
         .dependency = {
             .name = name,
             .manifest =
                 (file.Path().parent_path() / *directory / kManifestFileName)
                     .lexically_normal(),
             .declared_at = file.SpanOf(entry.first.source())}});
  }
  std::ranges::sort(written, {}, [](const Written& entry) {
    return std::pair{entry.at.line, entry.at.column};
  });
  for (Written& entry : written) {
    out.push_back(std::move(entry.dependency));
  }
  return {};
}

auto LoadManifest(const fs::path& path, slang::SourceManager& sources)
    -> diag::Result<Manifest>;

auto Spelled(const std::optional<std::string>& setting) -> std::string {
  return setting ? std::format("'{}'", *setting) : "none";
}

// The libraries a build reaches, gathered by following each declaration's
// dependencies, a library's own before the library.
class ReachedLibraries {
 public:
  ReachedLibraries(const Manifest& root, slang::SourceManager& sources)
      : root_(&root), sources_(&sources) {
    declared_by_.emplace(root.library.name, Canonical(root.path));
  }

  auto Follow(const Manifest& from) -> diag::Result<void> {
    following_.push_back(from.library.name);
    for (const DeclaredDependency& dependency : from.dependencies) {
      if (auto ok = Reach(dependency); !ok) {
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

  // Every refusal is placed at the entry of the depending file that names the
  // dependency, which is where the build reached it from.
  auto Reach(const DeclaredDependency& dependency) -> diag::Result<void> {
    const diag::SourceSpan at = dependency.declared_at;
    const fs::path file = Canonical(dependency.manifest);
    if (const auto met = declared_by_.find(dependency.name);
        met != declared_by_.end()) {
      if (met->second != file) {
        return std::unexpected(Refusal(
            at,
            "'[dependencies] {}' names the library declared by '{}', and the "
            "build already holds a library of that name, declared by '{}'",
            dependency.name, file.string(), met->second.string()));
      }
      if (const auto loop = std::ranges::find(following_, dependency.name);
          loop != following_.end()) {
        std::string chain;
        for (auto link = loop; link != following_.end(); ++link) {
          chain += *link + " -> ";
        }
        return std::unexpected(Refusal(
            at, "'[dependencies] {}' makes a library depend on itself ({}{})",
            dependency.name, chain, dependency.name));
      }
      return {};
    }

    std::error_code ec;
    if (!fs::exists(dependency.manifest, ec)) {
      return std::unexpected(Refusal(
          at, "'[dependencies] {}' names '{}', which does not exist",
          dependency.name, dependency.manifest.string()));
    }
    auto loaded = LoadManifest(dependency.manifest, *sources_);
    if (!loaded) {
      return std::unexpected(std::move(loaded.error()));
    }
    if (loaded->library.name != dependency.name) {
      return std::unexpected(Refusal(
          at, "'[dependencies] {}' names '{}', which declares library '{}'",
          dependency.name, dependency.manifest.string(), loaded->library.name));
    }
    if (auto ok = CheckReadableBesideTheRoot(*loaded, at); !ok) {
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
  [[nodiscard]] auto CheckReadableBesideTheRoot(
      const Manifest& library, diag::SourceSpan at) const
      -> diag::Result<void> {
    if (library.language_version != root_->language_version) {
      return std::unexpected(Refusal(
          at,
          "'[compile] std' is {} for library '{}' and {} for library '{}'; "
          "reading the libraries of one build under different language "
          "versions is not yet supported",
          Spelled(library.language_version), library.library.name,
          Spelled(root_->language_version), root_->library.name));
    }
    if (library.timescale != root_->timescale) {
      return std::unexpected(Refusal(
          at,
          "'[compile] timescale' is {} for library '{}' and {} for library "
          "'{}'; giving the libraries of one build different default time "
          "scales is not yet supported",
          Spelled(library.timescale), library.library.name,
          Spelled(root_->timescale), root_->library.name));
    }
    return {};
  }

  const Manifest* root_;
  slang::SourceManager* sources_;
  std::vector<Manifest> reached_;
  std::map<std::string, fs::path> declared_by_;
  std::vector<std::string> following_;
};

auto LoadManifest(const fs::path& path, slang::SourceManager& sources)
    -> diag::Result<Manifest> {
  // Read through the source manager that shows every diagnostic, so a refusal
  // of this file is shown at its place in it.
  auto buffer = sources.readSource(path);
  if (!buffer) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidManifest, "cannot read '{}' ({})",
        path.string(), buffer.error().message());
  }
  const ManifestText file(path, *buffer);
  const toml::parse_result parsed = toml::parse(file.Text(), path.string());
  if (!parsed) {
    return Fail(
        file, parsed.error().source(), "this text is not valid TOML ({})",
        parsed.error().description());
  }

  const toml::table& root = parsed.table();
  static constexpr std::array<std::string_view, 4> kTables = {
      "library", "design", "dependencies", "compile"};
  for (const auto& entry : root) {
    const std::string_view name = entry.first.str();
    if (!Contains(kTables, name)) {
      if (Contains(kInvocationKeys, name)) {
        return std::unexpected(
            Refusal(
                file, entry.first.source(),
                "'{}' names a property of one invocation or one machine, not "
                "of what the file declares",
                name)
                .WithNote(
                    "pass '{}' on the command line instead",
                    OptionSpelling(name)));
      }
      return Fail(
          file, entry.first.source(), "'[{}]' is not a recognized table", name);
    }
    if (entry.second.as_table() == nullptr) {
      return Fail(file, entry.second.source(), "'{}' is not a table", name);
    }
  }

  Manifest manifest;
  manifest.path = path;
  const auto* library = root.get_as<toml::table>("library");
  if (library != nullptr) {
    if (auto ok = ReadLibrary(file, *library, manifest.library); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  if (const auto* design = root.get_as<toml::table>("design");
      design != nullptr) {
    if (auto ok = ReadDesign(file, *design, manifest.design); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  if (const auto* dependencies = root.get_as<toml::table>("dependencies");
      dependencies != nullptr) {
    if (auto ok = ReadDependencies(file, *dependencies, manifest.dependencies);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  if (const auto* compile = root.get_as<toml::table>("compile");
      compile != nullptr) {
    static constexpr std::array<std::string_view, 4> kKeys = {
        "std", "timescale", "single_unit", "assertions"};
    if (auto ok = CheckKeys(file, "compile", *compile, kKeys); !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadString(
            file, "[compile] std", compile->get("std"),
            manifest.language_version);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadString(
            file, "[compile] timescale", compile->get("timescale"),
            manifest.timescale);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadBool(
            file, "[compile] single_unit", compile->get("single_unit"),
            manifest.single_unit);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
    if (auto ok = ReadAssertionPolicy(
            file, compile->get("assertions"), manifest.assertions);
        !ok) {
      return std::unexpected(std::move(ok.error()));
    }
  }

  // Checked last, so a misspelled table or key is reported as the mistake it is
  // rather than as a missing name. A file with no name is the shape this one is
  // not: a bag of options, which the command line already carries better. With
  // no library table there is nothing in the file to point at.
  if (library == nullptr) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidManifest,
        "'{}' has no '[library]' table, and a library has to say what it is "
        "called",
        path.string());
  }
  if (manifest.library.name.empty()) {
    return Fail(
        file, library->source(),
        "'[library]' has no name, and a library has to say what it is called");
  }
  // A library's name is an identifier (LRM 33.3.1).
  if (!support::IsSimpleIdentifier(manifest.library.name)) {
    return Fail(
        file, library->get("name")->source(),
        "library name '{}' is not an identifier, which a library's name has to "
        "be",
        manifest.library.name);
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

auto LoadDeclarations(const fs::path& path, slang::SourceManager& sources)
    -> diag::Result<Declarations> {
  auto root = LoadManifest(path, sources);
  if (!root) {
    return std::unexpected(std::move(root.error()));
  }
  ReachedLibraries reached(*root, sources);
  if (auto ok = reached.Follow(*root); !ok) {
    return std::unexpected(std::move(ok.error()));
  }
  return Declarations{.dependencies = reached.Take(), .root = *std::move(root)};
}

}  // namespace lyra::cli
