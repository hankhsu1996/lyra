#include "lyra/cli/object_code_switches.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::cli {

namespace {

// The extension a shared library has on the one platform Lyra builds for
// (LRM J.4).
constexpr std::string_view kSharedLibraryExtension = ".so";

constexpr std::string_view kBlanks = " \t\r";

auto IsBlank(char c) -> bool {
  return kBlanks.contains(c);
}

auto Trimmed(std::string_view line) -> std::string_view {
  const std::size_t first = line.find_first_not_of(kBlanks);
  if (first == std::string_view::npos) {
    return {};
  }
  return line.substr(first, line.find_last_not_of(kBlanks) - first + 1);
}

auto Under(const std::filesystem::path& root, std::string_view path)
    -> std::filesystem::path {
  const std::filesystem::path named{path};
  return named.is_absolute() ? named : root / named;
}

// The entries of a bootstrap file (LRM J.4.1): a first line holding
// `#!SV_LIBRARIES`, then one library per line, each preceded by at least one
// blank, among comment lines starting with `#`. The first line is taken with
// its blanks left out, because the standard's own example writes it with a
// blank after `#!`.
auto BootstrapEntries(const std::filesystem::path& file)
    -> diag::Result<std::vector<std::string>> {
  std::ifstream in(file);
  if (!in) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "cannot read the bootstrap file '{}'", file.string());
  }
  std::string line;
  std::getline(in, line);
  std::erase_if(line, IsBlank);
  if (line != "#!SV_LIBRARIES") {
    return diag::Fail(
        diag::DiagCode::kHostInvalidCliArgs,
        "'{}' is no bootstrap file, since its first line is not "
        "'#!SV_LIBRARIES'",
        file.string());
  }
  std::vector<std::string> entries;
  for (std::size_t number = 2; std::getline(in, line); ++number) {
    const std::string_view entry = Trimmed(line);
    if (entry.empty() || entry.front() == '#') {
      continue;
    }
    if (!IsBlank(line.front())) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "line {} of the bootstrap file '{}' names a library with no blank "
          "before it",
          number, file.string());
    }
    if (std::ranges::any_of(entry, IsBlank)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "line {} of the bootstrap file '{}' holds more than one library",
          number, file.string());
    }
    entries.emplace_back(entry);
  }
  return entries;
}

}  // namespace

auto SharedLibrariesNamedBy(std::span<const ObjectCodeSwitch> switches)
    -> diag::Result<std::vector<std::string>> {
  std::filesystem::path root;
  std::vector<std::string> listed;
  std::vector<std::string> named;
  const auto library = [&](std::string_view spelled_by, std::string_view value,
                           std::string_view path) -> diag::Result<std::string> {
    std::filesystem::path file = Under(root, path);
    file += kSharedLibraryExtension;
    std::error_code ec;
    if (!std::filesystem::exists(file, ec)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "'{} {}' names no shared library, since '{}' does not exist",
          spelled_by, value, file.string());
    }
    return file.string();
  };
  for (const ObjectCodeSwitch& given : switches) {
    auto taken = std::visit(
        Overloaded{
            [&](const SvRoot& sv_root) -> diag::Result<void> {
              root = sv_root.directory;
              return {};
            },
            [&](const SvLib& sv_lib) -> diag::Result<void> {
              auto file = library(
                  "-sv_lib", sv_lib.path_without_extension,
                  sv_lib.path_without_extension);
              if (!file) {
                return std::unexpected(std::move(file.error()));
              }
              named.push_back(*std::move(file));
              return {};
            },
            [&](const SvLiblist& sv_liblist) -> diag::Result<void> {
              auto entries =
                  BootstrapEntries(Under(root, sv_liblist.bootstrap_file));
              if (!entries) {
                return std::unexpected(std::move(entries.error()));
              }
              for (const std::string& entry : *entries) {
                auto file =
                    library("-sv_liblist", sv_liblist.bootstrap_file, entry);
                if (!file) {
                  return std::unexpected(std::move(file.error()));
                }
                listed.push_back(*std::move(file));
              }
              return {};
            }},
        given);
    if (!taken) {
      return std::unexpected(std::move(taken.error()));
    }
  }
  listed.insert(listed.end(), named.begin(), named.end());
  return listed;
}

}  // namespace lyra::cli
