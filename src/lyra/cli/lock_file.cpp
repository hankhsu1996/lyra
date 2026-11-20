#include "lyra/cli/lock_file.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <string_view>
#include <system_error>
#include <toml.hpp>
#include <utility>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/claimed_file.hpp"
#include "lyra/ports/port.hpp"

namespace lyra::cli {

namespace {

namespace fs = std::filesystem;

// What a lock file's layout is, so one a later Lyra writes in another is
// refused rather than misread.
constexpr std::int64_t kLockFormat = 1;

auto NotALock(const fs::path& path) -> diag::Diagnostic {
  return diag::Make(
             diag::DiagCode::kHostInvalidManifest,
             "'{}' is not a lock file as Lyra writes one", path.string())
      .WithNote(
          "delete it and build once to write it again, and commit what is "
          "written");
}

// Whether `table` holds exactly `keys`, so a key Lyra never writes is not
// passed over as though it said nothing.
template <std::size_t N>
auto HoldsExactly(
    const toml::table& table, const std::array<std::string_view, N>& keys)
    -> bool {
  if (table.size() != keys.size()) {
    return false;
  }
  return std::ranges::all_of(
      keys, [&](std::string_view key) { return table.contains(key); });
}

}  // namespace

auto LockFile::Find(std::string_view name) const -> const ports::PortMetadata* {
  const auto found = std::ranges::find(
      libraries, name,
      [](const ports::PortMetadata& library) -> std::string_view {
        return library.identity.name;
      });
  return found == libraries.end() ? nullptr : &*found;
}

auto ReadLockFile(const fs::path& path) -> diag::Result<LockFile> {
  std::error_code looking;
  if (!fs::exists(path, looking)) {
    return LockFile{};
  }
  const toml::parse_result parsed = toml::parse_file(path.string());
  if (!parsed) {
    return diag::Fail(
        diag::DiagCode::kHostInvalidManifest, "'{}' cannot be read ({})",
        path.string(), parsed.error().description());
  }
  const toml::table& table = parsed.table();
  if (table["format"].value<std::int64_t>() != kLockFormat) {
    return std::unexpected(NotALock(path));
  }
  static constexpr std::array<std::string_view, 2> kFileKeys = {
      "format", "library"};
  static constexpr std::array<std::string_view, 1> kEmptyFileKeys = {"format"};
  const bool empty = !table.contains("library");
  if (empty ? !HoldsExactly(table, kEmptyFileKeys)
            : !HoldsExactly(table, kFileKeys)) {
    return std::unexpected(NotALock(path));
  }
  LockFile lock;
  if (empty) {
    return lock;
  }
  const toml::array* entries = table["library"].as_array();
  if (entries == nullptr || !entries->is_array_of_tables()) {
    return std::unexpected(NotALock(path));
  }
  static constexpr std::array<std::string_view, 4> kLibraryKeys = {
      "name", "version", "port_version", "digest"};
  for (const toml::node& entry : *entries) {
    const toml::table& library = *entry.as_table();
    const auto name = library["name"].value<std::string>();
    const auto version = library["version"].value<std::string>();
    const auto port_version = library["port_version"].value<std::int64_t>();
    const auto digest = library["digest"].value<std::string>();
    if (!HoldsExactly(library, kLibraryKeys) || !name || !version ||
        !port_version || !digest) {
      return std::unexpected(NotALock(path));
    }
    if (lock.Find(*name) != nullptr) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidManifest,
          "'{}' records library '{}' twice", path.string(), *name);
    }
    lock.libraries.push_back(
        ports::PortMetadata{
            .identity =
                {.name = *name,
                 .version = *version,
                 .port_version = *port_version},
            .digest = *digest});
  }
  // Two changes each adding a library may merge into a file listing them out
  // of order; it records the same, and is read as the same.
  std::ranges::sort(
      lock.libraries, {},
      [](const ports::PortMetadata& library) -> const std::string& {
        return library.identity.name;
      });
  return lock;
}

auto WriteLockFile(const fs::path& path, const LockFile& lock)
    -> diag::Result<void> {
  std::string text = std::format(
      "# Written by Lyra: the port of each supported library this project is\n"
      "# built with. Commit it with the project.\nformat = {}\n",
      kLockFormat);
  for (const ports::PortMetadata& library : lock.libraries) {
    text +=
        std::format("\n[[library]]\n{}", ports::RenderPortMetadata(library));
  }
  auto claimed = driver::ClaimedFile::Claim(path);
  if (!claimed) {
    return std::unexpected(std::move(claimed.error()));
  }
  {
    std::ofstream out(
        claimed->WorkingPath(), std::ios::binary | std::ios::trunc);
    out << text;
    out.close();
    if (!out) {
      return diag::Fail(
          diag::DiagCode::kHostIoError, "cannot write '{}'", path.string());
    }
  }
  return claimed->Finish();
}

}  // namespace lyra::cli
