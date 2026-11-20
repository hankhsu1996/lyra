#pragma once

#include <filesystem>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/ports/port.hpp"

namespace lyra::cli {

// Beside a project's `lyra.toml`, written by Lyra and committed with the
// project: which port of each supported library the project's build took, so
// every later build, by this Lyra or a later one, takes the same.
inline constexpr std::string_view kLockFileName = "lyra.lock";

// What a lock file records: the port of each supported library the build took
// and the digest of its content, one per library, ordered by name.
struct LockFile {
  std::vector<ports::PortMetadata> libraries;

  auto operator==(const LockFile&) const -> bool = default;

  [[nodiscard]] auto Find(std::string_view name) const
      -> const ports::PortMetadata*;
};

// The lock at `path`, which records nothing where there is no file, its
// libraries in the order of their names however the file lists them. A file
// not as Lyra writes one is refused rather than read as some of what it says.
auto ReadLockFile(const std::filesystem::path& path) -> diag::Result<LockFile>;

// Replaces the lock at `path` whole, so a reader finds the lock as it was or as
// it is now and never part of one.
auto WriteLockFile(const std::filesystem::path& path, const LockFile& lock)
    -> diag::Result<void>;

}  // namespace lyra::cli
