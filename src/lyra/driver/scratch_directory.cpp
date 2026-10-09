#include "lyra/driver/scratch_directory.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <dirent.h>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <string_view>
#include <sys/file.h>
#include <sys/stat.h>
#include <system_error>
#include <utility>

#include "lyra/driver/signals.hpp"

namespace lyra::driver {

namespace {

// What every such directory's name starts with, and so what tells one from
// anything else in the temporary directory.
constexpr std::string_view kNamePrefix = "lyra-build-";

// Opens `dir` and takes the lock on it, answering the open directory, or
// nothing where it cannot be opened or someone holds it.
auto TryHold(const std::filesystem::path& dir) -> DIR* {
  DIR* const opened = ::opendir(dir.c_str());
  if (opened == nullptr) {
    return nullptr;
  }
  if (::flock(::dirfd(opened), LOCK_EX | LOCK_NB) != 0) {
    ::closedir(opened);
    return nullptr;
  }
  return opened;
}

// Removes every directory of this kind under `parent` that nobody holds: what
// a process left that ended without a turn to remove it.
void RemoveAbandoned(const std::filesystem::path& parent) {
  std::error_code ec;
  for (const auto& entry : std::filesystem::directory_iterator(parent, ec)) {
    if (!entry.path().filename().string().starts_with(kNamePrefix)) {
      continue;
    }
    if (DIR* const held = TryHold(entry.path()); held != nullptr) {
      std::error_code ignored;
      std::filesystem::remove_all(entry.path(), ignored);
      ::closedir(held);
    }
  }
}

}  // namespace

auto ScratchDirectory::Create()
    -> std::expected<ScratchDirectory, std::string> {
  std::error_code ec;
  const std::filesystem::path parent = std::filesystem::temp_directory_path(ec);
  if (ec) {
    return std::unexpected(
        std::format("there is no temporary directory: {}", ec.message()));
  }
  RemoveAbandoned(parent);

  // Between a directory being made and its lock being taken, another process
  // making its own may find it unheld and remove it. Whoever takes the lock
  // second sees which happened: the lock refused, or a directory that is gone.
  constexpr int kAttempts = 16;
  for (int attempt = 0; attempt < kAttempts; ++attempt) {
    std::string name = (parent / std::format("{}XXXXXX", kNamePrefix)).string();
    if (::mkdtemp(name.data()) == nullptr) {
      return std::unexpected(
          std::format("mkdtemp('{}') failed: {}", name, std::strerror(errno)));
    }
    DIR* const held = TryHold(name);
    if (held == nullptr) {
      continue;
    }
    struct stat made{};
    if (::fstat(::dirfd(held), &made) != 0 || made.st_nlink == 0) {
      ::closedir(held);
      continue;
    }
    RemoveOnSignal(name);
    return ScratchDirectory(std::filesystem::path(std::move(name)), held);
  }
  return std::unexpected(
      std::format(
          "no directory made under '{}' could be kept", parent.string()));
}

auto ScratchDirectory::operator=(ScratchDirectory&& other) noexcept
    -> ScratchDirectory& {
  if (this != &other) {
    Remove();
    path_ = std::exchange(other.path_, {});
    held_ = std::exchange(other.held_, nullptr);
  }
  return *this;
}

ScratchDirectory::~ScratchDirectory() {
  Remove();
}

void ScratchDirectory::Remove() noexcept {
  if (path_.empty()) {
    return;
  }
  std::error_code ignored;
  std::filesystem::remove_all(path_, ignored);
  DontRemoveOnSignal(path_);
  ::closedir(held_);
}

}  // namespace lyra::driver
