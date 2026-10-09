#pragma once

#include <dirent.h>
#include <expected>
#include <filesystem>
#include <string>
#include <utility>

namespace lyra::driver {

// A directory belonging to this process, in the system's temporary directory.
// What a command makes on the way to its product lives here, so an invocation
// leaves behind what it was asked for and nothing else.
//
// It goes with everything in it when the value does, and when the process is
// asked to end. A process killed outright has no turn to remove anything, so
// the directory is held locked for as long as its owner lives, and making one
// removes every other that nobody holds.
class ScratchDirectory {
 public:
  static auto Create() -> std::expected<ScratchDirectory, std::string>;

  ScratchDirectory(ScratchDirectory&& other) noexcept
      : path_(std::exchange(other.path_, {})),
        held_(std::exchange(other.held_, nullptr)) {
  }
  auto operator=(ScratchDirectory&& other) noexcept -> ScratchDirectory&;
  ScratchDirectory(const ScratchDirectory&) = delete;
  auto operator=(const ScratchDirectory&) -> ScratchDirectory& = delete;
  ~ScratchDirectory();

  [[nodiscard]] auto Path() const -> const std::filesystem::path& {
    return path_;
  }

 private:
  ScratchDirectory(std::filesystem::path path, DIR* held)
      : path_(std::move(path)), held_(held) {
  }

  void Remove() noexcept;

  // Empty once moved from, which is what leaves the directory to the value it
  // moved to.
  std::filesystem::path path_;
  // The open directory the lock is on, which the system releases however the
  // process ends.
  DIR* held_ = nullptr;
};

}  // namespace lyra::driver
