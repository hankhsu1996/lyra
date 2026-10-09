#pragma once

#include <filesystem>
#include <utility>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver {

// A file a request named as something to write, claimed before the work that
// fills it. Claiming is the first step of the write itself -- the directory is
// made and a file begun beside the destination -- so a place that cannot be
// written is refused then, by the same act that would have failed later.
//
// What is written goes to `WorkingPath()`, and `Finish` puts it at the
// destination in one step. A claim dropped unfinished takes what it began with
// it, as does a process asked to end, so the destination holds the whole file
// or what it held before.
class ClaimedFile {
 public:
  static auto Claim(const std::filesystem::path& destination)
      -> diag::Result<ClaimedFile>;

  ClaimedFile(ClaimedFile&& other) noexcept
      : destination_(std::move(other.destination_)),
        working_(std::exchange(other.working_, {})) {
  }
  auto operator=(ClaimedFile&& other) noexcept -> ClaimedFile&;
  ClaimedFile(const ClaimedFile&) = delete;
  auto operator=(const ClaimedFile&) -> ClaimedFile& = delete;
  ~ClaimedFile();

  [[nodiscard]] auto Destination() const -> const std::filesystem::path& {
    return destination_;
  }
  [[nodiscard]] auto WorkingPath() const -> const std::filesystem::path& {
    return working_;
  }

  auto Finish() -> diag::Result<void>;

 private:
  ClaimedFile(std::filesystem::path destination, std::filesystem::path working)
      : destination_(std::move(destination)), working_(std::move(working)) {
  }

  void Drop() noexcept;

  std::filesystem::path destination_;
  // Empty once finished or moved from.
  std::filesystem::path working_;
};

}  // namespace lyra::driver
