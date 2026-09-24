#pragma once

#include <expected>
#include <filesystem>
#include <string>
#include <utility>

namespace lyra::support {

// A directory belonging to this process, removed with everything in it when
// the value goes. What a command makes on the way to its product lives here, so
// an invocation leaves behind what it was asked for and nothing else.
class TemporaryDirectory {
 public:
  static auto Create() -> std::expected<TemporaryDirectory, std::string>;

  TemporaryDirectory(TemporaryDirectory&& other) noexcept
      : path_(std::exchange(other.path_, {})) {
  }
  auto operator=(TemporaryDirectory&& other) noexcept -> TemporaryDirectory&;
  TemporaryDirectory(const TemporaryDirectory&) = delete;
  auto operator=(const TemporaryDirectory&) -> TemporaryDirectory& = delete;
  ~TemporaryDirectory();

  [[nodiscard]] auto Path() const -> const std::filesystem::path& {
    return path_;
  }

 private:
  explicit TemporaryDirectory(std::filesystem::path path)
      : path_(std::move(path)) {
  }

  void Remove() noexcept;

  // Empty once moved from, which is what leaves the directory to the value it
  // moved to.
  std::filesystem::path path_;
};

}  // namespace lyra::support
