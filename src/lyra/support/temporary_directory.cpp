#include "lyra/support/temporary_directory.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <system_error>
#include <utility>

namespace lyra::support {

auto TemporaryDirectory::Create()
    -> std::expected<TemporaryDirectory, std::string> {
  const auto base = std::filesystem::temp_directory_path() / "lyra-XXXXXX";
  std::string templ = base.string();
  if (mkdtemp(templ.data()) == nullptr) {
    return std::unexpected(
        std::format(
            "mkdtemp('{}') failed: {}", base.string(), std::strerror(errno)));
  }
  return TemporaryDirectory(std::filesystem::path(std::move(templ)));
}

auto TemporaryDirectory::operator=(TemporaryDirectory&& other) noexcept
    -> TemporaryDirectory& {
  if (this != &other) {
    Remove();
    path_ = std::exchange(other.path_, {});
  }
  return *this;
}

TemporaryDirectory::~TemporaryDirectory() {
  Remove();
}

void TemporaryDirectory::Remove() noexcept {
  if (path_.empty()) {
    return;
  }
  std::error_code ignored;
  std::filesystem::remove_all(path_, ignored);
}

}  // namespace lyra::support
