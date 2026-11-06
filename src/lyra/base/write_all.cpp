#include "lyra/base/write_all.hpp"

#include <cerrno>
#include <cstddef>
#include <span>
#include <sys/types.h>
#include <system_error>
#include <unistd.h>

#include "lyra/base/errno_as_error_code.hpp"

namespace lyra::base {

auto WriteAll(int fd, std::span<const char> bytes) noexcept -> std::error_code {
  while (!bytes.empty()) {
    const ssize_t written = ::write(fd, bytes.data(), bytes.size());
    if (written < 0) {
      if (errno == EINTR) continue;
      return ErrnoAsErrorCode();
    }
    if (written == 0) return {EIO, std::system_category()};
    bytes = bytes.subspan(static_cast<std::size_t>(written));
  }
  return {};
}

void WriteAsMuchAsTaken(int fd, std::span<const char> bytes) noexcept {
  while (!bytes.empty()) {
    const ssize_t written = ::write(fd, bytes.data(), bytes.size());
    if (written < 0 && errno == EINTR) continue;
    if (written <= 0) return;
    bytes = bytes.subspan(static_cast<std::size_t>(written));
  }
}

}  // namespace lyra::base
