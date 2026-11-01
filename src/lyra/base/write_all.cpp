#include "lyra/base/write_all.hpp"

#include <cerrno>
#include <cstddef>
#include <span>
#include <sys/types.h>
#include <unistd.h>

namespace lyra::base {

void WriteAll(int fd, std::span<const char> bytes) noexcept {
  while (!bytes.empty()) {
    const ssize_t written = ::write(fd, bytes.data(), bytes.size());
    if (written <= 0) {
      if (written < 0 && errno == EINTR) continue;
      return;
    }
    bytes = bytes.subspan(static_cast<std::size_t>(written));
  }
}

}  // namespace lyra::base
