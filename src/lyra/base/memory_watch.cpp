#include "lyra/base/memory_watch.hpp"

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <new>
#include <string_view>
#include <sys/types.h>
#include <unistd.h>

namespace lyra::base {

namespace {

auto OutOfMemoryMessage() -> std::string_view& {
  static std::string_view message;
  return message;
}

}  // namespace

void InstallOutOfMemoryHandler(std::string_view message) {
  OutOfMemoryMessage() = message;
  std::set_new_handler(EndForLackOfMemory);
}

void EndForLackOfMemory() {
  std::cout.flush();
  std::fflush(stdout);
  std::string_view unwritten = OutOfMemoryMessage();
  while (!unwritten.empty()) {
    const ssize_t written =
        ::write(STDERR_FILENO, unwritten.data(), unwritten.size());
    if (written <= 0) {
      break;
    }
    unwritten.remove_prefix(static_cast<std::size_t>(written));
  }
  std::_Exit(EXIT_FAILURE);
}

}  // namespace lyra::base
