#include "lyra/base/memory_watch.hpp"

#include <cstdlib>
#include <new>
#include <string_view>
#include <unistd.h>

#include "lyra/base/fault_watch.hpp"
#include "lyra/base/write_all.hpp"

namespace lyra::base {

namespace {

auto OutOfMemoryMessage() -> std::string_view& {
  static std::string_view message;
  return message;
}

auto BeforeEndingAction() -> BeforeEnding& {
  static BeforeEnding action = &NothingBeforeEnding;
  return action;
}

}  // namespace

void InstallOutOfMemoryHandler(
    std::string_view message, BeforeEnding before_ending) {
  OutOfMemoryMessage() = message;
  BeforeEndingAction() = before_ending;
  std::set_new_handler(EndForLackOfMemory);
}

void EndForLackOfMemory() {
  BeforeEndingAction()();
  WriteAsMuchAsTaken(STDERR_FILENO, OutOfMemoryMessage());
  std::_Exit(EXIT_FAILURE);
}

}  // namespace lyra::base
