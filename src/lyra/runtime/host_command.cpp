#include "lyra/runtime/host_command.hpp"

#include <cstdio>
#include <cstdlib>
#include <string>

#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// The command runs as a child of this process and inherits its output, so
// whatever the design has written but not yet handed to the operating system
// would surface after the command's own output, and a file the design just
// wrote would reach the command incomplete.
void PublishPendingOutput(RuntimeEffects& runtime) {
  runtime.Files().Flush();
  std::fflush(nullptr);
}

}  // namespace

auto RunHostCommand(RuntimeEffects& runtime, const value::String& command)
    -> value::PackedArray {
  PublishPendingOutput(runtime);
  const std::string terminal_command_line{command.View()};
  return value::PackedArray::Int(std::system(terminal_command_line.c_str()));
}

auto RunHostCommand() -> value::PackedArray {
  return value::PackedArray::Int(std::system(nullptr));
}

}  // namespace lyra::runtime
