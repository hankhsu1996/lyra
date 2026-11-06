#include "lyra/runtime/finished_output.hpp"

#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/value_dumps.hpp"

namespace lyra::runtime {

// The files first: delivering them waits on nothing, where a dump waits on its
// thread.
void DeliverFinishedOutput() noexcept {
  DeliverFinishedFiles();
  DeliverFinishedDumps();
}

}  // namespace lyra::runtime
