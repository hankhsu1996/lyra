#include "lowering/mir_to_lir/process.hpp"

#include "lowering/mir_to_lir/lir_builder.hpp"
#include "lowering/mir_to_lir/statement.hpp"
#include "mir/process.hpp"

namespace lyra::lowering {

auto LowerProcess(const mir::Process& process, LirBuilder& builder) -> void {
  if (process.body.empty()) {
    // Skip empty processes
    return;
  }

  // Begin a new process with the appropriate kind
  builder.BeginProcess(static_cast<lir::ProcessKind>(process.process_kind));

  // Process each statement in the process
  for (const auto& statement : process.body) {
    LowerStatement(*statement, builder);
  }

  builder.EndProcess();
}
}  // namespace lyra::lowering
