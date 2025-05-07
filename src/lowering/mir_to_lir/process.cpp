#include "lowering/mir_to_lir/process.hpp"

#include "lowering/mir_to_lir/lir_builder.hpp"
#include "lowering/mir_to_lir/statement.hpp"
#include "mir/process.hpp"

namespace lyra::lowering {

// Map MIR process kind to LIR process kind
auto MapProcessKind(mir::ProcessKind kind) -> lir::ProcessKind {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return lir::ProcessKind::kInitial;
    case mir::ProcessKind::kAlways:
      return lir::ProcessKind::kAlways;
  }
}

auto LowerProcess(const mir::Process& process, LirBuilder& builder) -> void {
  if (!process.body) {
    // Skip empty processes
    return;
  }

  // Begin a new process with the mapped kind
  builder.BeginProcess(MapProcessKind(process.process_kind), process.name);

  // Start with an "entry" block for the process
  builder.StartBlock("entry");

  // Process the body statement in the process
  LowerStatement(*process.body, builder);

  builder.EndProcess();
}
}  // namespace lyra::lowering
