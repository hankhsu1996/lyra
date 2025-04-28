#include "lowering/mir_to_lir/process.hpp"

#include <stdexcept>

#include "lowering/mir_to_lir/lir_builder.hpp"
#include "lowering/mir_to_lir/statement.hpp"
#include "mir/process.hpp"

namespace lyra::lowering {

// Map MIR process kind to LIR process kind
auto MapProcessKind(mir::ProcessKind kind) -> lir::ProcessKind {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return lir::ProcessKind::kInitial;
    case mir::ProcessKind::kAlwaysComb:
    case mir::ProcessKind::kAlwaysFF:
      return lir::ProcessKind::kAlways;
    default:
      throw std::runtime_error("Unknown process kind");
  }
}

// Map MIR trigger to LIR trigger
auto MapTrigger(const mir::Trigger& mir_trigger) -> lir::Trigger {
  if (mir_trigger.variable.name.empty()) {
    throw std::runtime_error("Trigger variable has empty name");
  }

  return lir::Trigger{
      .edge_kind = mir_trigger.edge_kind,
      .variable = mir_trigger.variable.name};
}

auto LowerProcess(const mir::Process& process, LirBuilder& builder) -> void {
  if (process.body.empty()) {
    // Skip empty processes
    return;
  }

  // Begin a new process with the mapped kind
  builder.BeginProcess(MapProcessKind(process.process_kind));

  // Transfer the trigger list
  for (const auto& trigger : process.trigger_list) {
    builder.AddTrigger(MapTrigger(trigger));
  }

  // Process each statement in the process
  for (const auto& statement : process.body) {
    if (!statement) {
      throw std::runtime_error("Null statement in process body");
    }

    LowerStatement(*statement, builder);
  }

  builder.EndProcess();
}
}  // namespace lyra::lowering
