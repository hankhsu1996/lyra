#include "lyra/link/assemble_bindings.hpp"

#include <utility>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::link {

void AssembleBindings(
    mir::CompiledBindingPlan&& plan, mir::Arena& arena, mir::Design& design) {
  for (auto& drive : plan.drive_bindings) {
    mir::ProcessId pid = arena.AddProcess(std::move(drive.body.process));
    design.connection_processes.push_back(pid);
    design.port_connections.push_back(
        mir::PortConnection{
            .kind = drive.kind,
            .child_port_sym = drive.child_port_sym,
            .parent_instance_sym = drive.parent_instance_sym,
            .child_place = drive.child_place,
            .parent_place = drive.parent_place,
        });
  }
}

}  // namespace lyra::link
