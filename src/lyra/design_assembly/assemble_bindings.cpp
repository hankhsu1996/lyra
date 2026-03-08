#include "lyra/design_assembly/assemble_bindings.hpp"

#include <utility>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::design_assembly {

void AssembleBindings(
    CompiledBindingPlan&& plan, mir::Arena& arena, mir::Design& design) {
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

  for (auto& alias : plan.alias_bindings) {
    design.alias_map[alias.child_slot] = alias.parent_place;
    design.port_connections.push_back(
        mir::PortConnection{
            .kind = mir::PortConnection::Kind::kAlias,
            .child_port_sym = alias.child_port_sym,
            .parent_instance_sym = alias.parent_instance_sym,
            .child_place = alias.child_place,
            .parent_place = alias.parent_place,
        });
  }
}

}  // namespace lyra::design_assembly
