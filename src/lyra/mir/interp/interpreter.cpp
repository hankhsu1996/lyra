#include "lyra/mir/interp/interpreter.hpp"

#include <cstddef>
#include <cstdint>
#include <exception>
#include <format>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/runtime/engine.hpp"

namespace lyra::mir::interp {

namespace {

// Collects storage requirements and type information for locals, temps, and
// design slots.
//
// INVARIANT: This assumes all storage is discoverable via instruction operands.
// Any MIR feature that introduces implicit storage (e.g., call frames, hidden
// temporaries) will require revisiting this logic.
struct StorageCollector {
  std::vector<TypeId> local_types;
  std::vector<TypeId> temp_types;
  std::vector<TypeId> design_types;

  void VisitRoot(const PlaceRoot& root) {
    switch (root.kind) {
      case PlaceRoot::Kind::kLocal: {
        auto idx = static_cast<size_t>(root.id);
        if (idx >= local_types.size()) {
          local_types.resize(idx + 1, kInvalidTypeId);
        }
        if (!local_types[idx]) {
          local_types[idx] = root.type;
        }
        break;
      }
      case PlaceRoot::Kind::kTemp: {
        auto idx = static_cast<size_t>(root.id);
        if (idx >= temp_types.size()) {
          temp_types.resize(idx + 1, kInvalidTypeId);
        }
        if (!temp_types[idx]) {
          temp_types[idx] = root.type;
        }
        break;
      }
      case PlaceRoot::Kind::kDesign: {
        auto idx = static_cast<size_t>(root.id);
        if (idx >= design_types.size()) {
          design_types.resize(idx + 1, kInvalidTypeId);
        }
        if (!design_types[idx]) {
          design_types[idx] = root.type;
        }
        break;
      }
    }
  }

  void Visit(const Place& place, const Arena& arena) {
    VisitRoot(place.root);
    // Also visit operands inside projections (dynamic indices may reference
    // temps/locals that aren't in the instruction operand list)
    for (const auto& proj : place.projections) {
      std::visit(
          common::Overloaded{
              [](const FieldProjection&) {},
              [&](const IndexProjection& p) { Visit(p.index, arena); },
              [&](const SliceProjection& p) { Visit(p.start, arena); },
              [](const DerefProjection&) {},
              [&](const BitRangeProjection& p) { Visit(p.bit_offset, arena); },
              [](const UnionMemberProjection&) {},
          },
          proj.info);
    }
  }

  void Visit(const Operand& op, const Arena& arena) {
    if (op.kind == Operand::Kind::kUse) {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      Visit(arena[place_id], arena);
    }
  }

  void Visit(const Rvalue& rv, const Arena& arena) {
    for (const auto& op : rv.operands) {
      Visit(op, arena);
    }
    // Visit receiver for pop methods that mutate
    if (const auto* info = std::get_if<BuiltinCallRvalueInfo>(&rv.info)) {
      if (info->receiver) {
        Visit(arena[*info->receiver], arena);
      }
    }
    // Visit output place for $value$plusargs
    if (const auto* info = std::get_if<PlusargsRvalueInfo>(&rv.info)) {
      if (info->output) {
        Visit(arena[*info->output], arena);
      }
    }
  }

  // Uses exhaustive Overloaded pattern - adding a new Instruction or EffectOp
  // type will cause a compile error until handled here.
  void Visit(const Instruction& inst, const Arena& arena) {
    std::visit(
        common::Overloaded{
            [&](const Assign& i) {
              Visit(arena[i.target], arena);
              Visit(i.source, arena);
            },
            [&](const Compute& i) {
              Visit(arena[i.target], arena);
              Visit(i.value, arena);
            },
            [&](const GuardedAssign& i) {
              Visit(arena[i.target], arena);
              Visit(i.source, arena);
              Visit(i.validity, arena);
            },
            [&](const Effect& i) {
              std::visit(
                  common::Overloaded{
                      [&](const DisplayEffect& d) -> void {
                        if (d.descriptor) {
                          Visit(*d.descriptor, arena);
                        }
                        for (const auto& format_op : d.ops) {
                          if (format_op.value.has_value()) {
                            Visit(*format_op.value, arena);
                          }
                        }
                      },
                      [&](const SeverityEffect& s) -> void {
                        for (const auto& arg : s.args) {
                          Visit(arg, arena);
                        }
                      },
                      [&](const MemIOEffect& m) -> void {
                        Visit(arena[m.target], arena);
                        Visit(m.filename, arena);
                        if (m.start_addr) {
                          Visit(*m.start_addr, arena);
                        }
                        if (m.end_addr) {
                          Visit(*m.end_addr, arena);
                        }
                      },
                      [&](const FcloseEffect& f) -> void {
                        Visit(f.descriptor, arena);
                      },
                  },
                  i.op);
            },
            [&](const NonBlockingAssign& i) {
              Visit(arena[i.target], arena);
              Visit(i.source, arena);
            },
        },
        inst.data);
  }
};

}  // namespace

auto CreateProcessState(
    const Arena& arena, const TypeArena& types, ProcessId process_id,
    DesignState* design_state) -> ProcessState {
  const auto& process = arena[process_id];

  // Scan all blocks to collect local/temp storage requirements and types
  StorageCollector collector;
  for (const BasicBlock& block : process.blocks) {
    for (const auto& inst : block.instructions) {
      collector.Visit(inst, arena);
    }
  }

  // Initialize locals with default values based on their types
  std::vector<RuntimeValue> locals;
  locals.reserve(collector.local_types.size());
  for (TypeId type_id : collector.local_types) {
    locals.push_back(
        type_id ? CreateDefaultValue(types, type_id) : RuntimeValue{});
  }

  // Initialize temps with default values based on their types
  std::vector<RuntimeValue> temps;
  temps.reserve(collector.temp_types.size());
  for (TypeId type_id : collector.temp_types) {
    temps.push_back(
        type_id ? CreateDefaultValue(types, type_id) : RuntimeValue{});
  }

  return ProcessState{
      .process = process_id,
      .current_block = process.entry,
      .instruction_index = 0,
      .frame = Frame(std::move(locals), std::move(temps)),
      .design_state = design_state,
      .status = ProcessStatus::kRunning,
      .pending_suspend = std::nullopt,
  };
}

auto CreateDesignState(
    const Arena& arena, const TypeArena& types, const Design& design)
    -> DesignState {
  // Collect design slot types by scanning all processes
  StorageCollector collector;
  for (const auto& element : design.elements) {
    if (const auto* module = std::get_if<Module>(&element)) {
      for (ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        for (const BasicBlock& block : process.blocks) {
          for (const auto& inst : block.instructions) {
            collector.Visit(inst, arena);
          }
        }
      }
    }
  }
  for (ProcessId process_id : design.init_processes) {
    const auto& process = arena[process_id];
    for (const BasicBlock& block : process.blocks) {
      for (const auto& inst : block.instructions) {
        collector.Visit(inst, arena);
      }
    }
  }

  // Initialize design storage with default values based on types
  std::vector<RuntimeValue> storage;
  storage.resize(design.num_design_slots);
  for (size_t i = 0; i < collector.design_types.size(); ++i) {
    if (i < storage.size() && collector.design_types[i]) {
      storage[i] = CreateDefaultValue(types, collector.design_types[i]);
    }
  }

  DesignState state(0);  // Create with 0 slots, then move storage in
  state.storage = std::move(storage);
  return state;
}

auto FindInitialModule(const Design& design, const Arena& arena)
    -> std::optional<InitialModuleInfo> {
  for (const auto& element : design.elements) {
    if (const auto* module = std::get_if<Module>(&element)) {
      // Collect all kOnce processes (initial blocks) in module order
      std::vector<ProcessId> initial_processes;
      for (ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        if (process.kind == ProcessKind::kOnce) {
          initial_processes.push_back(process_id);
        }
      }
      if (!initial_processes.empty()) {
        return InitialModuleInfo{
            .module = module,
            .initial_processes = std::move(initial_processes),
        };
      }
    }
  }
  return std::nullopt;
}

auto RunSimulation(
    const Design& design, const Arena& mir_arena, const TypeArena& types,
    std::ostream* output, std::span<const std::string> plusargs,
    const lowering::DiagnosticContext* diag_ctx) -> SimulationResult {
  // Find initial module
  auto module_info = FindInitialModule(design, mir_arena);
  if (!module_info) {
    return SimulationResult{
        .exit_code = 1, .error_message = "no initial process found"};
  }

  // Create design state
  auto design_state = CreateDesignState(mir_arena, types, design);

  // Create interpreter
  Interpreter interp(&mir_arena, &types, diag_ctx);
  if (output != nullptr) {
    interp.SetOutput(output);
  } else {
    interp.SetOutput(&std::cout);
  }
  interp.SetPlusargs(
      std::vector<std::string>(plusargs.begin(), plusargs.end()));

  // Run design init processes (package variable initialization)
  for (ProcessId proc_id : design.init_processes) {
    auto state = CreateProcessState(mir_arena, types, proc_id, &design_state);
    auto result = interp.Run(state);
    if (!result) {
      return SimulationResult{
          .exit_code = 1, .error_message = result.error().primary.message};
    }
  }

  // Create process states
  std::unordered_map<uint32_t, ProcessState> process_states;
  for (ProcessId proc_id : module_info->initial_processes) {
    auto state = CreateProcessState(mir_arena, types, proc_id, &design_state);
    process_states.emplace(proc_id.value, std::move(state));
  }

  // Error from callback (captured by reference)
  std::optional<Diagnostic> callback_error;

  // Create engine with suspension handler
  runtime::Engine engine([&](runtime::Engine& eng,
                             runtime::ProcessHandle handle,
                             runtime::ResumePoint resume) {
    if (callback_error) {
      return;  // Stop processing if we already have an error
    }

    auto it = process_states.find(handle.process_id);
    if (it == process_states.end()) {
      return;
    }
    auto& state = it->second;

    state.current_block = BasicBlockId{resume.block_index};
    state.instruction_index = resume.instruction_index;
    state.status = ProcessStatus::kRunning;

    auto reason_result = interp.RunUntilSuspend(state);
    if (!reason_result) {
      callback_error = std::move(reason_result).error();
      return;
    }

    std::visit(
        common::Overloaded{
            [](const SuspendFinished&) {},
            [&](const SuspendDelay& d) {
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = d.resume_block.value,
                      .instruction_index = 0},
                  d.ticks);
            },
            [&](const SuspendWait& w) {
              // TODO(hankhsu): Subscribe to signal triggers
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = w.resume_block.value,
                      .instruction_index = 0},
                  1);
            },
            [&](const SuspendRepeat& r) {
              eng.Delay(
                  handle,
                  runtime::ResumePoint{
                      .block_index = r.resume_block.value,
                      .instruction_index = 0},
                  1);
            },
        },
        *reason_result);
  });

  // Schedule initial processes
  for (ProcessId proc_id : module_info->initial_processes) {
    engine.ScheduleInitial(
        runtime::ProcessHandle{.process_id = proc_id.value, .instance_id = 0});
  }

  // Run simulation
  try {
    engine.Run();
  } catch (const std::exception& e) {
    return SimulationResult{
        .exit_code = 1,
        .error_message = std::format("runtime error: {}", e.what())};
  }

  // Check if callback encountered an error
  if (callback_error) {
    return SimulationResult{
        .exit_code = 1, .error_message = callback_error->primary.message};
  }

  return SimulationResult{.exit_code = 0, .error_message = {}};
}

}  // namespace lyra::mir::interp
