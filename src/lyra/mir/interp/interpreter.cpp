#include "lyra/mir/interp/interpreter.hpp"

#include <cstddef>
#include <cstdint>
#include <exception>
#include <format>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/blob_codec.hpp"
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
          Overloaded{
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
  }

  // Uses exhaustive Overloaded pattern - adding a new Instruction or EffectOp
  // type will cause a compile error until handled here.
  void Visit(const Instruction& inst, const Arena& arena) {
    std::visit(
        Overloaded{
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
                  Overloaded{
                      [&](const DisplayEffect& d) -> void {
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
                  },
                  i.op);
            },
        },
        inst.data);
  }
};

}  // namespace

// Creates a default-initialized RuntimeValue for a given type following SV
// semantics:
// - 2-state integrals (bit, int, etc.): default to 0
// - 4-state integrals (logic, reg, etc.): default to X (unknown)
// - Strings: default to empty
// - Reals: default to 0.0
// - Arrays/structs: recursively default-initialize each element/field
auto CreateDefaultValue(const TypeArena& types, TypeId type_id)
    -> RuntimeValue {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kVoid:
      return std::monostate{};
    case TypeKind::kIntegral: {
      const auto& info = type.AsIntegral();
      if (info.is_four_state) {
        return MakeIntegralX(info.bit_width);  // 4-state defaults to X
      }
      return MakeIntegral(0, info.bit_width);  // 2-state defaults to 0
    }
    case TypeKind::kPackedArray: {
      // Packed arrays are stored as a flat integral with total bit width.
      // Use IsPackedFourState to recursively check base element type.
      uint32_t total_width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        return MakeIntegralX(total_width);
      }
      return MakeIntegral(0, total_width);
    }
    case TypeKind::kPackedStruct: {
      // Packed structs are stored as a flat integral with total bit width.
      const auto& info = type.AsPackedStruct();
      if (info.is_four_state) {
        return MakeIntegralX(info.total_bit_width);
      }
      return MakeIntegral(0, info.total_bit_width);
    }
    case TypeKind::kString:
      return MakeString("");
    case TypeKind::kReal:
      return MakeReal(0.0);
    case TypeKind::kShortReal:
      return MakeShortReal(0.0F);
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      auto size = static_cast<size_t>(info.range.Size());
      std::vector<RuntimeValue> elements;
      elements.reserve(size);
      for (size_t i = 0; i < size; ++i) {
        elements.push_back(CreateDefaultValue(types, info.element_type));
      }
      return MakeArray(std::move(elements));
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      std::vector<RuntimeValue> fields;
      fields.reserve(info.fields.size());
      for (const auto& field : info.fields) {
        fields.push_back(CreateDefaultValue(types, field.type));
      }
      return MakeStruct(std::move(fields));
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      RuntimeIntegral storage;
      storage.bit_width = info.storage_bit_width;
      uint32_t num_words = (info.storage_bit_width + 63) / 64;
      if (info.storage_is_four_state) {
        storage.value.resize(num_words, 0);
        storage.unknown.resize(num_words, ~uint64_t{0});
      } else {
        storage.value.resize(num_words, 0);
        storage.unknown.resize(num_words, 0);
      }
      // Default init member[0] via codec
      RuntimeValue member0_default =
          CreateDefaultValue(types, info.members[0].type);
      StoreToBlob(info.members[0].type, member0_default, storage, 0, types);
      return MakeUnion(std::move(storage));
    }
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      // Dynamic arrays and queues default to empty (size 0)
      return MakeArray({});
    case TypeKind::kEnum: {
      // Enum defaults to 0 (or X for 4-state base type)
      // Use PackedBitWidth/IsPackedFourState since base type can be packed
      // array
      uint32_t bit_width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        return MakeIntegralX(bit_width);
      }
      return MakeIntegral(0, bit_width);
    }
  }
  throw common::InternalError("CreateDefaultValue", "unknown type kind");
}

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
    std::ostream* output) -> SimulationResult {
  // Find initial module
  auto module_info = FindInitialModule(design, mir_arena);
  if (!module_info) {
    return SimulationResult{
        .exit_code = 1, .error_message = "no initial process found"};
  }

  // Create design state
  auto design_state = CreateDesignState(mir_arena, types, design);

  // Create interpreter
  Interpreter interp(&mir_arena, &types);
  if (output != nullptr) {
    interp.SetOutput(output);
  } else {
    interp.SetOutput(&std::cout);
  }

  // Run design init processes (package variable initialization)
  for (ProcessId proc_id : design.init_processes) {
    auto state = CreateProcessState(mir_arena, types, proc_id, &design_state);
    interp.Run(state);
  }

  // Create process states
  std::unordered_map<uint32_t, ProcessState> process_states;
  for (ProcessId proc_id : module_info->initial_processes) {
    auto state = CreateProcessState(mir_arena, types, proc_id, &design_state);
    process_states.emplace(proc_id.value, std::move(state));
  }

  // Create engine with suspension handler
  runtime::Engine engine([&](runtime::Engine& eng,
                             runtime::ProcessHandle handle,
                             runtime::ResumePoint resume) {
    auto it = process_states.find(handle.process_id);
    if (it == process_states.end()) {
      return;
    }
    auto& state = it->second;

    state.current_block = BasicBlockId{resume.block_index};
    state.instruction_index = resume.instruction_index;
    state.status = ProcessStatus::kRunning;

    auto reason = interp.RunUntilSuspend(state);

    std::visit(
        Overloaded{
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
        reason);
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

  return SimulationResult{.exit_code = 0, .error_message = {}};
}

}  // namespace lyra::mir::interp
