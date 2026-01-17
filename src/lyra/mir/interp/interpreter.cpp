#include "lyra/mir/interp/interpreter.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/eval_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::interp {

namespace {

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
    case TypeKind::kString:
      return MakeString("");
    case TypeKind::kReal:
      return MakeReal(0.0);
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
  }
  throw common::InternalError("CreateDefaultValue", "unknown type kind");
}

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
      if (const auto* op = std::get_if<Operand>(&proj.operand)) {
        Visit(*op, arena);
      }
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
            [&](const Effect& i) {
              std::visit(
                  Overloaded{
                      [&](const DisplayEffect& op) {
                        for (const auto& arg : op.args) {
                          Visit(arg, arena);
                        }
                      },
                  },
                  i.op);
            },
        },
        inst);
  }
};

// Helper template to apply projections uniformly for const and non-const paths.
// IndexEval is a callable that evaluates a dynamic Operand to an int64_t index.
// TypeArena and root_type are used to normalize array indices for
// non-zero-based ranges.
template <typename ValueRef, typename IndexEval>
auto ApplyProjectionsImpl(
    ValueRef root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index)
    -> ValueRef {
  auto* current = &root;
  TypeId current_type = root_type;

  for (const auto& proj : projections) {
    switch (proj.kind) {
      case Projection::Kind::kField: {
        if (!IsStruct(*current)) {
          throw common::InternalError(
              "ApplyProjections", "field projection on non-struct");
        }
        auto& s = AsStruct(*current);
        const auto* field_idx = std::get_if<int>(&proj.operand);
        if (field_idx == nullptr) {
          throw common::InternalError(
              "ApplyProjections", "field projection requires constant index");
        }
        if (*field_idx < 0 ||
            static_cast<size_t>(*field_idx) >= s.fields.size()) {
          throw common::InternalError(
              "ApplyProjections", "field index out of range");
        }
        // Update current type to the field's type
        if (current_type) {
          const auto& struct_info = types[current_type].AsUnpackedStruct();
          current_type =
              struct_info.fields[static_cast<size_t>(*field_idx)].type;
        }
        current = &s.fields[static_cast<size_t>(*field_idx)];
        break;
      }

      case Projection::Kind::kIndex: {
        if (!IsArray(*current)) {
          throw common::InternalError(
              "ApplyProjections", "index projection on non-array");
        }
        auto& a = AsArray(*current);

        // Evaluate index (constant or dynamic)
        int64_t index = 0;
        if (const auto* const_idx = std::get_if<int>(&proj.operand)) {
          index = *const_idx;
        } else {
          const auto& operand = std::get<Operand>(proj.operand);
          index = eval_index(operand);
        }

        // Get array bounds from type and normalize index to 0-based offset
        int64_t offset = index;
        auto size = static_cast<int64_t>(a.elements.size());
        if (current_type) {
          const auto& array_info = types[current_type].AsUnpackedArray();
          offset = index - array_info.range.lower;
          current_type = array_info.element_type;
        }

        // Bounds check (runtime error - user program error)
        if (offset < 0 || offset >= size) {
          throw std::runtime_error(
              std::format("array index {} out of bounds", index));
        }
        current = &a.elements[static_cast<size_t>(offset)];
        break;
      }

      case Projection::Kind::kSlice:
        throw common::InternalError(
            "ApplyProjections", "slice projections not supported");

      case Projection::Kind::kDeref:
        throw common::InternalError(
            "ApplyProjections", "deref projections not supported");
    }
  }
  return *current;
}

template <typename IndexEval>
auto ApplyProjections(
    RuntimeValue& root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index)
    -> RuntimeValue& {
  return ApplyProjectionsImpl<RuntimeValue&>(
      root, projections, types, root_type, eval_index);
}

template <typename IndexEval>
auto ApplyProjections(
    const RuntimeValue& root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index)
    -> const RuntimeValue& {
  return ApplyProjectionsImpl<const RuntimeValue&>(
      root, projections, types, root_type, eval_index);
}

}  // namespace

auto CreateProcessState(
    const Arena& arena, const TypeArena& types, ProcessId process_id,
    DesignState* design_state) -> ProcessState {
  const auto& process = arena[process_id];

  // Scan all blocks to collect local/temp storage requirements and types
  StorageCollector collector;
  for (BasicBlockId block_id : process.blocks) {
    const auto& block = arena[block_id];
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
  };
}

auto Interpreter::Run(ProcessState& state) -> ProcessStatus {
  while (state.status == ProcessStatus::kRunning) {
    const auto& block = (*arena_)[state.current_block];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      ExecInstruction(state, block.instructions[state.instruction_index]);
      state.instruction_index++;
    }

    // Execute terminator
    auto next = ExecTerminator(state, block.terminator);
    if (!next) {
      state.status = ProcessStatus::kFinished;
      break;
    }

    state.current_block = *next;
    state.instruction_index = 0;
  }

  return state.status;
}

auto Interpreter::EvalOperand(const ProcessState& state, const Operand& op)
    -> RuntimeValue {
  switch (op.kind) {
    case Operand::Kind::kConst: {
      const auto& c = std::get<Constant>(op.payload);
      return std::visit(
          [&](const auto& val) -> RuntimeValue {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, IntegralConstant>) {
              const auto& type = (*types_)[c.type];
              if (type.Kind() != TypeKind::kIntegral) {
                throw common::InternalError(
                    "EvalOperand", "type mismatch: expected integral");
              }
              return MakeIntegralFromConstant(val, type.AsIntegral().bit_width);
            } else if constexpr (std::is_same_v<T, StringConstant>) {
              return MakeString(val.value);
            } else if constexpr (std::is_same_v<T, RealConstant>) {
              return MakeReal(val.value);
            } else if constexpr (std::is_same_v<T, StructConstant>) {
              throw common::InternalError(
                  "EvalOperand", "struct constants not supported");
            } else if constexpr (std::is_same_v<T, ArrayConstant>) {
              throw common::InternalError(
                  "EvalOperand", "array constants not supported");
            }
          },
          c.value);
    }

    case Operand::Kind::kUse: {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      return ReadPlace(state, place_id);
    }

    case Operand::Kind::kPoison:
      throw common::InternalError("EvalOperand", "poison operand encountered");
  }

  throw common::InternalError("EvalOperand", "unknown operand kind");
}

auto Interpreter::EvalRvalue(const ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  switch (rv.kind) {
    case RvalueKind::kUnary: {
      if (rv.operands.size() != 1) {
        throw common::InternalError(
            "EvalRvalue", "unary operation requires exactly 1 operand");
      }
      auto operand = EvalOperand(state, rv.operands[0]);
      return EvalUnary(rv.op, operand);
    }

    case RvalueKind::kBinary: {
      if (rv.operands.size() != 2) {
        throw common::InternalError(
            "EvalRvalue", "binary operation requires exactly 2 operands");
      }
      auto lhs = EvalOperand(state, rv.operands[0]);
      auto rhs = EvalOperand(state, rv.operands[1]);
      return EvalBinary(rv.op, lhs, rhs);
    }

    case RvalueKind::kCast: {
      if (rv.operands.size() != 1) {
        throw common::InternalError(
            "EvalRvalue", "cast operation requires exactly 1 operand");
      }
      const auto* cast_info = std::get_if<CastInfo>(&rv.info);
      if (cast_info == nullptr) {
        throw common::InternalError(
            "EvalRvalue", "cast operation missing CastInfo");
      }
      auto operand = EvalOperand(state, rv.operands[0]);
      const auto& src_type = (*types_)[cast_info->source_type];
      const auto& tgt_type = (*types_)[cast_info->target_type];
      return EvalCast(operand, src_type, tgt_type);
    }

    case RvalueKind::kCall:
      // System calls that produce values (pure functions like $clog2) would be
      // handled here. Currently all supported system calls are effects.
      throw common::InternalError(
          "EvalRvalue", "pure system calls not yet supported");
  }

  throw common::InternalError("EvalRvalue", "unknown rvalue kind");
}

auto Interpreter::ResolveRoot(const ProcessState& state, const PlaceRoot& root)
    -> const RuntimeValue& {
  switch (root.kind) {
    case PlaceRoot::Kind::kLocal:
      return state.frame.GetLocal(root.id);
    case PlaceRoot::Kind::kTemp:
      return state.frame.GetTemp(root.id);
    case PlaceRoot::Kind::kDesign:
      return state.design_state->Get(root.id);
  }
  throw common::InternalError("ResolveRoot", "unknown PlaceRoot kind");
}

auto Interpreter::ResolveRootMut(ProcessState& state, const PlaceRoot& root)
    -> RuntimeValue& {
  switch (root.kind) {
    case PlaceRoot::Kind::kLocal:
      return state.frame.GetLocal(root.id);
    case PlaceRoot::Kind::kTemp:
      return state.frame.GetTemp(root.id);
    case PlaceRoot::Kind::kDesign:
      return state.design_state->Get(root.id);
  }
  throw common::InternalError("ResolveRootMut", "unknown PlaceRoot kind");
}

auto Interpreter::ReadPlace(const ProcessState& state, PlaceId place_id)
    -> RuntimeValue {
  const auto& place = (*arena_)[place_id];
  const auto& root_value = ResolveRoot(state, place.root);

  if (place.projections.empty()) {
    return Clone(root_value);
  }

  auto eval_index = [&](const Operand& op) -> int64_t {
    auto val = EvalOperand(state, op);
    if (!IsIntegral(val)) {
      throw common::InternalError("ReadPlace", "array index must be integral");
    }
    const auto& integral = AsIntegral(val);
    if (!integral.IsKnown()) {
      throw std::runtime_error("array index is X/Z");
    }
    return static_cast<int64_t>(integral.value.empty() ? 0 : integral.value[0]);
  };

  const auto& nested = ApplyProjections(
      root_value, place.projections, *types_, place.root.type, eval_index);
  return Clone(nested);
}

auto Interpreter::WritePlace(ProcessState& state, PlaceId place_id)
    -> RuntimeValue& {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    return root_value;
  }

  auto eval_index = [&](const Operand& op) -> int64_t {
    auto val = EvalOperand(state, op);
    if (!IsIntegral(val)) {
      throw common::InternalError("WritePlace", "array index must be integral");
    }
    const auto& integral = AsIntegral(val);
    if (!integral.IsKnown()) {
      throw std::runtime_error("array index is X/Z");
    }
    return static_cast<int64_t>(integral.value.empty() ? 0 : integral.value[0]);
  };

  return ApplyProjections(
      root_value, place.projections, *types_, place.root.type, eval_index);
}

void Interpreter::ExecAssign(ProcessState& state, const Assign& assign) {
  auto value = EvalOperand(state, assign.source);
  WritePlace(state, assign.target) = std::move(value);
}

void Interpreter::ExecCompute(ProcessState& state, const Compute& compute) {
  auto value = EvalRvalue(state, compute.value);
  WritePlace(state, compute.target) = std::move(value);
}

void Interpreter::ExecEffect(const ProcessState& state, const Effect& effect) {
  std::visit(
      [&](const auto& op) {
        using T = std::decay_t<decltype(op)>;
        if constexpr (std::is_same_v<T, DisplayEffect>) {
          ExecDisplayEffect(state, op);
        } else {
          throw common::InternalError("ExecEffect", "unknown effect operation");
        }
      },
      effect.op);
}

void Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) {
  // Get output stream (default to cout if not set)
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  // Format and print each argument
  for (size_t i = 0; i < disp.args.size(); ++i) {
    if (i > 0) {
      out << " ";
    }

    auto value = EvalOperand(state, disp.args[i]);

    if (IsString(value)) {
      out << AsString(value).value;
    } else if (IsIntegral(value)) {
      const auto& integral = AsIntegral(value);
      switch (disp.radix) {
        case PrintRadix::kDecimal:
          out << ToDecimalString(integral, false);
          break;
        case PrintRadix::kHex:
          out << ToHexString(integral);
          break;
        case PrintRadix::kBinary:
          out << ToBinaryString(integral);
          break;
        case PrintRadix::kOctal:
          out << ToOctalString(integral);
          break;
      }
    } else {
      out << ToString(value);
    }
  }

  if (disp.append_newline) {
    out << "\n";
  }
}

void Interpreter::ExecInstruction(
    ProcessState& state, const Instruction& inst) {
  std::visit(
      [&](const auto& i) {
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, Assign>) {
          ExecAssign(state, i);
        } else if constexpr (std::is_same_v<T, Compute>) {
          ExecCompute(state, i);
        } else if constexpr (std::is_same_v<T, Effect>) {
          ExecEffect(state, i);
        }
      },
      inst);
}

auto Interpreter::ExecTerminator(ProcessState& state, const Terminator& term)
    -> std::optional<BasicBlockId> {
  switch (term.kind) {
    case Terminator::Kind::kJump:
      if (term.targets.empty()) {
        throw common::InternalError(
            "ExecTerminator", "jump terminator has no target");
      }
      return term.targets[0];

    case Terminator::Kind::kBranch: {
      if (term.targets.size() != 2) {
        throw common::InternalError(
            "ExecTerminator", "branch terminator requires 2 targets");
      }
      // Get the condition from the place indicated by condition_operand
      PlaceId cond_place{static_cast<uint32_t>(term.condition_operand)};
      auto cond = ReadPlace(state, cond_place);
      if (!IsIntegral(cond)) {
        throw common::InternalError(
            "ExecTerminator", "branch condition must be integral");
      }
      const auto& cond_int = AsIntegral(cond);
      if (!cond_int.IsKnown()) {
        // Runtime error: program has X/Z in control flow
        throw std::runtime_error("branch condition is X/Z");
      }
      // targets[0] = true branch, targets[1] = false branch
      return cond_int.IsZero() ? term.targets[1] : term.targets[0];
    }

    case Terminator::Kind::kSwitch: {
      if (term.targets.empty()) {
        throw common::InternalError(
            "ExecTerminator", "switch terminator has no targets");
      }
      PlaceId selector_place{static_cast<uint32_t>(term.condition_operand)};
      auto selector = ReadPlace(state, selector_place);
      if (!IsIntegral(selector)) {
        throw common::InternalError(
            "ExecTerminator", "switch selector must be integral");
      }
      const auto& sel_int = AsIntegral(selector);
      if (!sel_int.IsKnown()) {
        // Default to last target (default case)
        return term.targets.back();
      }
      uint64_t val = sel_int.value.empty() ? 0 : sel_int.value[0];
      if (val >= term.targets.size() - 1) {
        // Out of range, use default
        return term.targets.back();
      }
      return term.targets[static_cast<size_t>(val)];
    }

    case Terminator::Kind::kFinish: {
      if (term.termination_info) {
        const auto& info = *term.termination_info;
        if (info.level >= 1) {
          // Get output stream (default to cout if not set)
          std::ostream& out = output_ != nullptr ? *output_ : std::cout;

          // Determine termination name for message
          const char* name = nullptr;
          switch (info.kind) {
            case TerminationKind::kFinish:
              name = "$finish";
              break;
            case TerminationKind::kStop:
              name = "$stop";
              break;
            case TerminationKind::kFatal:
              name = "$fatal";
              break;
            case TerminationKind::kExit:
              name = "$exit";
              break;
          }

          // Print time message (time is always 0 for now, until scheduler)
          out << name << " called at time 0\n";
        }
        // Level 2 would print statistics, but we don't track those yet
      }
      return std::nullopt;
    }

    case Terminator::Kind::kReturn:
      return std::nullopt;

    case Terminator::Kind::kDelay:
      throw common::InternalError(
          "ExecTerminator", "delay terminator requires runtime/scheduler");

    case Terminator::Kind::kWait:
      throw common::InternalError(
          "ExecTerminator", "wait terminator requires runtime/scheduler");

    case Terminator::Kind::kRepeat:
      throw common::InternalError(
          "ExecTerminator", "repeat terminator requires runtime/scheduler");
  }

  throw common::InternalError("ExecTerminator", "unknown terminator kind");
}

auto CreateDesignState(
    const Arena& arena, const TypeArena& types, const Module& module)
    -> DesignState {
  // Collect design slot types by scanning all processes
  StorageCollector collector;
  for (ProcessId process_id : module.processes) {
    const auto& process = arena[process_id];
    for (BasicBlockId block_id : process.blocks) {
      const auto& block = arena[block_id];
      for (const auto& inst : block.instructions) {
        collector.Visit(inst, arena);
      }
    }
  }

  // Initialize design storage with default values based on types
  std::vector<RuntimeValue> storage;
  storage.resize(module.num_module_slots);
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
      for (ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        if (process.kind == ProcessKind::kOnce) {
          return InitialModuleInfo{
              .module = module,
              .initial_process = process_id,
          };
        }
      }
    }
  }
  return std::nullopt;
}

}  // namespace lyra::mir::interp
