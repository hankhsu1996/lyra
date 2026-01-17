#include "lyra/mir/interp/interpreter.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/arena.hpp"
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

// Count the maximum storage ID needed for locals and temps across all places
// in the blocks of a process.
//
// INVARIANT: This assumes all locals and temps are discoverable via instruction
// operands. Any MIR feature that introduces implicit storage (e.g., call
// frames, hidden temporaries) will require revisiting this logic.
struct StorageCounter {
  size_t max_local = 0;
  size_t max_temp = 0;

  void Visit(const Place& place) {
    switch (place.root.kind) {
      case PlaceRoot::Kind::kLocal:
        max_local = std::max(max_local, static_cast<size_t>(place.root.id + 1));
        break;
      case PlaceRoot::Kind::kTemp:
        max_temp = std::max(max_temp, static_cast<size_t>(place.root.id + 1));
        break;
      case PlaceRoot::Kind::kDesign:
        break;
    }
  }

  void Visit(const Operand& op, const Arena& arena) {
    if (op.kind == Operand::Kind::kUse) {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      Visit(arena[place_id]);
    }
  }

  void Visit(const Rvalue& rv, const Arena& arena) {
    for (const auto& op : rv.operands) {
      Visit(op, arena);
    }
  }

  // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
  void Visit(const Instruction& inst, const Arena& arena) {
    std::visit(
        [&](const auto& i) {
          using T = std::decay_t<decltype(i)>;
          if constexpr (std::is_same_v<T, Assign>) {
            Visit(arena[i.target]);
            Visit(i.source, arena);
          } else if constexpr (std::is_same_v<T, Compute>) {
            Visit(arena[i.target]);
            Visit(i.value, arena);
          }
        },
        inst);
  }
};

// Helper template to apply projections uniformly for const and non-const paths.
template <typename ValueRef>
auto ApplyProjectionsImpl(
    ValueRef root, const std::vector<Projection>& projections) -> ValueRef {
  auto* current = &root;
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
        current = &s.fields[static_cast<size_t>(*field_idx)];
        break;
      }

      case Projection::Kind::kIndex: {
        if (!IsArray(*current)) {
          throw common::InternalError(
              "ApplyProjections", "index projection on non-array");
        }
        auto& a = AsArray(*current);
        const auto* const_idx = std::get_if<int>(&proj.operand);
        if (const_idx == nullptr) {
          throw common::InternalError(
              "ApplyProjections",
              "dynamic array index not yet supported in interpreter");
        }
        if (*const_idx < 0 ||
            static_cast<size_t>(*const_idx) >= a.elements.size()) {
          throw common::InternalError(
              "ApplyProjections", "array index out of range");
        }
        current = &a.elements[static_cast<size_t>(*const_idx)];
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

auto ApplyProjections(
    RuntimeValue& root, const std::vector<Projection>& projections)
    -> RuntimeValue& {
  return ApplyProjectionsImpl<RuntimeValue&>(root, projections);
}

auto ApplyProjections(
    const RuntimeValue& root, const std::vector<Projection>& projections)
    -> const RuntimeValue& {
  return ApplyProjectionsImpl<const RuntimeValue&>(root, projections);
}

}  // namespace

auto CreateProcessState(
    const Arena& arena, ProcessId process_id, DesignState* design_state)
    -> ProcessState {
  const auto& process = arena[process_id];

  // Scan all blocks to determine local/temp storage requirements
  StorageCounter counter;
  for (BasicBlockId block_id : process.blocks) {
    const auto& block = arena[block_id];
    for (const auto& inst : block.instructions) {
      counter.Visit(inst, arena);
    }
  }

  return ProcessState{
      .process = process_id,
      .current_block = process.entry,
      .instruction_index = 0,
      .frame = Frame(counter.max_local, counter.max_temp),
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

  const auto& nested = ApplyProjections(root_value, place.projections);
  return Clone(nested);
}

auto Interpreter::WritePlace(ProcessState& state, PlaceId place_id)
    -> RuntimeValue& {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    return root_value;
  }

  return ApplyProjections(root_value, place.projections);
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

    case Terminator::Kind::kFinish:
      return std::nullopt;

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

auto FindInitialModule(const Design& design, const Arena& arena)
    -> std::optional<InitialModuleInfo> {
  for (const auto& element : design.elements) {
    if (const auto* module = std::get_if<Module>(&element)) {
      for (ProcessId process_id : module->processes) {
        const auto& process = arena[process_id];
        if (process.kind == ProcessKind::kOnce) {
          return InitialModuleInfo{
              .num_module_slots = module->num_module_slots,
              .initial_process = process_id,
          };
        }
      }
    }
  }
  return std::nullopt;
}

}  // namespace lyra::mir::interp
