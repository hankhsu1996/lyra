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
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/eval_ops.hpp"
#include "lyra/mir/interp/format.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::interp {

namespace {

// Helper to enforce bounded queue constraint.
// If max_bound > 0, truncates elements to at most (max_bound + 1) elements.
void TruncateToBound(std::vector<RuntimeValue>& elements, uint32_t max_bound) {
  if (max_bound > 0) {
    while (elements.size() > max_bound + 1) {
      elements.pop_back();  // Discard from back
    }
  }
}

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
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      // Dynamic arrays and queues default to empty (size 0)
      return MakeArray({});
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
    // Visit receiver for pop methods that mutate
    if (const auto* info = std::get_if<BuiltinCallInfo>(&rv.info)) {
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
            [&](const Effect& i) {
              std::visit(
                  Overloaded{
                      [&](const DisplayEffect& op) -> void {
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
          const auto& type_info = types[current_type];
          if (type_info.Kind() == TypeKind::kUnpackedArray) {
            const auto& array_info = type_info.AsUnpackedArray();
            offset = index - array_info.range.lower;
            current_type = array_info.element_type;
          } else if (type_info.Kind() == TypeKind::kDynamicArray) {
            // Dynamic arrays are 0-based, no range normalization needed
            offset = index;
            current_type = type_info.AsDynamicArray().element_type;
          } else if (type_info.Kind() == TypeKind::kQueue) {
            // Queues are 0-based, no range normalization needed
            offset = index;
            current_type = type_info.AsQueue().element_type;
          }
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

auto RadixToFormatChar(PrintRadix radix) -> char {
  switch (radix) {
    case PrintRadix::kDecimal:
      return 'd';
    case PrintRadix::kHex:
      return 'h';
    case PrintRadix::kBinary:
      return 'b';
    case PrintRadix::kOctal:
      return 'o';
  }
  return 'd';
}

// Compute the resulting TypeId after applying projections to a place.
// This traverses the type structure to get the final element/field type.
auto TypeOfPlace(const Place& place, const TypeArena& types) -> TypeId {
  TypeId current_type = place.root.type;

  for (const auto& proj : place.projections) {
    if (!current_type) {
      break;
    }

    switch (proj.kind) {
      case Projection::Kind::kField: {
        const auto* field_idx = std::get_if<int>(&proj.operand);
        if (field_idx != nullptr) {
          const auto& struct_info = types[current_type].AsUnpackedStruct();
          current_type =
              struct_info.fields[static_cast<size_t>(*field_idx)].type;
        }
        break;
      }
      case Projection::Kind::kIndex: {
        const auto& type_info = types[current_type];
        if (type_info.Kind() == TypeKind::kUnpackedArray) {
          current_type = type_info.AsUnpackedArray().element_type;
        } else if (type_info.Kind() == TypeKind::kDynamicArray) {
          current_type = type_info.AsDynamicArray().element_type;
        } else if (type_info.Kind() == TypeKind::kQueue) {
          current_type = type_info.AsQueue().element_type;
        }
        break;
      }
      case Projection::Kind::kSlice:
        throw common::InternalError(
            "TypeOfPlace", "slice projections not supported");
      case Projection::Kind::kDeref:
        throw common::InternalError(
            "TypeOfPlace", "deref projections not supported");
    }
  }

  return current_type;
}

// Recover the TypeId for an Operand without evaluation.
auto TypeOfOperand(
    const Operand& op, const Arena& arena, const TypeArena& types) -> TypeId {
  switch (op.kind) {
    case Operand::Kind::kConst: {
      const auto& c = std::get<Constant>(op.payload);
      return c.type;
    }
    case Operand::Kind::kUse: {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      const auto& place = arena[place_id];
      return TypeOfPlace(place, types);
    }
    case Operand::Kind::kPoison:
      return kInvalidTypeId;
  }
  return kInvalidTypeId;
}

// Helper: safely extract index from operand, return nullopt if X/Z
// Per IEEE 1800-2023, invalid index means no-op (not an error)
auto TryGetIndex(
    const RuntimeValue& val, const TypeArena& types, TypeId type_id)
    -> std::optional<int64_t> {
  if (!IsIntegral(val)) {
    return std::nullopt;
  }
  const auto& integral = AsIntegral(val);
  if (!integral.IsKnown()) {
    return std::nullopt;  // X/Z -> invalid
  }

  // Extract raw bits
  uint64_t raw_bits = integral.value.empty() ? 0 : integral.value[0];

  // Sign-extend if operand type is signed
  const auto& type_info = types[type_id];
  if (type_info.Kind() == TypeKind::kIntegral) {
    const auto& info = type_info.AsIntegral();
    if (info.is_signed && info.bit_width < 64) {
      uint64_t sign_bit = 1ULL << (info.bit_width - 1);
      if ((raw_bits & sign_bit) != 0) {
        uint64_t mask = ~((1ULL << info.bit_width) - 1);
        raw_bits |= mask;
      }
    }
  }

  return static_cast<int64_t>(raw_bits);
}

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
  };
}

auto Interpreter::Run(ProcessState& state) -> ProcessStatus {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

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

auto Interpreter::RunFunction(
    FunctionId func_id, const std::vector<RuntimeValue>& args,
    DesignState* design_state) -> RuntimeValue {
  const auto& func = (*arena_)[func_id];

  // Use function's metadata for storage allocation
  const auto& local_types = func.local_types;
  const auto& temp_types = func.temp_types;

  // Initialize locals with default values
  std::vector<RuntimeValue> locals;
  locals.reserve(local_types.size());
  for (TypeId type_id : local_types) {
    locals.push_back(
        type_id ? CreateDefaultValue(*types_, type_id) : RuntimeValue{});
  }

  // Initialize temps with default values
  std::vector<RuntimeValue> temps;
  temps.reserve(temp_types.size());
  for (TypeId type_id : temp_types) {
    temps.push_back(
        type_id ? CreateDefaultValue(*types_, type_id) : RuntimeValue{});
  }

  // Copy arguments to parameter locals
  // For non-void functions: local 0 = return value, parameters start at local 1
  // For void functions: parameters start at local 0
  // We detect this by checking if there are more locals than args
  size_t param_start = 0;
  if (local_types.size() > args.size()) {
    param_start = 1;  // local 0 is return place
  }

  for (size_t i = 0; i < args.size(); ++i) {
    size_t local_idx = param_start + i;
    if (local_idx < locals.size()) {
      locals[local_idx] = Clone(args[i]);
    }
  }

  // Create function state (reuse ProcessState structure for convenience)
  ProcessState func_state{
      .process = ProcessId{0},  // Unused for function execution
      .current_block = func.entry,
      .instruction_index = 0,
      .frame = Frame(std::move(locals), std::move(temps)),
      .design_state = design_state,
      .status = ProcessStatus::kRunning,
  };

  // Execute function (look up blocks from func directly)
  while (func_state.status == ProcessStatus::kRunning) {
    const auto& block = func.blocks[func_state.current_block.value];

    while (func_state.instruction_index < block.instructions.size()) {
      ExecInstruction(
          func_state, block.instructions[func_state.instruction_index]);
      func_state.instruction_index++;
    }

    auto next = ExecTerminator(func_state, block.terminator);
    if (!next) {
      break;  // Return reached
    }

    func_state.current_block = *next;
    func_state.instruction_index = 0;
  }

  // Return value is in local 0 (for non-void functions)
  if (param_start == 1 && func_state.frame.NumLocals() > 0) {
    return Clone(func_state.frame.GetLocal(0));
  }

  // Void function - return monostate
  return std::monostate{};
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

auto Interpreter::EvalRvalue(ProcessState& state, const Rvalue& rv)
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

    case RvalueKind::kCall: {
      // User function call
      if (const auto* user_call = std::get_if<UserCallInfo>(&rv.info)) {
        // Evaluate arguments
        std::vector<RuntimeValue> args;
        args.reserve(rv.operands.size());
        for (const auto& operand : rv.operands) {
          args.push_back(EvalOperand(state, operand));
        }

        // Execute function and return result
        return RunFunction(user_call->callee, args, state.design_state);
      }

      // System calls that produce values (pure functions like $clog2) would be
      // handled here. Currently all supported system calls are effects.
      throw common::InternalError(
          "EvalRvalue", "pure system calls not yet supported");
    }

    case RvalueKind::kAggregate: {
      const auto* info = std::get_if<AggregateInfo>(&rv.info);
      if (info == nullptr) {
        throw common::InternalError(
            "EvalRvalue", "kAggregate missing AggregateInfo");
      }

      const Type& type = (*types_)[info->result_type];

      // Handle struct aggregates
      if (type.Kind() == TypeKind::kUnpackedStruct) {
        const auto& struct_info = type.AsUnpackedStruct();
        if (rv.operands.size() != struct_info.fields.size()) {
          throw common::InternalError(
              "EvalRvalue", "kAggregate operand count mismatch");
        }

        std::vector<RuntimeValue> fields;
        fields.reserve(rv.operands.size());
        for (const auto& operand : rv.operands) {
          fields.push_back(EvalOperand(state, operand));
        }
        return MakeStruct(std::move(fields));
      }

      // Handle array/queue aggregates (unpacked array, dynamic array, queue)
      if (type.Kind() == TypeKind::kUnpackedArray ||
          type.Kind() == TypeKind::kDynamicArray ||
          type.Kind() == TypeKind::kQueue) {
        std::vector<RuntimeValue> elements;
        elements.reserve(rv.operands.size());
        for (const auto& operand : rv.operands) {
          elements.push_back(EvalOperand(state, operand));
        }
        // For bounded queues, truncate to max_bound + 1 elements
        if (type.Kind() == TypeKind::kQueue) {
          TruncateToBound(elements, type.AsQueue().max_bound);
        }
        return MakeArray(std::move(elements));
      }

      throw common::InternalError("EvalRvalue", "kAggregate: unsupported type");
    }

    case RvalueKind::kBuiltinCall: {
      const auto* info = std::get_if<BuiltinCallInfo>(&rv.info);
      if (info == nullptr) {
        throw common::InternalError(
            "EvalRvalue", "kBuiltinCall missing BuiltinCallInfo");
      }

      switch (info->method) {
        case BuiltinMethod::kNewArray: {
          // new[size] or new[size](init)
          if (rv.operands.empty()) {
            throw common::InternalError("EvalRvalue", "new[] requires size");
          }
          auto size_val = EvalOperand(state, rv.operands[0]);
          if (!IsIntegral(size_val)) {
            throw common::InternalError(
                "EvalRvalue", "new[] size must be integral");
          }
          const auto& size_int = AsIntegral(size_val);
          if (!size_int.IsKnown()) {
            throw std::runtime_error("new[] size is X/Z");
          }
          // Get raw value and check for negative (size is int, which is signed)
          // Must sign-extend based on operand type to properly detect negatives
          uint64_t raw_bits = size_int.value.empty() ? 0 : size_int.value[0];
          TypeId size_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
          const auto& type_info = (*types_)[size_type];
          if (type_info.Kind() == TypeKind::kIntegral) {
            const auto& integral = type_info.AsIntegral();
            if (integral.is_signed && integral.bit_width < 64) {
              // Sign-extend: if the sign bit is set, fill upper bits with 1s
              uint64_t sign_bit = 1ULL << (integral.bit_width - 1);
              if ((raw_bits & sign_bit) != 0) {
                uint64_t mask = ~((1ULL << integral.bit_width) - 1);
                raw_bits |= mask;
              }
            }
          }
          auto signed_size = static_cast<int64_t>(raw_bits);
          if (signed_size < 0) {
            throw std::runtime_error("new[] size cannot be negative");
          }
          auto new_size = static_cast<size_t>(signed_size);

          // Get element type from result type (which is the dynamic array type)
          const auto& array_type = (*types_)[info->result_type];
          if (array_type.Kind() != TypeKind::kDynamicArray) {
            throw common::InternalError(
                "EvalRvalue", "new[] result must be dynamic array");
          }
          TypeId element_type = array_type.AsDynamicArray().element_type;

          std::vector<RuntimeValue> elements;
          elements.reserve(new_size);

          if (rv.operands.size() > 1) {
            // new[size](init) - copy from initializer
            auto init_val = EvalOperand(state, rv.operands[1]);
            if (!IsArray(init_val)) {
              throw common::InternalError(
                  "EvalRvalue", "new[] init must be array");
            }
            const auto& init_arr = AsArray(init_val);
            size_t copy_count = std::min(new_size, init_arr.elements.size());
            for (size_t i = 0; i < copy_count; ++i) {
              elements.push_back(Clone(init_arr.elements[i]));
            }
            // Fill remaining with defaults
            for (size_t i = copy_count; i < new_size; ++i) {
              elements.push_back(CreateDefaultValue(*types_, element_type));
            }
          } else {
            // new[size] - all default values
            for (size_t i = 0; i < new_size; ++i) {
              elements.push_back(CreateDefaultValue(*types_, element_type));
            }
          }
          return MakeArray(std::move(elements));
        }

        case BuiltinMethod::kArraySize:
        case BuiltinMethod::kQueueSize: {
          // arr.size() / q.size() - returns int
          if (rv.operands.empty()) {
            throw common::InternalError("EvalRvalue", "size() requires array");
          }
          auto arr_val = EvalOperand(state, rv.operands[0]);
          if (!IsArray(arr_val)) {
            throw common::InternalError(
                "EvalRvalue", "size() operand must be array");
          }
          auto size = static_cast<int64_t>(AsArray(arr_val).elements.size());
          return MakeIntegral(size, 32);  // int is 32-bit
        }

        case BuiltinMethod::kQueuePopBack: {
          // q.pop_back() - returns element, mutates queue
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "pop_back() requires receiver");
          }
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          if (elements.empty()) {
            // IEEE: return default value if queue is empty
            return CreateDefaultValue(*types_, info->result_type);
          }
          RuntimeValue result = std::move(elements.back());
          elements.pop_back();
          return result;
        }

        case BuiltinMethod::kQueuePopFront: {
          // q.pop_front() - returns element, mutates queue
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "pop_front() requires receiver");
          }
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          if (elements.empty()) {
            // IEEE: return default value if queue is empty
            return CreateDefaultValue(*types_, info->result_type);
          }
          RuntimeValue result = std::move(elements.front());
          elements.erase(elements.begin());
          return result;
        }

        case BuiltinMethod::kQueuePushBack: {
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "push_back() requires receiver");
          }
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          elements.push_back(EvalOperand(state, rv.operands[0]));
          TypeId receiver_type =
              TypeOfPlace((*arena_)[*info->receiver], *types_);
          const auto& type = (*types_)[receiver_type];
          if (type.Kind() == TypeKind::kQueue) {
            TruncateToBound(elements, type.AsQueue().max_bound);
          }
          return std::monostate{};
        }

        case BuiltinMethod::kQueuePushFront: {
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "push_front() requires receiver");
          }
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          elements.insert(elements.begin(), EvalOperand(state, rv.operands[0]));
          TypeId receiver_type =
              TypeOfPlace((*arena_)[*info->receiver], *types_);
          const auto& type = (*types_)[receiver_type];
          if (type.Kind() == TypeKind::kQueue) {
            TruncateToBound(elements, type.AsQueue().max_bound);
          }
          return std::monostate{};
        }

        case BuiltinMethod::kQueueInsert: {
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "insert() requires receiver");
          }
          TypeId idx_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
          auto idx_opt = TryGetIndex(
              EvalOperand(state, rv.operands[0]), *types_, idx_type);
          if (!idx_opt) {
            return std::monostate{};  // X/Z -> no-op
          }
          auto idx = *idx_opt;
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          if (idx < 0 || idx > static_cast<int64_t>(elements.size())) {
            return std::monostate{};  // Invalid index -> no-op
          }
          elements.insert(
              elements.begin() + idx, EvalOperand(state, rv.operands[1]));
          TypeId receiver_type =
              TypeOfPlace((*arena_)[*info->receiver], *types_);
          const auto& type = (*types_)[receiver_type];
          if (type.Kind() == TypeKind::kQueue) {
            TruncateToBound(elements, type.AsQueue().max_bound);
          }
          return std::monostate{};
        }

        case BuiltinMethod::kArrayDelete:
        case BuiltinMethod::kQueueDelete: {
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "delete() requires receiver");
          }
          AsArray(WritePlace(state, *info->receiver)).elements.clear();
          return std::monostate{};
        }

        case BuiltinMethod::kQueueDeleteAt: {
          if (!info->receiver) {
            throw common::InternalError(
                "EvalRvalue", "delete(idx) requires receiver");
          }
          TypeId idx_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
          auto idx_opt = TryGetIndex(
              EvalOperand(state, rv.operands[0]), *types_, idx_type);
          if (!idx_opt) {
            return std::monostate{};  // X/Z -> no-op
          }
          auto idx = *idx_opt;
          auto& elements = AsArray(WritePlace(state, *info->receiver)).elements;
          if (idx >= 0 && idx < static_cast<int64_t>(elements.size())) {
            elements.erase(elements.begin() + idx);
          }
          return std::monostate{};
        }
      }
      throw common::InternalError("EvalRvalue", "unknown builtin method");
    }
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

void Interpreter::ExecEffect(ProcessState& state, const Effect& effect) {
  std::visit(
      Overloaded{
          [&](const DisplayEffect& op) { ExecDisplayEffect(state, op); },
      },
      effect.op);
}

void Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  // Evaluate each argument with type provenance for semantic decisions
  std::vector<TypedValue> typed_args;
  typed_args.reserve(disp.args.size());
  for (const auto& arg : disp.args) {
    TypeId type = TypeOfOperand(arg, *arena_, *types_);
    RuntimeValue value = EvalOperand(state, arg);
    typed_args.push_back(TypedValue{.value = std::move(value), .type = type});
  }

  char default_format = RadixToFormatChar(disp.radix);
  FormatContext ctx{};
  std::string output = FormatMessage(typed_args, default_format, *types_, ctx);

  out << output;
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
    for (const BasicBlock& block : process.blocks) {
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
