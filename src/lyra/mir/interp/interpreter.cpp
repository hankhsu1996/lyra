#include "lyra/mir/interp/interpreter.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
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
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
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

// Sign-extends a value based on its bit width to produce a signed int64.
// For example, a 32-bit value 0xFFFFFFFF becomes -1 as int64_t.
auto SignExtendToInt64(uint64_t raw, uint32_t bit_width) -> int64_t {
  if (bit_width > 0 && bit_width < 64) {
    uint64_t sign_bit = 1ULL << (bit_width - 1);
    if ((raw & sign_bit) != 0) {
      uint64_t mask = ~((1ULL << bit_width) - 1);
      raw |= mask;
    }
  }
  return static_cast<int64_t>(raw);
}

// Gets the TypeId of an Operand.
auto TypeOfOperand(
    const Operand& op, const Arena& arena, const TypeArena& types) -> TypeId {
  switch (op.kind) {
    case Operand::Kind::kConst:
      return std::get<Constant>(op.payload).type;
    case Operand::Kind::kUse: {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      const auto& place = arena[place_id];
      return TypeOfPlace(types, place);
    }
    case Operand::Kind::kPoison:
      return TypeId{};  // Invalid type for poison
  }
  return TypeId{};
}

// Checks if a type is signed (handles both kIntegral and packed types).
auto IsSignedType(const TypeArena& types, TypeId type_id) -> bool {
  if (!type_id) {
    return false;
  }
  const auto& type = types[type_id];
  if (type.Kind() == TypeKind::kIntegral) {
    return type.AsIntegral().is_signed;
  }
  if (IsPacked(type)) {
    return IsPackedSigned(type, types);
  }
  return false;
}

// Backward-compatible alias for IsSignedType (only for kIntegral).
auto IsSignedIntegral(const TypeArena& types, TypeId type_id) -> bool {
  return IsSignedType(types, type_id);
}

// Result of evaluating an index operand with signedness information.
struct IndexValue {
  uint64_t raw;        // Raw bit pattern
  uint32_t bit_width;  // Original bit width
  bool is_signed;      // Whether the type is signed

  // Convert to int64_t, sign-extending if signed.
  [[nodiscard]] auto AsInt64() const -> int64_t {
    if (is_signed) {
      return SignExtendToInt64(raw, bit_width);
    }
    return static_cast<int64_t>(raw);
  }

  // Check if value is in bounds [lower, upper].
  // For unsigned indices with negative bounds, properly handles the mismatch.
  [[nodiscard]] auto InBounds(int64_t lower, int64_t upper) const -> bool {
    if (is_signed) {
      int64_t val = SignExtendToInt64(raw, bit_width);
      return val >= lower && val <= upper;
    }
    // Unsigned index: can never match negative bounds
    if (lower < 0) {
      // If upper is also negative, unsigned index is always out of bounds
      if (upper < 0) {
        return false;
      }
      // If lower is negative but upper is non-negative,
      // check if raw is in [0, upper]
      return std::cmp_less_equal(raw, upper);
    }
    // Both bounds are non-negative, compare as unsigned
    return std::cmp_greater_equal(raw, lower) &&
           std::cmp_less_equal(raw, upper);
  }
};

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
      std::visit(
          Overloaded{
              [](const FieldProjection&) {},
              [&](const IndexProjection& p) { Visit(p.index, arena); },
              [&](const SliceProjection& p) { Visit(p.start, arena); },
              [](const DerefProjection&) {},
              [&](const BitRangeProjection& p) { Visit(p.bit_offset, arena); },
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
        inst);
  }
};

// Helper: format severity prefix for $info/$warning/$error messages
auto FormatSeverityPrefix(Severity level) -> std::string_view {
  switch (level) {
    case Severity::kInfo:
      return "info: ";
    case Severity::kWarning:
      return "warning: ";
    case Severity::kError:
      return "error: ";
  }
  return "unknown: ";
}

// Apply a single non-BitSlice projection to navigate into a value.
// Returns a pointer to the sub-value and updates current_type.
template <typename ValuePtr, typename IndexEval>
auto ApplySingleProjection(
    ValuePtr current, TypeId& current_type, const Projection& proj,
    const TypeArena& types, IndexEval eval_index) -> ValuePtr {
  return std::visit(
      Overloaded{
          [&](const FieldProjection& fp) -> ValuePtr {
            if (!IsStruct(*current)) {
              throw common::InternalError(
                  "ApplyProjection", "field projection on non-struct");
            }
            auto& s = AsStruct(*current);
            auto idx = static_cast<size_t>(fp.field_index);
            if (idx >= s.fields.size()) {
              throw common::InternalError(
                  "ApplyProjection", "field index out of range");
            }
            if (current_type) {
              const auto& struct_info = types[current_type].AsUnpackedStruct();
              current_type = struct_info.fields[idx].type;
            }
            return &s.fields[idx];
          },
          [&](const IndexProjection& ip) -> ValuePtr {
            if (!IsArray(*current)) {
              throw common::InternalError(
                  "ApplyProjection", "index projection on non-array");
            }
            auto& a = AsArray(*current);
            int64_t index = eval_index(ip.index);

            // Get array bounds from type and normalize index to 0-based offset
            int64_t offset = index;
            auto size = static_cast<int64_t>(a.elements.size());
            if (current_type) {
              const auto& type_info = types[current_type];
              if (type_info.Kind() == TypeKind::kUnpackedArray) {
                const auto& array_info = type_info.AsUnpackedArray();
                offset = index - array_info.range.Lower();
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

            if (offset < 0 || offset >= size) {
              // OOB should never reach here - lowering should emit GuardedUse/
              // GuardedAssign for OOB-safe access per IEEE 1800-2023.
              throw common::InternalError(
                  "ApplyProjection",
                  std::format("array index {} out of bounds", index));
            }
            return &a.elements[static_cast<size_t>(offset)];
          },
          [](const SliceProjection& /*sp*/) -> ValuePtr {
            throw common::InternalError(
                "ApplyProjection", "slice projection not supported");
          },
          [](const DerefProjection& /*dp*/) -> ValuePtr {
            throw common::InternalError(
                "ApplyProjection", "deref projection not supported");
          },
          [](const BitRangeProjection& /*bp*/) -> ValuePtr {
            // BitRange should be handled specially, not here
            throw common::InternalError(
                "ApplyProjection", "BitRange must be handled separately");
          },
      },
      proj.info);
}

// Apply projections up to 'count', returning a pointer to the final value.
// Used internally; callers should use ApplyProjections which returns reference.
template <typename ValuePtr, typename IndexEval>
auto ApplyProjectionsPtr(
    ValuePtr root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index,
    size_t count) -> ValuePtr {
  ValuePtr current = root;
  TypeId current_type = root_type;

  for (size_t i = 0; i < count; ++i) {
    current = ApplySingleProjection<ValuePtr>(
        current, current_type, projections[i], types, eval_index);
  }
  return current;
}

template <typename IndexEval>
auto ApplyProjections(
    RuntimeValue& root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index)
    -> RuntimeValue& {
  return *ApplyProjectionsPtr<RuntimeValue*>(
      &root, projections, types, root_type, eval_index, projections.size());
}

template <typename IndexEval>
auto ApplyProjections(
    const RuntimeValue& root, const std::vector<Projection>& projections,
    const TypeArena& types, TypeId root_type, IndexEval eval_index)
    -> const RuntimeValue& {
  return *ApplyProjectionsPtr<const RuntimeValue*>(
      &root, projections, types, root_type, eval_index, projections.size());
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
              if (!IsPacked(type)) {
                throw common::InternalError(
                    "EvalOperand", "type mismatch: expected packed type");
              }
              return MakeIntegralFromConstant(
                  val, PackedBitWidth(type, *types_));
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
  return std::visit(
      Overloaded{
          [&](const UnaryRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "unary operation requires exactly 1 operand");
            }
            auto operand = EvalOperand(state, rv.operands[0]);
            return EvalUnary(info.op, operand);
          },
          [&](const BinaryRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 2) {
              throw common::InternalError(
                  "EvalRvalue", "binary operation requires exactly 2 operands");
            }
            auto lhs = EvalOperand(state, rv.operands[0]);
            auto rhs = EvalOperand(state, rv.operands[1]);
            return EvalBinary(info.op, lhs, rhs);
          },
          [&](const CastRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "cast operation requires exactly 1 operand");
            }
            auto operand = EvalOperand(state, rv.operands[0]);
            const auto& src_type = (*types_)[info.source_type];
            const auto& tgt_type = (*types_)[info.target_type];
            return EvalCast(operand, src_type, tgt_type, *types_);
          },
          [&](const SystemCallRvalueInfo& /*info*/) -> RuntimeValue {
            // System calls that produce values (pure functions like $clog2)
            // would be handled here. Currently all supported system calls are
            // effects.
            throw common::InternalError(
                "EvalRvalue", "pure system calls not yet supported");
          },
          [&](const UserCallRvalueInfo& info) -> RuntimeValue {
            // User function call - evaluate arguments and execute
            std::vector<RuntimeValue> args;
            args.reserve(rv.operands.size());
            for (const auto& operand : rv.operands) {
              args.push_back(EvalOperand(state, operand));
            }
            return RunFunction(info.callee, args, state.design_state);
          },
          [&](const AggregateRvalueInfo& info) -> RuntimeValue {
            const Type& type = (*types_)[info.result_type];

            // Handle struct aggregates
            if (type.Kind() == TypeKind::kUnpackedStruct) {
              const auto& struct_info = type.AsUnpackedStruct();
              if (rv.operands.size() != struct_info.fields.size()) {
                throw common::InternalError(
                    "EvalRvalue", "aggregate operand count mismatch");
              }
              std::vector<RuntimeValue> fields;
              fields.reserve(rv.operands.size());
              for (const auto& operand : rv.operands) {
                fields.push_back(EvalOperand(state, operand));
              }
              return MakeStruct(std::move(fields));
            }

            // Handle array/queue aggregates (unpacked array, dynamic array,
            // queue)
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

            throw common::InternalError(
                "EvalRvalue", "aggregate: unsupported type");
          },
          [&](const BuiltinCallRvalueInfo& info) -> RuntimeValue {
            switch (info.method) {
              case BuiltinMethod::kNewArray: {
                // new[size] or new[size](init)
                if (rv.operands.empty()) {
                  throw common::InternalError(
                      "EvalRvalue", "new[] requires size");
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
                // Sign-extend based on operand type to detect negatives
                uint64_t raw_bits =
                    size_int.value.empty() ? 0 : size_int.value[0];
                TypeId size_type =
                    TypeOfOperand(rv.operands[0], *arena_, *types_);
                const auto& type_info = (*types_)[size_type];
                if (type_info.Kind() == TypeKind::kIntegral) {
                  const auto& integral = type_info.AsIntegral();
                  if (integral.is_signed && integral.bit_width < 64) {
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

                // Get element type from result type
                const auto& array_type = (*types_)[info.result_type];
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
                  size_t copy_count =
                      std::min(new_size, init_arr.elements.size());
                  for (size_t i = 0; i < copy_count; ++i) {
                    elements.push_back(Clone(init_arr.elements[i]));
                  }
                  // Fill remaining with defaults
                  for (size_t i = copy_count; i < new_size; ++i) {
                    elements.push_back(
                        CreateDefaultValue(*types_, element_type));
                  }
                } else {
                  // new[size] - all default values
                  for (size_t i = 0; i < new_size; ++i) {
                    elements.push_back(
                        CreateDefaultValue(*types_, element_type));
                  }
                }
                return MakeArray(std::move(elements));
              }

              case BuiltinMethod::kArraySize:
              case BuiltinMethod::kQueueSize: {
                if (rv.operands.empty()) {
                  throw common::InternalError(
                      "EvalRvalue", "size() requires array");
                }
                auto arr_val = EvalOperand(state, rv.operands[0]);
                if (!IsArray(arr_val)) {
                  throw common::InternalError(
                      "EvalRvalue", "size() operand must be array");
                }
                auto size =
                    static_cast<int64_t>(AsArray(arr_val).elements.size());
                return MakeIntegral(size, 32);  // int is 32-bit
              }

              case BuiltinMethod::kQueuePopBack: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "pop_back() requires receiver");
                }
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                if (elements.empty()) {
                  return CreateDefaultValue(*types_, info.result_type);
                }
                RuntimeValue result = std::move(elements.back());
                elements.pop_back();
                return result;
              }

              case BuiltinMethod::kQueuePopFront: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "pop_front() requires receiver");
                }
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                if (elements.empty()) {
                  return CreateDefaultValue(*types_, info.result_type);
                }
                RuntimeValue result = std::move(elements.front());
                elements.erase(elements.begin());
                return result;
              }

              case BuiltinMethod::kQueuePushBack: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "push_back() requires receiver");
                }
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                elements.push_back(EvalOperand(state, rv.operands[0]));
                TypeId receiver_type =
                    TypeOfPlace(*types_, (*arena_)[*info.receiver]);
                const auto& type = (*types_)[receiver_type];
                if (type.Kind() == TypeKind::kQueue) {
                  TruncateToBound(elements, type.AsQueue().max_bound);
                }
                return std::monostate{};
              }

              case BuiltinMethod::kQueuePushFront: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "push_front() requires receiver");
                }
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                elements.insert(
                    elements.begin(), EvalOperand(state, rv.operands[0]));
                TypeId receiver_type =
                    TypeOfPlace(*types_, (*arena_)[*info.receiver]);
                const auto& type = (*types_)[receiver_type];
                if (type.Kind() == TypeKind::kQueue) {
                  TruncateToBound(elements, type.AsQueue().max_bound);
                }
                return std::monostate{};
              }

              case BuiltinMethod::kQueueInsert: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "insert() requires receiver");
                }
                TypeId idx_type =
                    TypeOfOperand(rv.operands[0], *arena_, *types_);
                auto idx_opt = TryGetIndex(
                    EvalOperand(state, rv.operands[0]), *types_, idx_type);
                if (!idx_opt) {
                  return std::monostate{};  // X/Z -> no-op
                }
                auto idx = *idx_opt;
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                if (idx < 0 || std::cmp_greater(idx, elements.size())) {
                  return std::monostate{};  // Invalid index -> no-op
                }
                elements.insert(
                    elements.begin() + idx, EvalOperand(state, rv.operands[1]));
                TypeId receiver_type =
                    TypeOfPlace(*types_, (*arena_)[*info.receiver]);
                const auto& type = (*types_)[receiver_type];
                if (type.Kind() == TypeKind::kQueue) {
                  TruncateToBound(elements, type.AsQueue().max_bound);
                }
                return std::monostate{};
              }

              case BuiltinMethod::kArrayDelete:
              case BuiltinMethod::kQueueDelete: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "delete() requires receiver");
                }
                AsArray(WritePlace(state, *info.receiver)).elements.clear();
                return std::monostate{};
              }

              case BuiltinMethod::kQueueDeleteAt: {
                if (!info.receiver) {
                  throw common::InternalError(
                      "EvalRvalue", "delete(idx) requires receiver");
                }
                TypeId idx_type =
                    TypeOfOperand(rv.operands[0], *arena_, *types_);
                auto idx_opt = TryGetIndex(
                    EvalOperand(state, rv.operands[0]), *types_, idx_type);
                if (!idx_opt) {
                  return std::monostate{};  // X/Z -> no-op
                }
                auto idx = *idx_opt;
                auto& elements =
                    AsArray(WritePlace(state, *info.receiver)).elements;
                if (idx >= 0 && std::cmp_less(idx, elements.size())) {
                  elements.erase(elements.begin() + idx);
                }
                return std::monostate{};
              }
            }
            throw common::InternalError("EvalRvalue", "unknown builtin method");
          },
          [&](const IndexValidityRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "index_validity requires exactly 1 operand");
            }
            auto index = EvalOperand(state, rv.operands[0]);
            if (!IsIntegral(index)) {
              throw common::InternalError(
                  "EvalRvalue", "index_validity operand must be integral");
            }
            const auto& idx_int = AsIntegral(index);

            // Check if index is known (no X/Z bits)
            bool is_known = idx_int.IsKnown();
            if (info.check_known && !is_known) {
              return MakeIntegral(0, 1);  // Invalid: X/Z index
            }

            // Check bounds using runtime integral comparison ops (handles wide
            // values). Create bounds constants at the same width as the index.
            TypeId index_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
            bool is_signed = IsSignedType(*types_, index_type);

            // Special case: unsigned index with negative bounds.
            // An unsigned value can never be less than 0, so:
            // - If both bounds are negative → always out of bounds
            // - If only lower is negative → treat lower as 0
            if (!is_signed) {
              if (info.lower_bound < 0) {
                if (info.upper_bound < 0) {
                  return MakeIntegral(0, 1);  // Always invalid
                }
                // lower is negative but upper is non-negative
                // Check only: index <= upper (lower effectively 0)
                auto upper_int = MakeIntegral(
                    static_cast<uint64_t>(info.upper_bound), idx_int.bit_width);
                const auto& upper_val = AsIntegral(upper_int);
                return IntegralLe(idx_int, upper_val, false);
              }
              // Both bounds non-negative, do normal unsigned comparison
              auto lower_int = MakeIntegral(
                  static_cast<uint64_t>(info.lower_bound), idx_int.bit_width);
              auto upper_int = MakeIntegral(
                  static_cast<uint64_t>(info.upper_bound), idx_int.bit_width);
              const auto& lower_val = AsIntegral(lower_int);
              const auto& upper_val = AsIntegral(upper_int);
              auto ge_lower = IntegralGe(idx_int, lower_val, false);
              auto le_upper = IntegralLe(idx_int, upper_val, false);
              return IntegralLogicalAnd(ge_lower, le_upper);
            }

            // Signed index: use signed comparison with proper sign extension
            auto lower_int =
                MakeIntegralSigned(info.lower_bound, idx_int.bit_width);
            auto upper_int =
                MakeIntegralSigned(info.upper_bound, idx_int.bit_width);
            const auto& lower_val = AsIntegral(lower_int);
            const auto& upper_val = AsIntegral(upper_int);
            auto ge_lower = IntegralGe(idx_int, lower_val, true);
            auto le_upper = IntegralLe(idx_int, upper_val, true);
            return IntegralLogicalAnd(ge_lower, le_upper);
          },
          [&](const GuardedUseRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "guarded_use requires exactly 1 operand");
            }
            auto validity = EvalOperand(state, rv.operands[0]);
            if (!IsIntegral(validity)) {
              throw common::InternalError(
                  "EvalRvalue", "guarded_use validity must be integral");
            }
            const auto& valid_int = AsIntegral(validity);

            // If valid, read from place
            if (!valid_int.IsZero()) {
              return ReadPlace(state, info.place);
            }

            // OOB: return default based on result type
            return CreateDefaultValue(*types_, info.result_type);
          },
      },
      rv.info);
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

auto Interpreter::ApplyProjectionsForRead(
    const ProcessState& state, const Place& place, const RuntimeValue& root)
    -> ConstLocation {
  ConstLocation loc{.base = &root, .bit_slice = std::nullopt};

  for (const auto& proj : place.projections) {
    std::visit(
        Overloaded{
            [&](const FieldProjection& fp) {
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "Field projection after BitRange");
              }
              if (!IsStruct(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "field projection on non-struct");
              }
              const auto& s = AsStruct(*loc.base);
              auto idx = static_cast<size_t>(fp.field_index);
              if (idx >= s.fields.size()) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "field index out of range");
              }
              loc.base = &s.fields[idx];
            },
            [&](const IndexProjection& ip) {
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "Index projection after BitRange");
              }
              if (!IsArray(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "index projection on non-array");
              }
              const auto& arr = AsArray(*loc.base);

              auto idx_val = EvalOperand(state, ip.index);
              if (!IsIntegral(idx_val)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "index must be integral");
              }
              const auto& idx_int = AsIntegral(idx_val);
              if (!idx_int.IsKnown()) {
                throw std::runtime_error("index is X/Z");
              }
              uint64_t raw = idx_int.value.empty() ? 0 : idx_int.value[0];
              TypeId type_id = TypeOfOperand(ip.index, *arena_, *types_);
              int64_t idx = 0;
              if (IsSignedIntegral(*types_, type_id)) {
                idx = SignExtendToInt64(raw, idx_int.bit_width);
              } else {
                idx = static_cast<int64_t>(raw);
              }

              // MIR lowering normalizes indices to 0-based
              if (idx < 0 || static_cast<size_t>(idx) >= arr.elements.size()) {
                // TODO(hankhsu): LRM says return X for OOB reads
                // For now, throw until 4-state semantics are implemented
                throw std::runtime_error("array index out of bounds");
              }
              loc.base = &arr.elements[static_cast<size_t>(idx)];
            },
            [&](const BitRangeProjection& br) {
              // Verify base is integral before first BitRange
              if (!loc.bit_slice && !IsIntegral(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "BitRangeProjection on non-integral base");
              }

              auto offset_val = EvalOperand(state, br.bit_offset);
              if (!IsIntegral(offset_val)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "bit offset must be integral");
              }
              const auto& offset_int = AsIntegral(offset_val);
              int64_t raw_offset =
                  offset_int.value.empty()
                      ? 0
                      : static_cast<int64_t>(offset_int.value[0]);
              // Check for negative
              if (raw_offset < 0) {
                // TODO(hankhsu): LRM defines behavior for negative offsets
                // (return X) For now, throw until 4-state semantics are
                // implemented
                throw std::runtime_error("negative bit offset");
              }
              auto offset = static_cast<uint64_t>(raw_offset);

              if (loc.bit_slice) {
                // Accumulate: base stays same (container), add offsets
                loc.bit_slice->total_offset += offset;
                loc.bit_slice->width = br.width;  // innermost wins
                loc.bit_slice->element_type =
                    br.element_type;  // innermost wins
              } else {
                // First BitRange: base does NOT change - it stays as container
                loc.bit_slice = BitSlice{
                    .total_offset = offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
              }
            },
            [](const SliceProjection&) {
              throw common::InternalError(
                  "ApplyProjectionsForRead", "SliceProjection not supported");
            },
            [](const DerefProjection&) {
              throw common::InternalError(
                  "ApplyProjectionsForRead", "DerefProjection not supported");
            },
        },
        proj.info);
  }
  return loc;
}

auto Interpreter::ApplyProjectionsForWrite(
    ProcessState& state, const Place& place, RuntimeValue& root) -> Location {
  Location loc{.base = &root, .bit_slice = std::nullopt};

  for (const auto& proj : place.projections) {
    std::visit(
        Overloaded{
            [&](const FieldProjection& fp) {
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "Field projection after BitRange");
              }
              if (!IsStruct(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "field projection on non-struct");
              }
              auto& s = AsStruct(*loc.base);
              auto idx = static_cast<size_t>(fp.field_index);
              if (idx >= s.fields.size()) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite", "field index out of range");
              }
              loc.base = &s.fields[idx];
            },
            [&](const IndexProjection& ip) {
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "Index projection after BitRange");
              }
              if (!IsArray(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "index projection on non-array");
              }
              auto& arr = AsArray(*loc.base);

              auto idx_val = EvalOperand(state, ip.index);
              if (!IsIntegral(idx_val)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite", "index must be integral");
              }
              const auto& idx_int = AsIntegral(idx_val);
              if (!idx_int.IsKnown()) {
                throw std::runtime_error("index is X/Z");
              }
              uint64_t raw = idx_int.value.empty() ? 0 : idx_int.value[0];
              TypeId type_id = TypeOfOperand(ip.index, *arena_, *types_);
              int64_t idx = 0;
              if (IsSignedIntegral(*types_, type_id)) {
                idx = SignExtendToInt64(raw, idx_int.bit_width);
              } else {
                idx = static_cast<int64_t>(raw);
              }

              // MIR lowering normalizes indices to 0-based
              if (idx < 0 || static_cast<size_t>(idx) >= arr.elements.size()) {
                // TODO(hankhsu): LRM defines behavior for OOB writes
                // For now, throw until 4-state semantics are implemented
                throw std::runtime_error("array index out of bounds");
              }
              loc.base = &arr.elements[static_cast<size_t>(idx)];
            },
            [&](const BitRangeProjection& br) {
              // Verify base is integral before first BitRange
              if (!loc.bit_slice && !IsIntegral(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "BitRangeProjection on non-integral base");
              }

              auto offset_val = EvalOperand(state, br.bit_offset);
              if (!IsIntegral(offset_val)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite", "bit offset must be integral");
              }
              const auto& offset_int = AsIntegral(offset_val);
              int64_t raw_offset =
                  offset_int.value.empty()
                      ? 0
                      : static_cast<int64_t>(offset_int.value[0]);
              // Check for negative
              if (raw_offset < 0) {
                // TODO(hankhsu): LRM defines behavior for negative offsets
                // For now, throw until 4-state semantics are implemented
                throw std::runtime_error("negative bit offset");
              }
              auto offset = static_cast<uint64_t>(raw_offset);

              if (loc.bit_slice) {
                // Accumulate: base stays same (container), add offsets
                loc.bit_slice->total_offset += offset;
                loc.bit_slice->width = br.width;  // innermost wins
                loc.bit_slice->element_type =
                    br.element_type;  // innermost wins
              } else {
                // First BitRange: base does NOT change - it stays as container
                loc.bit_slice = BitSlice{
                    .total_offset = offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
              }
            },
            [](const SliceProjection&) {
              throw common::InternalError(
                  "ApplyProjectionsForWrite", "SliceProjection not supported");
            },
            [](const DerefProjection&) {
              throw common::InternalError(
                  "ApplyProjectionsForWrite", "DerefProjection not supported");
            },
        },
        proj.info);
  }
  return loc;
}

auto Interpreter::ReadPlace(const ProcessState& state, PlaceId place_id)
    -> RuntimeValue {
  const auto& place = (*arena_)[place_id];
  const auto& root_value = ResolveRoot(state, place.root);

  if (place.projections.empty()) {
    return Clone(root_value);
  }

  auto loc = ApplyProjectionsForRead(state, place, root_value);
  if (loc.bit_slice) {
    const auto& bs = *loc.bit_slice;
    const auto& container = AsIntegral(*loc.base);
    // Overflow-safe check using uint64_t arithmetic
    if (bs.total_offset + bs.width > container.bit_width) {
      // TODO(hankhsu): LRM says return X for invalid slice (defined behavior)
      // For now, throw until 4-state semantics are implemented
      throw std::runtime_error("bit slice exceeds container width");
    }
    return IntegralExtractSlice(
        container, static_cast<uint32_t>(bs.total_offset), bs.width);
  }
  return Clone(*loc.base);
}

auto Interpreter::WritePlace(ProcessState& state, PlaceId place_id)
    -> RuntimeValue& {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    return root_value;
  }

  // This function returns a reference, so BitRange is not allowed (can't
  // return reference to computed bit slice). Callers needing BitRange should
  // use StoreToPlace instead.
  auto loc = ApplyProjectionsForWrite(state, place, root_value);
  if (loc.bit_slice) {
    throw common::InternalError(
        "WritePlace", "BitRange requires StoreToPlace path");
  }
  return *loc.base;
}

void Interpreter::StoreToPlace(
    ProcessState& state, PlaceId place_id, RuntimeValue value) {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    root_value = std::move(value);
    return;
  }

  auto loc = ApplyProjectionsForWrite(state, place, root_value);
  if (loc.bit_slice) {
    const auto& bs = *loc.bit_slice;
    auto& container = AsIntegral(*loc.base);
    // Overflow-safe check using uint64_t arithmetic
    if (bs.total_offset + bs.width > container.bit_width) {
      // TODO(hankhsu): LRM defines write behavior for invalid slice
      // For now, throw until 4-state semantics are implemented
      throw std::runtime_error("bit slice exceeds container width");
    }
    // Validate value is integral with matching width (sanity check - MIR
    // guarantees this)
    if (!IsIntegral(value)) {
      throw common::InternalError(
          "StoreToPlace", "writing non-integral value to bit slice");
    }
    const auto& val_integral = AsIntegral(value);
    if (val_integral.bit_width != bs.width) {
      throw common::InternalError("StoreToPlace", "bit slice width mismatch");
    }
    *loc.base = IntegralInsertSlice(
        container, val_integral, static_cast<uint32_t>(bs.total_offset),
        bs.width);
  } else {
    *loc.base = std::move(value);
  }
}

void Interpreter::ExecAssign(ProcessState& state, const Assign& assign) {
  auto value = EvalOperand(state, assign.source);
  StoreToPlace(state, assign.target, std::move(value));
}

void Interpreter::ExecCompute(ProcessState& state, const Compute& compute) {
  auto value = EvalRvalue(state, compute.value);
  StoreToPlace(state, compute.target, std::move(value));
}

void Interpreter::ExecEffect(ProcessState& state, const Effect& effect) {
  std::visit(
      Overloaded{
          [&](const DisplayEffect& op) { ExecDisplayEffect(state, op); },
          [&](const SeverityEffect& op) { ExecSeverityEffect(state, op); },
      },
      effect.op);
}

void Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  FormatContext ctx{};

  for (const auto& op : disp.ops) {
    if (op.kind == FormatKind::kLiteral) {
      out << op.literal;
    } else {
      // Evaluate the value and format it
      RuntimeValue value = EvalOperand(state, *op.value);
      TypedValue typed{.value = std::move(value), .type = op.type};

      // Convert FormatKind and modifiers to FormatSpec
      FormatSpec spec{
          .spec = FormatKindToSpecChar(op.kind),
          .zero_pad = op.mods.zero_pad,
          .left_align = op.mods.left_align,
          .width = op.mods.width,
          .precision = op.mods.precision};

      out << FormatValue(typed, spec, *types_, ctx);
    }
  }

  if (disp.print_kind == PrintKind::kDisplay) {
    out << "\n";
  }
}

void Interpreter::ExecSeverityEffect(
    const ProcessState& state, const SeverityEffect& severity) {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  // Print severity prefix
  out << FormatSeverityPrefix(severity.level);

  // Evaluate and format message arguments
  std::vector<TypedValue> typed_args;
  typed_args.reserve(severity.args.size());
  for (const auto& arg : severity.args) {
    TypeId type = TypeOfOperand(arg, *arena_, *types_);
    RuntimeValue value = EvalOperand(state, arg);
    typed_args.push_back(TypedValue{.value = std::move(value), .type = type});
  }

  // Use decimal as default radix for severity messages
  FormatContext ctx{};
  std::string message = FormatMessage(typed_args, 'd', *types_, ctx);

  out << message << "\n";
}

void Interpreter::ExecGuardedAssign(
    ProcessState& state, const GuardedAssign& guarded) {
  // Always evaluate source unconditionally (per spec: only the write is
  // guarded)
  auto value = EvalOperand(state, guarded.source);

  // Evaluate validity predicate
  auto validity = EvalOperand(state, guarded.validity);
  if (!IsIntegral(validity)) {
    throw common::InternalError(
        "ExecGuardedAssign", "validity must be integral");
  }
  const auto& valid_int = AsIntegral(validity);

  // If invalid (OOB/X/Z), the write is a no-op
  if (valid_int.IsZero()) {
    return;
  }

  // Valid: perform the assignment
  StoreToPlace(state, guarded.target, std::move(value));
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
        } else if constexpr (std::is_same_v<T, GuardedAssign>) {
          ExecGuardedAssign(state, i);
        } else if constexpr (std::is_same_v<T, Effect>) {
          ExecEffect(state, i);
        }
      },
      inst);
}

auto Interpreter::ExecTerminator(ProcessState& state, const Terminator& term)
    -> std::optional<BasicBlockId> {
  return std::visit(
      Overloaded{
          [](const Jump& t) -> std::optional<BasicBlockId> { return t.target; },

          [&](const Branch& t) -> std::optional<BasicBlockId> {
            auto cond = ReadPlace(state, t.condition);
            if (!IsIntegral(cond)) {
              throw common::InternalError(
                  "ExecTerminator", "branch condition must be integral");
            }
            const auto& cond_int = AsIntegral(cond);
            if (!cond_int.IsKnown()) {
              throw std::runtime_error("branch condition is X/Z");
            }
            return cond_int.IsZero() ? t.else_target : t.then_target;
          },

          [&](const Switch& t) -> std::optional<BasicBlockId> {
            if (t.targets.empty()) {
              throw common::InternalError(
                  "ExecTerminator", "switch terminator has no targets");
            }
            auto selector = ReadPlace(state, t.selector);
            if (!IsIntegral(selector)) {
              throw common::InternalError(
                  "ExecTerminator", "switch selector must be integral");
            }
            const auto& sel_int = AsIntegral(selector);
            if (!sel_int.IsKnown()) {
              return t.targets.back();
            }
            uint64_t val = sel_int.value.empty() ? 0 : sel_int.value[0];
            if (val >= t.targets.size() - 1) {
              return t.targets.back();
            }
            return t.targets[static_cast<size_t>(val)];
          },

          [&](const QualifiedDispatch& t) -> std::optional<BasicBlockId> {
            // Validate invariant: targets = one per condition + else
            if (t.targets.size() != t.conditions.size() + 1) {
              throw common::InternalError(
                  "ExecTerminator",
                  "QualifiedDispatch: targets.size() != conditions.size() + 1");
            }

            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            // Read all condition values and count how many are true
            size_t first_true_index = t.conditions.size();  // sentinel
            size_t true_count = 0;
            for (size_t i = 0; i < t.conditions.size(); ++i) {
              auto cond = ReadPlace(state, t.conditions[i]);
              if (!IsIntegral(cond)) {
                throw common::InternalError(
                    "ExecTerminator",
                    "QualifiedDispatch condition must be integral");
              }
              const auto& cond_int = AsIntegral(cond);
              // Throw on X/Z conditions (same as Branch)
              if (!cond_int.IsKnown()) {
                throw std::runtime_error("QualifiedDispatch condition is X/Z");
              }
              if (!cond_int.IsZero()) {
                ++true_count;
                if (first_true_index == t.conditions.size()) {
                  first_true_index = i;
                }
              }
            }

            const char* qualifier_name =
                (t.qualifier == DispatchQualifier::kUnique) ? "unique"
                                                            : "unique0";
            const char* stmt_name =
                (t.statement_kind == DispatchStatementKind::kIf) ? "if"
                                                                 : "case";

            // Check for overlap (multiple conditions true)
            if (true_count > 1) {
              const char* what =
                  (t.statement_kind == DispatchStatementKind::kIf)
                      ? "multiple conditions true"
                      : "multiple case items match";
              out << "warning: " << what << " in " << qualifier_name << " "
                  << stmt_name << "\n";
            }

            // Check for no-match (only for kUnique, and only if no else)
            if (true_count == 0 && !t.has_else &&
                t.qualifier == DispatchQualifier::kUnique) {
              const char* what =
                  (t.statement_kind == DispatchStatementKind::kIf)
                      ? "no condition matched"
                      : "no matching case item";
              out << "warning: " << what << " in " << qualifier_name << " "
                  << stmt_name << "\n";
            }

            // Dispatch to first true target or else target (last one)
            if (first_true_index < t.conditions.size()) {
              return t.targets[first_true_index];
            }
            return t.targets.back();
          },

          [](const Delay& /*t*/) -> std::optional<BasicBlockId> {
            throw common::InternalError(
                "ExecTerminator",
                "delay terminator requires runtime/scheduler");
          },

          [](const Wait& /*t*/) -> std::optional<BasicBlockId> {
            throw common::InternalError(
                "ExecTerminator", "wait terminator requires runtime/scheduler");
          },

          [](const Return& /*t*/) -> std::optional<BasicBlockId> {
            return std::nullopt;
          },

          [&](const Finish& t) -> std::optional<BasicBlockId> {
            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            if (t.kind == TerminationKind::kFatal) {
              if (t.level >= 1) {
                out << "fatal: ";
                if (!t.message_args.empty()) {
                  std::vector<TypedValue> typed_args;
                  typed_args.reserve(t.message_args.size());
                  for (const auto& arg : t.message_args) {
                    TypeId type = TypeOfOperand(arg, *arena_, *types_);
                    RuntimeValue value = EvalOperand(state, arg);
                    typed_args.push_back(
                        TypedValue{.value = std::move(value), .type = type});
                  }
                  FormatContext ctx{};
                  std::string message =
                      FormatMessage(typed_args, 'd', *types_, ctx);
                  out << message;
                }
                out << "\n";
              }
            } else if (t.level >= 1) {
              const char* name = nullptr;
              switch (t.kind) {
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
              out << name << " called at time 0\n";
            }
            return std::nullopt;
          },

          [](const Repeat& /*t*/) -> std::optional<BasicBlockId> {
            throw common::InternalError(
                "ExecTerminator",
                "repeat terminator requires runtime/scheduler");
          },
      },
      term);
}

auto Interpreter::RunUntilSuspend(ProcessState& state) -> SuspendReason {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      ExecInstruction(state, block.instructions[state.instruction_index]);
      state.instruction_index++;
    }

    // Execute terminator
    auto next_block = ExecTerminator(state, block.terminator);

    if (next_block) {
      // Continue to next block
      state.current_block = *next_block;
      state.instruction_index = 0;
    } else {
      // Process finished (Return or Finish terminator)
      state.status = ProcessStatus::kFinished;
      return SuspendFinished{};
    }
  }

  return SuspendFinished{};
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

}  // namespace lyra::mir::interp
