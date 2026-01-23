#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/interp/eval_ops.hpp"
#include "lyra/mir/interp/format.hpp"
#include "lyra/mir/interp/interp_helpers.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/semantic/format.hpp"

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
              const auto& type = (*types_)[c.type];
              if (type.Kind() == TypeKind::kShortReal) {
                return MakeShortReal(static_cast<float>(val.value));
              }
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

auto Interpreter::EvalRvalue(
    ProcessState& state, const Rvalue& rv, TypeId result_type) -> RuntimeValue {
  return std::visit(
      Overloaded{
          [&](const UnaryRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "unary operation requires exactly 1 operand");
            }
            auto operand = EvalOperand(state, rv.operands[0]);
            return EvalUnary(info.op, operand, result_type, *types_);
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
          [&](const BitCastRvalueInfo& info) -> RuntimeValue {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "bitcast operation requires exactly 1 operand");
            }
            auto operand = EvalOperand(state, rv.operands[0]);
            return EvalBitCast(
                operand, info.source_type, info.target_type, *types_);
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

              case BuiltinMethod::kEnumNext:
              case BuiltinMethod::kEnumPrev: {
                if (!info.enum_type) {
                  throw common::InternalError(
                      "EvalRvalue", "enum next/prev requires enum_type");
                }
                const Type& enum_type = (*types_)[*info.enum_type];
                if (enum_type.Kind() != TypeKind::kEnum) {
                  throw common::InternalError(
                      "EvalRvalue", "enum_type is not kEnum");
                }
                const auto& enum_info = enum_type.AsEnum();
                const auto& members = enum_info.members;
                if (members.empty()) {
                  throw common::InternalError(
                      "EvalRvalue", "enum has no members");
                }

                // Get bit width (use PackedBitWidth since base can be packed
                // array)
                auto bit_width = PackedBitWidth(enum_type, *types_);

                // Get current value from operands[0]
                auto current_val = EvalOperand(state, rv.operands[0]);
                if (!IsIntegral(current_val)) {
                  throw common::InternalError(
                      "EvalRvalue", "enum value must be integral");
                }
                const auto& current_int = AsIntegral(current_val);

                // X/Z anywhere → invalid
                bool is_invalid = !current_int.IsKnown();

                // Get step from operands[1] (default 1 if no operand)
                size_t step = 1;
                if (rv.operands.size() > 1) {
                  auto step_val = EvalOperand(state, rv.operands[1]);
                  const auto& step_int = AsIntegral(step_val);
                  if (!step_int.IsKnown()) {
                    throw std::runtime_error(
                        "enum next/prev step cannot be X/Z");
                  }
                  // Step is expected to be an int - extract low 32 bits
                  auto lo = step_int.value.empty()
                                ? 0ULL
                                : step_int.value[0] & 0xFFFF'FFFFU;
                  step = static_cast<size_t>(lo);
                }

                // Find current index by bitwise equality
                size_t n = members.size();
                size_t current_idx = n;  // n means "not found"
                if (!is_invalid) {
                  for (size_t i = 0; i < n; ++i) {
                    auto member_ri =
                        MakeIntegralFromConstant(members[i].value, bit_width);
                    auto eq = IntegralEq(current_int, AsIntegral(member_ri));
                    if (eq.IsKnown() && !eq.value.empty() && eq.value[0] == 1) {
                      current_idx = i;
                      break;  // First match wins for duplicate values
                    }
                  }
                }

                // Compute result index using size_t math
                size_t step_mod = step % n;
                size_t result_idx = 0;
                if (current_idx >= n) {
                  // Invalid value: next→first, prev→last
                  result_idx =
                      (info.method == BuiltinMethod::kEnumNext) ? 0 : n - 1;
                } else if (info.method == BuiltinMethod::kEnumNext) {
                  result_idx = (current_idx + step_mod) % n;
                } else {
                  // prev: add (n - step_mod) to avoid underflow
                  result_idx = (current_idx + (n - step_mod)) % n;
                }

                // Return member value as RuntimeIntegral
                return MakeIntegralFromConstant(
                    members[result_idx].value, bit_width);
              }

              case BuiltinMethod::kEnumName: {
                if (!info.enum_type) {
                  throw common::InternalError(
                      "EvalRvalue", "name() requires enum_type");
                }
                const Type& enum_type = (*types_)[*info.enum_type];
                if (enum_type.Kind() != TypeKind::kEnum) {
                  throw common::InternalError(
                      "EvalRvalue", "enum_type is not kEnum");
                }
                const auto& enum_info = enum_type.AsEnum();
                auto bit_width = PackedBitWidth(enum_type, *types_);

                auto current_val = EvalOperand(state, rv.operands[0]);
                if (!IsIntegral(current_val)) {
                  return MakeString("");
                }
                const auto& current_int = AsIntegral(current_val);

                // X/Z → empty string
                if (!current_int.IsKnown()) {
                  return MakeString("");
                }

                // Find first matching member name
                for (const auto& member : enum_info.members) {
                  auto member_ri =
                      MakeIntegralFromConstant(member.value, bit_width);
                  auto eq = IntegralEq(current_int, AsIntegral(member_ri));
                  if (eq.IsKnown() && !eq.value.empty() && eq.value[0] == 1) {
                    return MakeString(member.name);
                  }
                }

                // Invalid value → empty string
                return MakeString("");
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
          [&](const ConcatRvalueInfo& info) -> RuntimeValue {
            const Type& result_type = (*types_)[info.result_type];

            // String concatenation (byte-based)
            if (result_type.Kind() == TypeKind::kString) {
              std::string result;
              for (const auto& operand : rv.operands) {
                auto val = EvalOperand(state, operand);
                if (IsString(val)) {
                  result += AsString(val).value;
                } else if (IsIntegral(val)) {
                  result += semantic::PackedToStringBytes(AsIntegral(val));
                } else {
                  throw common::InternalError(
                      "EvalRvalue",
                      "string concat operand must be string or integral");
                }
              }
              return MakeString(std::move(result));
            }

            // Integral concatenation (bit-based)
            uint32_t result_width = PackedBitWidth(result_type, *types_);

            // Evaluate all operands and compute total width
            std::vector<RuntimeIntegral> values;
            values.reserve(rv.operands.size());
            uint32_t total_width = 0;
            for (const auto& operand : rv.operands) {
              auto val = EvalOperand(state, operand);
              RuntimeIntegral integral;
              if (IsString(val)) {
                // String literal in packed concat: convert to integral
                TypeId op_type = TypeOfOperand(operand, *arena_, *types_);
                const Type& op_type_ref = (*types_)[op_type];
                uint32_t op_width = 0;
                if (IsPacked(op_type_ref)) {
                  op_width = PackedBitWidth(op_type_ref, *types_);
                } else {
                  // String type: derive width from string length (8 bits/char)
                  op_width =
                      static_cast<uint32_t>(AsString(val).value.size()) * 8;
                }
                integral = StringBytesToIntegral(AsString(val).value, op_width);
              } else {
                integral = std::move(AsIntegral(val));
              }
              total_width += integral.bit_width;
              values.push_back(std::move(integral));
            }

            if (total_width != result_width) {
              throw common::InternalError(
                  "EvalRvalue",
                  std::format(
                      "concat operand widths {} != result width {}",
                      total_width, result_width));
            }

            // Build result from MSB to LSB using 4-state insert
            auto result = MakeKnownIntegral(result_width);
            uint32_t bit_pos = result_width;
            for (const auto& val : values) {
              bit_pos -= val.bit_width;
              result = IntegralInsertSlice4State(
                  result, val, bit_pos, val.bit_width);
            }
            return result;
          },
          [&](const SFormatRvalueInfo& info) -> RuntimeValue {
            FormatContext ctx{};
            std::string result;

            if (!info.ops.empty()) {
              // Compile-time path: iterate ops like ExecDisplayEffect
              for (const auto& op : info.ops) {
                if (op.kind == FormatKind::kLiteral) {
                  result += op.literal;
                } else {
                  RuntimeValue value = EvalOperand(state, *op.value);
                  TypedValue typed{.value = std::move(value), .type = op.type};
                  FormatSpec spec{
                      .kind = op.kind,
                      .width = op.mods.width,
                      .precision = op.mods.precision,
                      .zero_pad = op.mods.zero_pad,
                      .left_align = op.mods.left_align};
                  result += FormatValue(typed, spec, *types_, ctx);
                }
              }
            } else if (info.has_runtime_format) {
              // Runtime format path: rv.operands[0] is format string,
              // rv.operands[1..] are value args
              RuntimeValue fmt_val = EvalOperand(state, rv.operands[0]);
              std::string fmt_str = AsString(fmt_val).value;
              std::vector<TypedValue> value_args;
              for (size_t i = 1; i < rv.operands.size(); ++i) {
                TypeId type = TypeOfOperand(rv.operands[i], *arena_, *types_);
                value_args.push_back(
                    {EvalOperand(state, rv.operands[i]), type});
              }
              char fmt_char = FormatKindToSpecChar(info.default_format);
              result =
                  FormatDisplay(fmt_str, value_args, fmt_char, *types_, ctx);
            } else {
              // Auto-format path: all rv.operands are value args
              std::vector<TypedValue> typed_args;
              for (const auto& operand : rv.operands) {
                TypeId type = TypeOfOperand(operand, *arena_, *types_);
                typed_args.push_back({EvalOperand(state, operand), type});
              }
              char fmt_char = FormatKindToSpecChar(info.default_format);
              result = FormatMessage(typed_args, fmt_char, *types_, ctx);
            }

            return MakeString(std::move(result));
          },
      },
      rv.info);
}

}  // namespace lyra::mir::interp
