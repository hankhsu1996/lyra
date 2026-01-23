#include <algorithm>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
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
#include "lyra/mir/interp/blob_codec.hpp"
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

auto CreateDefaultValue(const TypeArena& types, TypeId type_id)
    -> RuntimeValue {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kVoid:
      return std::monostate{};
    case TypeKind::kIntegral: {
      const auto& info = type.AsIntegral();
      if (info.is_four_state) {
        return MakeIntegralX(info.bit_width);
      }
      return MakeIntegral(0, info.bit_width);
    }
    case TypeKind::kPackedArray: {
      uint32_t total_width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        return MakeIntegralX(total_width);
      }
      return MakeIntegral(0, total_width);
    }
    case TypeKind::kPackedStruct: {
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
      RuntimeValue member0_default =
          CreateDefaultValue(types, info.members[0].type);
      StoreToBlob(info.members[0].type, member0_default, storage, 0, types);
      return MakeUnion(std::move(storage));
    }
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return MakeArray({});
    case TypeKind::kEnum: {
      uint32_t bit_width = PackedBitWidth(type, types);
      if (IsPackedFourState(type, types)) {
        return MakeIntegralX(bit_width);
      }
      return MakeIntegral(0, bit_width);
    }
  }
  throw common::InternalError("CreateDefaultValue", "unknown type kind");
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
          [&](const SystemCallRvalueInfo&) -> RuntimeValue {
            throw common::InternalError(
                "EvalRvalue", "pure system calls not yet supported");
          },
          [&](const UserCallRvalueInfo& info) -> RuntimeValue {
            std::vector<RuntimeValue> args;
            args.reserve(rv.operands.size());
            for (const auto& operand : rv.operands) {
              args.push_back(EvalOperand(state, operand));
            }
            return RunFunction(info.callee, args, state.design_state);
          },
          [&](const AggregateRvalueInfo&) -> RuntimeValue {
            return EvalAggregate(state, rv);
          },
          [&](const BuiltinCallRvalueInfo&) -> RuntimeValue {
            return EvalBuiltinCall(state, rv);
          },
          [&](const IndexValidityRvalueInfo&) -> RuntimeValue {
            return EvalIndexValidity(state, rv);
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
            if (!valid_int.IsZero()) {
              return ReadPlace(state, info.place);
            }
            return CreateDefaultValue(*types_, info.result_type);
          },
          [&](const ConcatRvalueInfo&) -> RuntimeValue {
            return EvalConcat(state, rv);
          },
          [&](const SFormatRvalueInfo&) -> RuntimeValue {
            return EvalSFormat(state, rv);
          },
          [&](const PlusargsRvalueInfo&) -> RuntimeValue {
            return EvalPlusargs(state, rv);
          },
      },
      rv.info);
}

auto Interpreter::EvalAggregate(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<AggregateRvalueInfo>(rv.info);
  const Type& type = (*types_)[info.result_type];

  if (type.Kind() == TypeKind::kUnpackedStruct) {
    const auto& struct_info = type.AsUnpackedStruct();
    if (rv.operands.size() != struct_info.fields.size()) {
      throw common::InternalError(
          "EvalAggregate", "aggregate operand count mismatch");
    }
    std::vector<RuntimeValue> fields;
    fields.reserve(rv.operands.size());
    for (const auto& operand : rv.operands) {
      fields.push_back(EvalOperand(state, operand));
    }
    return MakeStruct(std::move(fields));
  }

  if (type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    std::vector<RuntimeValue> elements;
    elements.reserve(rv.operands.size());
    for (const auto& operand : rv.operands) {
      elements.push_back(EvalOperand(state, operand));
    }
    if (type.Kind() == TypeKind::kQueue) {
      TruncateToBound(elements, type.AsQueue().max_bound);
    }
    return MakeArray(std::move(elements));
  }

  throw common::InternalError("EvalAggregate", "unsupported type");
}

auto Interpreter::EvalNewArray(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  if (rv.operands.empty()) {
    throw common::InternalError("EvalNewArray", "new[] requires size");
  }
  auto size_val = EvalOperand(state, rv.operands[0]);
  if (!IsIntegral(size_val)) {
    throw common::InternalError("EvalNewArray", "new[] size must be integral");
  }
  const auto& size_int = AsIntegral(size_val);
  if (!size_int.IsKnown()) {
    throw std::runtime_error("new[] size is X/Z");
  }

  // Sign-extend based on operand type to detect negatives
  uint64_t raw_bits = size_int.value.empty() ? 0 : size_int.value[0];
  TypeId size_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
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
        "EvalNewArray", "new[] result must be dynamic array");
  }
  TypeId element_type = array_type.AsDynamicArray().element_type;

  std::vector<RuntimeValue> elements;
  elements.reserve(new_size);

  if (rv.operands.size() > 1) {
    // new[size](init) - copy from initializer
    auto init_val = EvalOperand(state, rv.operands[1]);
    if (!IsArray(init_val)) {
      throw common::InternalError("EvalNewArray", "new[] init must be array");
    }
    const auto& init_arr = AsArray(init_val);
    size_t copy_count = std::min(new_size, init_arr.elements.size());
    for (size_t i = 0; i < copy_count; ++i) {
      elements.push_back(Clone(init_arr.elements[i]));
    }
    for (size_t i = copy_count; i < new_size; ++i) {
      elements.push_back(CreateDefaultValue(*types_, element_type));
    }
  } else {
    for (size_t i = 0; i < new_size; ++i) {
      elements.push_back(CreateDefaultValue(*types_, element_type));
    }
  }
  return MakeArray(std::move(elements));
}

auto Interpreter::EvalEnumNextPrev(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  if (!info.enum_type) {
    throw common::InternalError(
        "EvalEnumNextPrev", "enum next/prev requires enum_type");
  }
  const Type& enum_type = (*types_)[*info.enum_type];
  if (enum_type.Kind() != TypeKind::kEnum) {
    throw common::InternalError("EvalEnumNextPrev", "enum_type is not kEnum");
  }
  const auto& enum_info = enum_type.AsEnum();
  const auto& members = enum_info.members;
  if (members.empty()) {
    throw common::InternalError("EvalEnumNextPrev", "enum has no members");
  }

  auto bit_width = PackedBitWidth(enum_type, *types_);

  auto current_val = EvalOperand(state, rv.operands[0]);
  if (!IsIntegral(current_val)) {
    throw common::InternalError(
        "EvalEnumNextPrev", "enum value must be integral");
  }
  const auto& current_int = AsIntegral(current_val);
  bool is_invalid = !current_int.IsKnown();

  // Get step from operands[1] (default 1 if no operand)
  size_t step = 1;
  if (rv.operands.size() > 1) {
    auto step_val = EvalOperand(state, rv.operands[1]);
    const auto& step_int = AsIntegral(step_val);
    if (!step_int.IsKnown()) {
      throw std::runtime_error("enum next/prev step cannot be X/Z");
    }
    auto lo = step_int.value.empty() ? 0ULL : step_int.value[0] & 0xFFFF'FFFFU;
    step = static_cast<size_t>(lo);
  }

  // Find current index by bitwise equality
  size_t n = members.size();
  size_t current_idx = n;  // n means "not found"
  if (!is_invalid) {
    for (size_t i = 0; i < n; ++i) {
      auto member_ri = MakeIntegralFromConstant(members[i].value, bit_width);
      auto eq = IntegralEq(current_int, AsIntegral(member_ri));
      if (eq.IsKnown() && !eq.value.empty() && eq.value[0] == 1) {
        current_idx = i;
        break;
      }
    }
  }

  // Compute result index
  size_t step_mod = step % n;
  size_t result_idx = 0;
  if (current_idx >= n) {
    result_idx = (info.method == BuiltinMethod::kEnumNext) ? 0 : n - 1;
  } else if (info.method == BuiltinMethod::kEnumNext) {
    result_idx = (current_idx + step_mod) % n;
  } else {
    result_idx = (current_idx + (n - step_mod)) % n;
  }

  return MakeIntegralFromConstant(members[result_idx].value, bit_width);
}

auto Interpreter::EvalEnumName(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  if (!info.enum_type) {
    throw common::InternalError("EvalEnumName", "name() requires enum_type");
  }
  const Type& enum_type = (*types_)[*info.enum_type];
  if (enum_type.Kind() != TypeKind::kEnum) {
    throw common::InternalError("EvalEnumName", "enum_type is not kEnum");
  }
  const auto& enum_info = enum_type.AsEnum();
  auto bit_width = PackedBitWidth(enum_type, *types_);

  auto current_val = EvalOperand(state, rv.operands[0]);
  if (!IsIntegral(current_val)) {
    return MakeString("");
  }
  const auto& current_int = AsIntegral(current_val);
  if (!current_int.IsKnown()) {
    return MakeString("");
  }

  for (const auto& member : enum_info.members) {
    auto member_ri = MakeIntegralFromConstant(member.value, bit_width);
    auto eq = IntegralEq(current_int, AsIntegral(member_ri));
    if (eq.IsKnown() && !eq.value.empty() && eq.value[0] == 1) {
      return MakeString(member.name);
    }
  }

  return MakeString("");
}

auto Interpreter::EvalBuiltinCall(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  switch (info.method) {
    case BuiltinMethod::kNewArray:
      return EvalNewArray(state, rv);

    case BuiltinMethod::kArraySize:
    case BuiltinMethod::kQueueSize: {
      if (rv.operands.empty()) {
        throw common::InternalError("EvalBuiltinCall", "size() requires array");
      }
      auto arr_val = EvalOperand(state, rv.operands[0]);
      if (!IsArray(arr_val)) {
        throw common::InternalError(
            "EvalBuiltinCall", "size() operand must be array");
      }
      auto size = static_cast<int64_t>(AsArray(arr_val).elements.size());
      return MakeIntegral(size, 32);
    }

    case BuiltinMethod::kQueuePopBack: {
      if (!info.receiver) {
        throw common::InternalError(
            "EvalBuiltinCall", "pop_back() requires receiver");
      }
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
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
            "EvalBuiltinCall", "pop_front() requires receiver");
      }
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
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
            "EvalBuiltinCall", "push_back() requires receiver");
      }
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
      elements.push_back(EvalOperand(state, rv.operands[0]));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[*info.receiver]);
      const auto& type = (*types_)[receiver_type];
      if (type.Kind() == TypeKind::kQueue) {
        TruncateToBound(elements, type.AsQueue().max_bound);
      }
      return std::monostate{};
    }

    case BuiltinMethod::kQueuePushFront: {
      if (!info.receiver) {
        throw common::InternalError(
            "EvalBuiltinCall", "push_front() requires receiver");
      }
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
      elements.insert(elements.begin(), EvalOperand(state, rv.operands[0]));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[*info.receiver]);
      const auto& type = (*types_)[receiver_type];
      if (type.Kind() == TypeKind::kQueue) {
        TruncateToBound(elements, type.AsQueue().max_bound);
      }
      return std::monostate{};
    }

    case BuiltinMethod::kQueueInsert: {
      if (!info.receiver) {
        throw common::InternalError(
            "EvalBuiltinCall", "insert() requires receiver");
      }
      TypeId idx_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
      auto idx_opt =
          TryGetIndex(EvalOperand(state, rv.operands[0]), *types_, idx_type);
      if (!idx_opt) {
        return std::monostate{};  // X/Z -> no-op
      }
      auto idx = *idx_opt;
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
      if (idx < 0 || std::cmp_greater(idx, elements.size())) {
        return std::monostate{};  // Invalid index -> no-op
      }
      elements.insert(
          elements.begin() + idx, EvalOperand(state, rv.operands[1]));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[*info.receiver]);
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
            "EvalBuiltinCall", "delete() requires receiver");
      }
      AsArray(WritePlace(state, *info.receiver)).elements.clear();
      return std::monostate{};
    }

    case BuiltinMethod::kQueueDeleteAt: {
      if (!info.receiver) {
        throw common::InternalError(
            "EvalBuiltinCall", "delete(idx) requires receiver");
      }
      TypeId idx_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
      auto idx_opt =
          TryGetIndex(EvalOperand(state, rv.operands[0]), *types_, idx_type);
      if (!idx_opt) {
        return std::monostate{};  // X/Z -> no-op
      }
      auto idx = *idx_opt;
      auto& elements = AsArray(WritePlace(state, *info.receiver)).elements;
      if (idx >= 0 && std::cmp_less(idx, elements.size())) {
        elements.erase(elements.begin() + idx);
      }
      return std::monostate{};
    }

    case BuiltinMethod::kEnumNext:
    case BuiltinMethod::kEnumPrev:
      return EvalEnumNextPrev(state, rv);

    case BuiltinMethod::kEnumName:
      return EvalEnumName(state, rv);
  }
  throw common::InternalError("EvalBuiltinCall", "unknown builtin method");
}

auto Interpreter::EvalIndexValidity(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<IndexValidityRvalueInfo>(rv.info);
  if (rv.operands.size() != 1) {
    throw common::InternalError(
        "EvalIndexValidity", "requires exactly 1 operand");
  }
  auto index = EvalOperand(state, rv.operands[0]);
  if (!IsIntegral(index)) {
    throw common::InternalError(
        "EvalIndexValidity", "operand must be integral");
  }
  const auto& idx_int = AsIntegral(index);

  bool is_known = idx_int.IsKnown();
  if (info.check_known && !is_known) {
    return MakeIntegral(0, 1);  // Invalid: X/Z index
  }

  TypeId index_type = TypeOfOperand(rv.operands[0], *arena_, *types_);
  bool is_signed = IsSignedType(*types_, index_type);

  // Special case: unsigned index with negative bounds.
  if (!is_signed) {
    if (info.lower_bound < 0) {
      if (info.upper_bound < 0) {
        return MakeIntegral(0, 1);  // Always invalid
      }
      auto upper_int = MakeIntegral(
          static_cast<uint64_t>(info.upper_bound), idx_int.bit_width);
      const auto& upper_val = AsIntegral(upper_int);
      return IntegralLe(idx_int, upper_val, false);
    }
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
  auto lower_int = MakeIntegralSigned(info.lower_bound, idx_int.bit_width);
  auto upper_int = MakeIntegralSigned(info.upper_bound, idx_int.bit_width);
  const auto& lower_val = AsIntegral(lower_int);
  const auto& upper_val = AsIntegral(upper_int);
  auto ge_lower = IntegralGe(idx_int, lower_val, true);
  auto le_upper = IntegralLe(idx_int, upper_val, true);
  return IntegralLogicalAnd(ge_lower, le_upper);
}

auto Interpreter::EvalConcat(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<ConcatRvalueInfo>(rv.info);
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
            "EvalConcat", "string concat operand must be string or integral");
      }
    }
    return MakeString(std::move(result));
  }

  // Integral concatenation (bit-based)
  uint32_t result_width = PackedBitWidth(result_type, *types_);

  std::vector<RuntimeIntegral> values;
  values.reserve(rv.operands.size());
  uint32_t total_width = 0;
  for (const auto& operand : rv.operands) {
    auto val = EvalOperand(state, operand);
    RuntimeIntegral integral;
    if (IsString(val)) {
      TypeId op_type = TypeOfOperand(operand, *arena_, *types_);
      const Type& op_type_ref = (*types_)[op_type];
      uint32_t op_width = 0;
      if (IsPacked(op_type_ref)) {
        op_width = PackedBitWidth(op_type_ref, *types_);
      } else {
        op_width = static_cast<uint32_t>(AsString(val).value.size()) * 8;
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
        "EvalConcat", std::format(
                          "concat operand widths {} != result width {}",
                          total_width, result_width));
  }

  // Build result from MSB to LSB using 4-state insert
  auto result = MakeKnownIntegral(result_width);
  uint32_t bit_pos = result_width;
  for (const auto& val : values) {
    bit_pos -= val.bit_width;
    result = IntegralInsertSlice4State(result, val, bit_pos, val.bit_width);
  }
  return result;
}

auto Interpreter::EvalSFormat(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<SFormatRvalueInfo>(rv.info);
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
    // Runtime format path
    RuntimeValue fmt_val = EvalOperand(state, rv.operands[0]);
    std::string fmt_str = AsString(fmt_val).value;
    std::vector<TypedValue> value_args;
    for (size_t i = 1; i < rv.operands.size(); ++i) {
      TypeId type = TypeOfOperand(rv.operands[i], *arena_, *types_);
      value_args.push_back({EvalOperand(state, rv.operands[i]), type});
    }
    char fmt_char = FormatKindToSpecChar(info.default_format);
    result = FormatDisplay(fmt_str, value_args, fmt_char, *types_, ctx);
  } else {
    // Auto-format path
    std::vector<TypedValue> typed_args;
    for (const auto& operand : rv.operands) {
      TypeId type = TypeOfOperand(operand, *arena_, *types_);
      typed_args.push_back({EvalOperand(state, operand), type});
    }
    char fmt_char = FormatKindToSpecChar(info.default_format);
    result = FormatMessage(typed_args, fmt_char, *types_, ctx);
  }

  return MakeString(std::move(result));
}

auto Interpreter::EvalPlusargs(ProcessState& state, const Rvalue& rv)
    -> RuntimeValue {
  const auto& info = std::get<PlusargsRvalueInfo>(rv.info);

  // Evaluate query/format operand (always a string)
  RuntimeValue query_val = EvalOperand(state, rv.operands[0]);
  if (!IsString(query_val)) {
    throw common::InternalError(
        "EvalPlusargs", "query operand is not a string");
  }
  std::string_view query = AsString(query_val).value;

  // Helper: strip '+' prefix from a plusarg
  auto get_content = [](std::string_view arg) -> std::string_view {
    if (arg.starts_with('+')) {
      return arg.substr(1);
    }
    return arg;
  };

  if (info.kind == PlusargsKind::kTest) {
    // $test$plusargs: prefix match
    for (const auto& arg : plusargs_) {
      std::string_view content = get_content(arg);
      if (content.starts_with(query)) {
        return MakeIntegralSigned(1, 32);
      }
    }
    return MakeIntegralSigned(0, 32);
  }

  // $value$plusargs: parse format and extract value
  // Parse format string: "PREFIX%<spec>" → prefix + spec char
  auto percent_pos = query.find('%');
  if (percent_pos == std::string_view::npos) {
    // No format specifier, treat as test
    for (const auto& arg : plusargs_) {
      std::string_view content = get_content(arg);
      if (content.starts_with(query)) {
        return MakeIntegralSigned(1, 32);
      }
    }
    return MakeIntegralSigned(0, 32);
  }

  std::string_view prefix = query.substr(0, percent_pos);
  char spec = '\0';
  size_t spec_pos = percent_pos + 1;
  if (spec_pos < query.size()) {
    spec = query[spec_pos];
    // Skip leading '0' in format specifier (e.g., %0d → %d)
    if (spec == '0' && spec_pos + 1 < query.size()) {
      spec = query[spec_pos + 1];
    }
  }

  for (const auto& arg : plusargs_) {
    std::string_view content = get_content(arg);
    if (!content.starts_with(prefix)) {
      continue;
    }
    std::string_view remainder = content.substr(prefix.size());

    // Parse based on format specifier
    if (spec == 'd' || spec == 'D') {
      int32_t parsed_value = 0;
      auto [ptr, ec] = std::from_chars(
          remainder.data(), remainder.data() + remainder.size(), parsed_value);
      if (ec != std::errc{}) {
        parsed_value = 0;  // Conversion failed
      }
      if (info.output.has_value()) {
        StoreToPlace(state, *info.output, MakeIntegralSigned(parsed_value, 32));
      }
      return MakeIntegralSigned(1, 32);
    }
    if (spec == 's' || spec == 'S') {
      if (info.output.has_value()) {
        StoreToPlace(state, *info.output, MakeString(std::string(remainder)));
      }
      return MakeIntegralSigned(1, 32);
    }

    // Unsupported format spec - match but don't write
    return MakeIntegralSigned(1, 32);
  }

  // No match found
  return MakeIntegralSigned(0, 32);
}

}  // namespace lyra::mir::interp
