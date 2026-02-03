#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
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
#include "lyra/mir/rvalue.hpp"

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
    -> Result<RuntimeValue> {
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
    ProcessState& state, const Rvalue& rv, TypeId result_type)
    -> Result<RuntimeValue> {
  return std::visit(
      common::Overloaded{
          [&](const UnaryRvalueInfo& info) -> Result<RuntimeValue> {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "unary operation requires exactly 1 operand");
            }
            auto operand_result = EvalOperand(state, rv.operands[0]);
            if (!operand_result) {
              return std::unexpected(std::move(operand_result).error());
            }
            return EvalUnary(info.op, *operand_result, result_type, *types_);
          },
          [&](const BinaryRvalueInfo& info) -> Result<RuntimeValue> {
            if (rv.operands.size() != 2) {
              throw common::InternalError(
                  "EvalRvalue", "binary operation requires exactly 2 operands");
            }
            auto lhs_result = EvalOperand(state, rv.operands[0]);
            if (!lhs_result) {
              return std::unexpected(std::move(lhs_result).error());
            }
            auto rhs_result = EvalOperand(state, rv.operands[1]);
            if (!rhs_result) {
              return std::unexpected(std::move(rhs_result).error());
            }
            return EvalBinary(info.op, *lhs_result, *rhs_result);
          },
          [&](const CastRvalueInfo& info) -> Result<RuntimeValue> {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "cast operation requires exactly 1 operand");
            }
            auto operand_result = EvalOperand(state, rv.operands[0]);
            if (!operand_result) {
              return std::unexpected(std::move(operand_result).error());
            }
            const auto& src_type = (*types_)[info.source_type];
            const auto& tgt_type = (*types_)[info.target_type];
            return EvalCast(*operand_result, src_type, tgt_type, *types_);
          },
          [&](const BitCastRvalueInfo& info) -> Result<RuntimeValue> {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "bitcast operation requires exactly 1 operand");
            }
            auto operand_result = EvalOperand(state, rv.operands[0]);
            if (!operand_result) {
              return std::unexpected(std::move(operand_result).error());
            }
            return EvalBitCast(
                *operand_result, info.source_type, info.target_type, *types_);
          },
          [&](const AggregateRvalueInfo&) -> Result<RuntimeValue> {
            return EvalAggregate(state, rv);
          },
          [&](const BuiltinCallRvalueInfo&) -> Result<RuntimeValue> {
            return EvalBuiltinCall(state, rv);
          },
          [&](const IndexValidityRvalueInfo&) -> Result<RuntimeValue> {
            return EvalIndexValidity(state, rv);
          },
          [&](const GuardedUseRvalueInfo& info) -> Result<RuntimeValue> {
            if (rv.operands.size() != 1) {
              throw common::InternalError(
                  "EvalRvalue", "guarded_use requires exactly 1 operand");
            }
            auto validity_result = EvalOperand(state, rv.operands[0]);
            if (!validity_result) {
              return std::unexpected(std::move(validity_result).error());
            }
            auto& validity = *validity_result;
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
          [&](const ConcatRvalueInfo&) -> Result<RuntimeValue> {
            return EvalConcat(state, rv);
          },
          [&](const ReplicateRvalueInfo&) -> Result<RuntimeValue> {
            return EvalReplicate(state, rv);
          },
          [&](const SFormatRvalueInfo&) -> Result<RuntimeValue> {
            return EvalSFormat(state, rv);
          },
          [&](const TestPlusargsRvalueInfo& info) -> Result<RuntimeValue> {
            return EvalTestPlusargs(state, rv, info);
          },
          [&](const FopenRvalueInfo& info) -> Result<RuntimeValue> {
            return EvalFopen(state, info);
          },
          [&](const SystemTfRvalueInfo& info) -> Result<RuntimeValue> {
            switch (info.opcode) {
              case SystemTfOpcode::kFopen:
                throw common::InternalError(
                    "EvalRvalue:SystemTf",
                    "$fopen should use FopenRvalueInfo, not "
                    "SystemTfRvalueInfo");
              case SystemTfOpcode::kFclose:
                throw common::InternalError(
                    "EvalRvalue:SystemTf", "$fclose is an effect, not rvalue");
              case SystemTfOpcode::kFflush:
                throw common::InternalError(
                    "EvalRvalue:SystemTf", "$fflush is an effect, not rvalue");
              case SystemTfOpcode::kValuePlusargs:
                throw common::InternalError(
                    "EvalRvalue:SystemTf",
                    "$value$plusargs should be lowered via Call, not "
                    "SystemTfRvalueInfo");
              case SystemTfOpcode::kRandom: {
                // LCG with glibc constants: a=1103515245, c=12345, m=2^31
                constexpr uint32_t kMultiplier = 1103515245;
                constexpr uint32_t kIncrement = 12345;
                prng_state_ = prng_state_ * kMultiplier + kIncrement;
                return MakeIntegralSigned(
                    static_cast<int32_t>(prng_state_), 32);
              }
              case SystemTfOpcode::kUrandom: {
                constexpr uint32_t kMultiplier = 1103515245;
                constexpr uint32_t kIncrement = 12345;
                prng_state_ = prng_state_ * kMultiplier + kIncrement;
                return MakeIntegral(prng_state_, 32);
              }
            }
            throw common::InternalError(
                "EvalRvalue:SystemTf", "unhandled SystemTfOpcode");
          },
          [&](const RuntimeQueryRvalueInfo&) -> Result<RuntimeValue> {
            // Runtime queries (e.g., $time) require the LLVM backend runtime.
            // Note: EvalRvalue doesn't have access to instruction origin,
            // so we use UnknownSpan here.
            return std::unexpected(
                Diagnostic{
                    .primary =
                        {.kind = DiagKind::kUnsupported,
                         .span = UnknownSpan{},
                         .message = std::format(
                             "rvalue kind '{}' requires runtime backend",
                             GetRvalueKind(rv.info)),
                         .category = UnsupportedCategory::kFeature},
                    .notes = {},
                });
          },
          [&](const MathCallRvalueInfo& info) -> Result<RuntimeValue> {
            return EvalMathCall(state, rv, info);
          },
          [&](const ArrayQueryRvalueInfo& info) -> Result<RuntimeValue> {
            return EvalArrayQuery(state, rv, info);
          },
          [&](const SystemCmdRvalueInfo& info) -> Result<RuntimeValue> {
            return EvalSystemCmd(state, info);
          },
      },
      rv.info);
}

auto Interpreter::EvalAggregate(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
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
      auto field_result = EvalOperand(state, operand);
      if (!field_result) {
        return std::unexpected(std::move(field_result).error());
      }
      fields.push_back(std::move(*field_result));
    }
    return MakeStruct(std::move(fields));
  }

  if (type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    std::vector<RuntimeValue> elements;
    elements.reserve(rv.operands.size());
    for (const auto& operand : rv.operands) {
      auto elem_result = EvalOperand(state, operand);
      if (!elem_result) {
        return std::unexpected(std::move(elem_result).error());
      }
      elements.push_back(std::move(*elem_result));
    }
    if (type.Kind() == TypeKind::kQueue) {
      TruncateToBound(elements, type.AsQueue().max_bound);
    }
    return MakeArray(std::move(elements));
  }

  throw common::InternalError("EvalAggregate", "unsupported type");
}

auto Interpreter::EvalNewArray(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  if (rv.operands.empty()) {
    throw common::InternalError("EvalNewArray", "new[] requires size");
  }
  auto size_val_result = EvalOperand(state, rv.operands[0]);
  if (!size_val_result) {
    return std::unexpected(std::move(size_val_result).error());
  }
  auto& size_val = *size_val_result;
  if (!IsIntegral(size_val)) {
    throw common::InternalError("EvalNewArray", "new[] size must be integral");
  }
  const auto& size_int = AsIntegral(size_val);
  if (!size_int.IsKnown()) {
    // TODO(hankhsu): SV semantics - should return X, not throw
    return std::unexpected(Diagnostic::HostError("new[] size is X/Z"));
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
    // TODO(hankhsu): SV semantics - should return X, not throw
    return std::unexpected(
        Diagnostic::HostError("new[] size cannot be negative"));
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
    auto init_val_result = EvalOperand(state, rv.operands[1]);
    if (!init_val_result) {
      return std::unexpected(std::move(init_val_result).error());
    }
    auto& init_val = *init_val_result;
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
    -> Result<RuntimeValue> {
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

  auto current_val_result = EvalOperand(state, rv.operands[0]);
  if (!current_val_result) {
    return std::unexpected(std::move(current_val_result).error());
  }
  auto& current_val = *current_val_result;
  if (!IsIntegral(current_val)) {
    throw common::InternalError(
        "EvalEnumNextPrev", "enum value must be integral");
  }
  const auto& current_int = AsIntegral(current_val);
  bool is_invalid = !current_int.IsKnown();

  // Get step from operands[1] (default 1 if no operand)
  size_t step = 1;
  if (rv.operands.size() > 1) {
    auto step_val_result = EvalOperand(state, rv.operands[1]);
    if (!step_val_result) {
      return std::unexpected(std::move(step_val_result).error());
    }
    const auto& step_int = AsIntegral(*step_val_result);
    if (!step_int.IsKnown()) {
      // TODO(hankhsu): SV semantics - should return X, not throw
      return std::unexpected(
          Diagnostic::HostError("enum next/prev step cannot be X/Z"));
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
      auto eq = IntegralEqual(current_int, AsIntegral(member_ri));
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
    -> Result<RuntimeValue> {
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

  auto current_val_result = EvalOperand(state, rv.operands[0]);
  if (!current_val_result) {
    return std::unexpected(std::move(current_val_result).error());
  }
  auto& current_val = *current_val_result;
  if (!IsIntegral(current_val)) {
    return MakeString("");
  }
  const auto& current_int = AsIntegral(current_val);
  if (!current_int.IsKnown()) {
    return MakeString("");
  }

  for (const auto& member : enum_info.members) {
    auto member_ri = MakeIntegralFromConstant(member.value, bit_width);
    auto eq = IntegralEqual(current_int, AsIntegral(member_ri));
    if (eq.IsKnown() && !eq.value.empty() && eq.value[0] == 1) {
      return MakeString(member.name);
    }
  }

  return MakeString("");
}

auto Interpreter::EvalBuiltinCall(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
  const auto& info = std::get<BuiltinCallRvalueInfo>(rv.info);
  switch (info.method) {
    case BuiltinMethod::kNewArray:
      return EvalNewArray(state, rv);

    case BuiltinMethod::kArraySize:
    case BuiltinMethod::kQueueSize: {
      if (rv.operands.empty()) {
        throw common::InternalError("EvalBuiltinCall", "size() requires array");
      }
      auto arr_val_result = EvalOperand(state, rv.operands[0]);
      if (!arr_val_result) {
        return std::unexpected(std::move(arr_val_result).error());
      }
      if (!IsArray(*arr_val_result)) {
        throw common::InternalError(
            "EvalBuiltinCall", "size() operand must be array");
      }
      auto size =
          static_cast<int64_t>(AsArray(*arr_val_result).elements.size());
      return MakeIntegral(size, 32);
    }

    case BuiltinMethod::kQueuePopBack:
    case BuiltinMethod::kQueuePopFront:
    case BuiltinMethod::kQueuePushBack:
    case BuiltinMethod::kQueuePushFront:
    case BuiltinMethod::kQueueInsert:
    case BuiltinMethod::kArrayDelete:
    case BuiltinMethod::kQueueDelete:
    case BuiltinMethod::kQueueDeleteAt:
      // Container-mutating builtins are now handled via BuiltinCall instruction
      throw common::InternalError(
          "EvalBuiltinCall",
          "container-mutating builtins must use BuiltinCall instruction");

    case BuiltinMethod::kEnumNext:
    case BuiltinMethod::kEnumPrev:
      return EvalEnumNextPrev(state, rv);

    case BuiltinMethod::kEnumName:
      return EvalEnumName(state, rv);
  }
  throw common::InternalError("EvalBuiltinCall", "unknown builtin method");
}

auto Interpreter::EvalIndexValidity(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
  const auto& info = std::get<IndexValidityRvalueInfo>(rv.info);
  if (rv.operands.size() != 1) {
    throw common::InternalError(
        "EvalIndexValidity", "requires exactly 1 operand");
  }
  auto index_result = EvalOperand(state, rv.operands[0]);
  if (!index_result) {
    return std::unexpected(std::move(index_result).error());
  }
  auto& index = *index_result;
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
    -> Result<RuntimeValue> {
  const auto& info = std::get<ConcatRvalueInfo>(rv.info);
  const Type& result_type = (*types_)[info.result_type];

  // String concatenation (byte-based)
  if (result_type.Kind() == TypeKind::kString) {
    std::string result;
    for (const auto& operand : rv.operands) {
      auto val_result = EvalOperand(state, operand);
      if (!val_result) {
        return std::unexpected(std::move(val_result).error());
      }
      result += CoerceToString(*val_result, "EvalConcat");
    }
    return MakeString(std::move(result));
  }

  // Integral concatenation (bit-based)
  uint32_t result_width = PackedBitWidth(result_type, *types_);

  std::vector<RuntimeIntegral> values;
  values.reserve(rv.operands.size());
  uint32_t total_width = 0;
  for (const auto& operand : rv.operands) {
    auto val_result = EvalOperand(state, operand);
    if (!val_result) {
      return std::unexpected(std::move(val_result).error());
    }
    auto& val = *val_result;
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

auto Interpreter::EvalReplicate(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
  const auto& info = std::get<ReplicateRvalueInfo>(rv.info);
  const Type& result_type = (*types_)[info.result_type];

  if (rv.operands.size() != 1) {
    throw common::InternalError(
        "EvalReplicate", "replicate requires exactly 1 operand");
  }

  auto elem_result = EvalOperand(state, rv.operands[0]);
  if (!elem_result) {
    return std::unexpected(std::move(elem_result).error());
  }

  // String replication: concatenate string N times
  if (result_type.Kind() == TypeKind::kString) {
    if (!IsString(*elem_result)) {
      throw common::InternalError(
          "EvalReplicate", "string result type but operand is not string");
    }
    const std::string& elem_str = AsString(*elem_result).value;
    std::string result;
    result.reserve(elem_str.size() * info.count);
    for (uint32_t i = 0; i < info.count; ++i) {
      result += elem_str;
    }
    return MakeString(std::move(result));
  }

  // Packed replication: bit-replicate element N times
  uint32_t result_width = PackedBitWidth(result_type, *types_);
  const auto& elem = AsIntegral(*elem_result);
  uint32_t elem_width = elem.bit_width;

  // Build result from MSB to LSB: insert element at (count - 1 - i) *
  // elem_width
  auto result = MakeKnownIntegral(result_width);
  for (uint32_t i = 0; i < info.count; ++i) {
    uint32_t bit_pos = (info.count - 1 - i) * elem_width;
    result = IntegralInsertSlice4State(result, elem, bit_pos, elem_width);
  }
  return result;
}

auto Interpreter::EvalSFormat(ProcessState& state, const Rvalue& rv)
    -> Result<RuntimeValue> {
  const auto& info = std::get<SFormatRvalueInfo>(rv.info);
  FormatContext ctx{};
  std::string result;

  if (!info.ops.empty()) {
    // Compile-time path: iterate ops like ExecDisplayEffect
    for (const auto& op : info.ops) {
      if (op.kind == FormatKind::kLiteral) {
        result += op.literal;
      } else {
        auto value_result = EvalOperand(state, *op.value);
        if (!value_result) {
          return std::unexpected(std::move(value_result).error());
        }
        TypedValue typed{.value = std::move(*value_result), .type = op.type};
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
    auto fmt_val_result = EvalOperand(state, rv.operands[0]);
    if (!fmt_val_result) {
      return std::unexpected(std::move(fmt_val_result).error());
    }
    std::string fmt_str = AsString(*fmt_val_result).value;
    std::vector<TypedValue> value_args;
    for (size_t i = 1; i < rv.operands.size(); ++i) {
      TypeId type = TypeOfOperand(rv.operands[i], *arena_, *types_);
      auto arg_result = EvalOperand(state, rv.operands[i]);
      if (!arg_result) {
        return std::unexpected(std::move(arg_result).error());
      }
      value_args.push_back({std::move(*arg_result), type});
    }
    char fmt_char = FormatKindToSpecChar(info.default_format);
    result = FormatDisplay(fmt_str, value_args, fmt_char, *types_, ctx);
  } else {
    // Auto-format path
    std::vector<TypedValue> typed_args;
    for (const auto& operand : rv.operands) {
      TypeId type = TypeOfOperand(operand, *arena_, *types_);
      auto arg_result = EvalOperand(state, operand);
      if (!arg_result) {
        return std::unexpected(std::move(arg_result).error());
      }
      typed_args.push_back({std::move(*arg_result), type});
    }
    char fmt_char = FormatKindToSpecChar(info.default_format);
    result = FormatMessage(typed_args, fmt_char, *types_, ctx);
  }

  return MakeString(std::move(result));
}

auto Interpreter::EvalTestPlusargs(
    ProcessState& state, [[maybe_unused]] const Rvalue& rv,
    const TestPlusargsRvalueInfo& info) -> Result<RuntimeValue> {
  // Evaluate query operand and coerce to string if packed
  auto query_val_result = EvalOperand(state, info.query.operand);
  if (!query_val_result) {
    return std::unexpected(std::move(query_val_result).error());
  }
  std::string query_str = CoerceToString(*query_val_result, "EvalTestPlusargs");
  std::string_view query = query_str;

  // Helper: strip '+' prefix from a plusarg
  auto get_content = [](std::string_view arg) -> std::string_view {
    if (arg.starts_with('+')) {
      return arg.substr(1);
    }
    return arg;
  };

  // $test$plusargs: prefix match
  for (const auto& arg : plusargs_) {
    std::string_view content = get_content(arg);
    if (content.starts_with(query)) {
      return MakeIntegralSigned(1, 32);
    }
  }
  return MakeIntegralSigned(0, 32);
}

auto Interpreter::EvalFopen(ProcessState& state, const FopenRvalueInfo& info)
    -> Result<RuntimeValue> {
  // Evaluate filename and coerce to string if packed
  auto filename_val_result = EvalOperand(state, info.filename.operand);
  if (!filename_val_result) {
    return std::unexpected(std::move(filename_val_result).error());
  }
  std::string filename = CoerceToString(*filename_val_result, "EvalFopen");

  int32_t result = 0;
  if (info.mode) {
    // FD mode: $fopen(filename, mode)
    auto mode_val_result = EvalOperand(state, info.mode->operand);
    if (!mode_val_result) {
      return std::unexpected(std::move(mode_val_result).error());
    }
    std::string mode = CoerceToString(*mode_val_result, "EvalFopen");
    result = file_manager_.FopenFd(filename, mode);
  } else {
    // MCD mode: $fopen(filename)
    result = file_manager_.FopenMcd(filename);
  }

  return MakeIntegralSigned(result, 32);
}

auto Interpreter::EvalMathCall(
    ProcessState& state, const Rvalue& rv, const MathCallRvalueInfo& info)
    -> Result<RuntimeValue> {
  // Evaluate all operands
  std::vector<RuntimeValue> args;
  args.reserve(rv.operands.size());
  for (const auto& operand : rv.operands) {
    auto arg_result = EvalOperand(state, operand);
    if (!arg_result) {
      return std::unexpected(std::move(arg_result).error());
    }
    args.push_back(std::move(*arg_result));
  }

  // Delegate to the single math call entry point (handles arity validation)
  return mir::interp::EvalMathCall(info.fn, args, *types_);
}

auto Interpreter::EvalArrayQuery(
    ProcessState& state, const Rvalue& rv, const ArrayQueryRvalueInfo& info)
    -> Result<RuntimeValue> {
  using lyra::ArrayQuerySysFnKind;

  // 1. Evaluate dim operand
  auto dim_val = EvalOperand(state, rv.operands[1]);
  if (!dim_val) return std::unexpected(std::move(dim_val).error());

  if (!IsIntegral(*dim_val)) {
    throw common::InternalError(
        "EvalArrayQuery", "dimension operand must be integral");
  }
  const auto& dim_int = AsIntegral(*dim_val);

  // If dim has unknown bits, return 'x
  if (!dim_int.IsKnown()) {
    return MakeIntegralX(32);
  }

  // Get dim as signed 32-bit integer
  int32_t dim = 1;
  if (!dim_int.value.empty()) {
    auto raw = static_cast<int64_t>(dim_int.value[0]);
    // Handle sign extension for negative values
    if (dim_int.bit_width <= 32) {
      auto mask = static_cast<int32_t>((1LL << dim_int.bit_width) - 1);
      dim = static_cast<int32_t>(raw & mask);
      // Sign-extend if MSB is set
      if ((dim & (1 << (dim_int.bit_width - 1))) != 0) {
        dim |= ~mask;
      }
    } else {
      dim = static_cast<int32_t>(raw);
    }
  }

  // 2. Validate dim range (out of range -> 'x, not error)
  if (dim < 1 || dim > static_cast<int32_t>(info.total_dims)) {
    return MakeIntegralX(32);
  }

  // 3. Get dimension metadata (0-indexed)
  const auto& dim_info = info.dims.at(static_cast<size_t>(dim - 1));

  // Note: IEEE 20.7.1 (dim > 1 on variable-sized dimension for array variable)
  // is enforced at compile-time for constant dim on variable-rooted lvalues.
  // For non-constant dim at runtime, we cannot distinguish variable-rooted
  // lvalues from other expressions, but the spec only requires an error for
  // the variable case. For robustness, we return 'x for any dim > 1 on a
  // variable-sized dimension, which is a safe conservative behavior.
  if (dim > 1 && dim_info.is_variable_sized) {
    return MakeIntegralX(32);
  }

  // 5. Get left/right (possibly from runtime size for variable dims)
  int32_t left = dim_info.left;
  int32_t right = dim_info.right;

  if (dim_info.is_variable_sized) {
    // Evaluate array operand to get runtime size
    auto array_val = EvalOperand(state, rv.operands[0]);
    if (!array_val) return std::unexpected(std::move(array_val).error());

    // Get element count (works for RuntimeArray from dynamic array and queue)
    // Note: String dimension queries should be folded to 'x at compile time,
    // so we don't handle IsString here.
    int32_t size = 0;
    if (IsArray(*array_val)) {
      size = static_cast<int32_t>(AsArray(*array_val).elements.size());
    }

    // Variable-sized: left=0, right=size-1 (or -1 if empty)
    left = 0;
    right = (size == 0) ? -1 : size - 1;
  }

  // 6. Compute increment, low, high, size using correct semantics
  // increment = -1 for variable-sized, otherwise (left >= right) ? 1 : -1
  int32_t increment = -1;
  if (!dim_info.is_variable_sized) {
    increment = (left >= right) ? 1 : -1;
  }

  // low = (increment == -1) ? left : right
  // high = (increment == -1) ? right : left
  int32_t low = (increment == -1) ? left : right;
  int32_t high = (increment == -1) ? right : left;
  int32_t computed_size = high - low + 1;

  // 7. Return based on query kind
  switch (info.kind) {
    case ArrayQuerySysFnKind::kLeft:
      return MakeIntegralSigned(left, 32);
    case ArrayQuerySysFnKind::kRight:
      return MakeIntegralSigned(right, 32);
    case ArrayQuerySysFnKind::kLow:
      return MakeIntegralSigned(low, 32);
    case ArrayQuerySysFnKind::kHigh:
      return MakeIntegralSigned(high, 32);
    case ArrayQuerySysFnKind::kIncrement:
      return MakeIntegralSigned(increment, 32);
    case ArrayQuerySysFnKind::kSize:
      return MakeIntegralSigned(computed_size, 32);
    case ArrayQuerySysFnKind::kDimensions:
      // Should be folded at lowering, but handle for robustness
      return MakeIntegralSigned(info.total_dims, 32);
    case ArrayQuerySysFnKind::kUnpackedDimensions:
      // Should be folded at lowering, but handle for robustness
      return MakeIntegralSigned(info.unpacked_dims, 32);
  }

  throw common::InternalError("EvalArrayQuery", "unknown array query kind");
}

auto Interpreter::EvalSystemCmd(
    ProcessState& state, const SystemCmdRvalueInfo& info)
    -> Result<RuntimeValue> {
  // Security gate: check enable_system_ before executing
  if (!enable_system_) {
    // Disabled: return -1 deterministically (safe for CI/sandbox)
    return MakeIntegralSigned(-1, 32);
  }

  if (!info.command) {
    // No-arg form: check shell availability
    // NOLINTNEXTLINE(cert-env33-c) - $system is an intentional shell interface
    int result = std::system(nullptr);
    return MakeIntegralSigned(static_cast<int32_t>(result), 32);
  }

  // Evaluate command and coerce to string (handles packed arrays -> string)
  auto cmd_val_result = EvalOperand(state, info.command->operand);
  if (!cmd_val_result) {
    return std::unexpected(std::move(cmd_val_result).error());
  }
  std::string command = CoerceToString(*cmd_val_result, "EvalSystemCmd");

  // Execute and return raw result truncated to int32 (host-dependent semantics)
  // NOLINTNEXTLINE(cert-env33-c) - $system is an intentional shell interface
  int result = std::system(command.c_str());
  return MakeIntegralSigned(static_cast<int32_t>(result), 32);
}

}  // namespace lyra::mir::interp
