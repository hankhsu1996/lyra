#include "lyra/mir/interp/blob_codec.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

namespace {

// Check if a slice of an integral has any X/Z bits set.
auto SliceHasXZ(
    const RuntimeIntegral& storage, uint32_t bit_offset, uint32_t width)
    -> bool {
  RuntimeIntegral slice = IntegralExtractSlice(storage, bit_offset, width);
  return !slice.IsKnown();
}

}  // namespace

auto LoadFromBlob(
    TypeId type_id, const RuntimeIntegral& storage, uint32_t bit_offset,
    const TypeArena& types) -> Result<RuntimeValue> {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kReal: {
      if (SliceHasXZ(storage, bit_offset, 64)) {
        return std::unexpected(
            Diagnostic::HostError("reading real from X/Z-containing storage"));
      }
      RuntimeIntegral bits = IntegralExtractSlice(storage, bit_offset, 64);
      uint64_t raw = bits.value.empty() ? 0 : bits.value[0];
      return MakeReal(std::bit_cast<double>(raw));
    }
    case TypeKind::kShortReal: {
      if (SliceHasXZ(storage, bit_offset, 32)) {
        return std::unexpected(
            Diagnostic::HostError(
                "reading shortreal from X/Z-containing storage"));
      }
      RuntimeIntegral bits = IntegralExtractSlice(storage, bit_offset, 32);
      uint64_t raw = bits.value.empty() ? 0 : bits.value[0];
      return MakeShortReal(
          std::bit_cast<float>(static_cast<uint32_t>(raw & 0xFFFFFFFF)));
    }
    case TypeKind::kIntegral: {
      uint32_t width = type.AsIntegral().bit_width;
      return IntegralExtractSlice(storage, bit_offset, width);
    }
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct: {
      uint32_t width = PackedBitWidth(type, types);
      return IntegralExtractSlice(storage, bit_offset, width);
    }
    case TypeKind::kEnum: {
      uint32_t width = PackedBitWidth(type, types);
      return IntegralExtractSlice(storage, bit_offset, width);
    }
    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      Layout layout = LayoutOf(type_id, types);
      std::vector<RuntimeValue> fields;
      fields.reserve(info.fields.size());
      for (const auto& fl : layout.fields) {
        auto field_result =
            LoadFromBlob(fl.type, storage, bit_offset + fl.bit_offset, types);
        if (!field_result) {
          return std::unexpected(std::move(field_result).error());
        }
        fields.push_back(std::move(*field_result));
      }
      return MakeStruct(std::move(fields));
    }
    case TypeKind::kUnpackedArray: {
      Layout layout = LayoutOf(type_id, types);
      const auto& info = type.AsUnpackedArray();
      auto count = static_cast<size_t>(info.range.Size());
      std::vector<RuntimeValue> elements;
      elements.reserve(count);
      for (size_t i = 0; i < count; ++i) {
        uint32_t elem_offset = bit_offset + (static_cast<uint32_t>(i) *
                                             *layout.element_stride_bits);
        auto elem_result =
            LoadFromBlob(*layout.element_type, storage, elem_offset, types);
        if (!elem_result) {
          return std::unexpected(std::move(elem_result).error());
        }
        elements.push_back(std::move(*elem_result));
      }
      return MakeArray(std::move(elements));
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      uint32_t width = info.storage_bit_width;
      RuntimeIntegral union_storage =
          IntegralExtractSlice(storage, bit_offset, width);
      return MakeUnion(std::move(union_storage));
    }
    default:
      throw common::InternalError("LoadFromBlob", "unsupported type kind");
  }
}

void StoreToBlob(
    TypeId type_id, const RuntimeValue& val, RuntimeIntegral& storage,
    uint32_t bit_offset, const TypeArena& types) {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kReal: {
      if (!IsReal(val)) {
        throw common::InternalError("StoreToBlob", "expected real value");
      }
      auto bits = std::bit_cast<uint64_t>(AsReal(val).value);
      RuntimeIntegral val_bits;
      val_bits.bit_width = 64;
      val_bits.value = {bits};
      val_bits.unknown = {0};
      storage = IntegralInsertSlice4State(storage, val_bits, bit_offset, 64);
      break;
    }
    case TypeKind::kShortReal: {
      if (!IsShortReal(val)) {
        throw common::InternalError("StoreToBlob", "expected shortreal value");
      }
      auto bits = std::bit_cast<uint32_t>(AsShortReal(val).value);
      RuntimeIntegral val_bits;
      val_bits.bit_width = 32;
      val_bits.value = {bits};
      val_bits.unknown = {0};
      storage = IntegralInsertSlice4State(storage, val_bits, bit_offset, 32);
      break;
    }
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      if (!IsIntegral(val)) {
        throw common::InternalError("StoreToBlob", "expected integral value");
      }
      uint32_t width = BlobBitSize(type_id, types);
      RuntimeIntegral src = AsIntegral(val);
      // 2-state canonicalization: force-clear unknown bits
      if (!IsFourStateType(type_id, types)) {
        std::ranges::fill(src.unknown, 0ULL);
      }
      storage = IntegralInsertSlice4State(storage, src, bit_offset, width);
      break;
    }
    case TypeKind::kUnpackedStruct: {
      if (!IsStruct(val)) {
        throw common::InternalError("StoreToBlob", "expected struct value");
      }
      Layout layout = LayoutOf(type_id, types);
      const auto& s = AsStruct(val);
      for (size_t i = 0; i < layout.fields.size(); ++i) {
        const auto& fl = layout.fields[i];
        StoreToBlob(
            fl.type, s.fields[i], storage, bit_offset + fl.bit_offset, types);
      }
      break;
    }
    case TypeKind::kUnpackedArray: {
      if (!IsArray(val)) {
        throw common::InternalError("StoreToBlob", "expected array value");
      }
      Layout layout = LayoutOf(type_id, types);
      const auto& arr = AsArray(val);
      for (size_t i = 0; i < arr.elements.size(); ++i) {
        uint32_t elem_offset = bit_offset + (static_cast<uint32_t>(i) *
                                             *layout.element_stride_bits);
        StoreToBlob(
            *layout.element_type, arr.elements[i], storage, elem_offset, types);
      }
      break;
    }
    case TypeKind::kUnpackedUnion: {
      if (!IsUnion(val)) {
        throw common::InternalError("StoreToBlob", "expected union value");
      }
      uint32_t width = BlobBitSize(type_id, types);
      const auto& union_storage = AsUnion(val).storage_bits;
      storage =
          IntegralInsertSlice4State(storage, union_storage, bit_offset, width);
      break;
    }
    default:
      throw common::InternalError("StoreToBlob", "unsupported type kind");
  }
}

auto ValueContainsXZ(
    const RuntimeValue& val, TypeId type_id, const TypeArena& types) -> bool {
  const auto& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kReal:
    case TypeKind::kShortReal:
      return false;
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      if (!IsIntegral(val)) {
        return false;
      }
      return !AsIntegral(val).IsKnown();
    }
    case TypeKind::kUnpackedStruct: {
      if (!IsStruct(val)) {
        return false;
      }
      const auto& info = type.AsUnpackedStruct();
      const auto& s = AsStruct(val);
      for (size_t i = 0; i < info.fields.size(); ++i) {
        if (ValueContainsXZ(s.fields[i], info.fields[i].type, types)) {
          return true;
        }
      }
      return false;
    }
    case TypeKind::kUnpackedArray: {
      if (!IsArray(val)) {
        return false;
      }
      const auto& info = type.AsUnpackedArray();
      const auto& arr = AsArray(val);
      return std::ranges::any_of(arr.elements, [&](const auto& elem) {
        return ValueContainsXZ(elem, info.element_type, types);
      });
    }
    case TypeKind::kUnpackedUnion: {
      if (!IsUnion(val)) {
        return false;
      }
      return !AsUnion(val).storage_bits.IsKnown();
    }
    default:
      return false;
  }
}

}  // namespace lyra::mir::interp
