#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/interp/blob_codec.hpp"
#include "lyra/mir/interp/interp_helpers.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/location.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir::interp {

namespace {

// Sign-extends a value based on its bit width to produce a signed int64.
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

// Evaluates an index operand to int64, handling sign extension and X/Z checks.
auto ResolveIndex(
    const RuntimeValue& idx_val, const Operand& idx_operand, const Arena& arena,
    const TypeArena& types) -> Result<int64_t> {
  if (!IsIntegral(idx_val)) {
    throw common::InternalError("ResolveIndex", "index must be integral");
  }
  const auto& idx_int = AsIntegral(idx_val);
  if (!idx_int.IsKnown()) {
    // TODO(hankhsu): SV semantics - should return X-filled value
    return std::unexpected(Diagnostic::HostError("index is X/Z"));
  }
  uint64_t raw = idx_int.value.empty() ? 0 : idx_int.value[0];
  TypeId type_id = TypeOfOperand(idx_operand, arena, types);
  if (IsSignedIntegral(types, type_id)) {
    return SignExtendToInt64(raw, idx_int.bit_width);
  }
  return static_cast<int64_t>(raw);
}

// Evaluates a bit offset operand to uint64, checking for negative values.
auto ResolveBitOffset(const RuntimeValue& offset_val) -> Result<uint64_t> {
  if (!IsIntegral(offset_val)) {
    throw common::InternalError(
        "ResolveBitOffset", "bit offset must be integral");
  }
  const auto& offset_int = AsIntegral(offset_val);
  int64_t raw_offset =
      offset_int.value.empty() ? 0 : static_cast<int64_t>(offset_int.value[0]);
  if (raw_offset < 0) {
    // TODO(hankhsu): SV semantics - should return X-filled value
    return std::unexpected(Diagnostic::HostError("negative bit offset"));
  }
  return static_cast<uint64_t>(raw_offset);
}

}  // namespace

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
    -> Result<ConstLocation> {
  ConstLocation loc{
      .base = &root, .bit_slice = std::nullopt, .blob_view = std::nullopt};
  TypeId current_type = place.root.type;

  // Error to propagate from visitor
  std::optional<Diagnostic> error;

  for (const auto& proj : place.projections) {
    std::visit(
        common::Overloaded{
            [&](const FieldProjection& fp) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "Field projection after BitRange");
              }
              auto idx = static_cast<size_t>(fp.field_index);
              if (loc.blob_view) {
                Layout layout = LayoutOf(loc.blob_view->view_type, *types_);
                if (idx >= layout.fields.size()) {
                  throw common::InternalError(
                      "ApplyProjectionsForRead",
                      "field index out of range in blob mode");
                }
                loc.blob_view->bit_offset += layout.fields[idx].bit_offset;
                loc.blob_view->view_type = layout.fields[idx].type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsStruct(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "field projection on non-struct");
              }
              const auto& s = AsStruct(*loc.base);
              if (idx >= s.fields.size()) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "field index out of range");
              }
              loc.base = &s.fields[idx];
              const auto& struct_info =
                  (*types_)[current_type].AsUnpackedStruct();
              current_type = struct_info.fields[idx].type;
            },
            [&](const IndexProjection& ip) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "Index projection after BitRange");
              }
              auto idx_val_result = EvalOperand(state, ip.index);
              if (!idx_val_result) {
                error = std::move(idx_val_result).error();
                return;
              }
              auto idx_result =
                  ResolveIndex(*idx_val_result, ip.index, *arena_, *types_);
              if (!idx_result) {
                error = std::move(idx_result).error();
                return;
              }
              int64_t idx = *idx_result;

              if (loc.blob_view) {
                Layout layout = LayoutOf(loc.blob_view->view_type, *types_);
                const auto& arr_info =
                    (*types_)[loc.blob_view->view_type].AsUnpackedArray();
                if (idx < 0 ||
                    std::cmp_greater_equal(idx, arr_info.range.Size())) {
                  // TODO(hankhsu): SV semantics - should return X
                  error = Diagnostic::HostError("array index out of bounds");
                  return;
                }
                loc.blob_view->bit_offset +=
                    static_cast<uint32_t>(idx) * *layout.element_stride_bits;
                loc.blob_view->view_type = *layout.element_type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsArray(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "index projection on non-array");
              }
              const auto& arr = AsArray(*loc.base);
              if (idx < 0 || static_cast<size_t>(idx) >= arr.elements.size()) {
                // TODO(hankhsu): SV semantics - should return X
                error = Diagnostic::HostError("array index out of bounds");
                return;
              }
              loc.base = &arr.elements[static_cast<size_t>(idx)];
              const auto& type = (*types_)[current_type];
              if (type.Kind() == TypeKind::kUnpackedArray) {
                current_type = type.AsUnpackedArray().element_type;
              } else if (type.Kind() == TypeKind::kDynamicArray) {
                current_type = type.AsDynamicArray().element_type;
              } else if (type.Kind() == TypeKind::kQueue) {
                current_type = type.AsQueue().element_type;
              }
            },
            [&](const BitRangeProjection& br) {
              if (error) {
                return;
              }
              auto offset_val_result = EvalOperand(state, br.bit_offset);
              if (!offset_val_result) {
                error = std::move(offset_val_result).error();
                return;
              }
              auto offset_result = ResolveBitOffset(*offset_val_result);
              if (!offset_result) {
                error = std::move(offset_result).error();
                return;
              }
              uint64_t offset = *offset_result;

              if (loc.blob_view) {
                loc.bit_slice = BitSlice{
                    .total_offset = loc.blob_view->bit_offset + offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
                return;
              }

              if (!loc.bit_slice && !IsIntegral(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "BitRangeProjection on non-integral base");
              }

              if (loc.bit_slice) {
                loc.bit_slice->total_offset += offset;
                loc.bit_slice->width = br.width;
                loc.bit_slice->element_type = br.element_type;
              } else {
                loc.bit_slice = BitSlice{
                    .total_offset = offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
              }
            },
            [&](const UnionMemberProjection& up) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForRead",
                    "Union projection after BitRange");
              }
              if (loc.blob_view) {
                const auto& union_info =
                    (*types_)[loc.blob_view->view_type].AsUnpackedUnion();
                loc.blob_view->view_type =
                    union_info.members[up.member_index].type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsUnion(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForRead", "union projection on non-union");
              }
              const auto& union_info =
                  (*types_)[current_type].AsUnpackedUnion();
              loc.blob_view = ConstBitBlobView{
                  .storage = &AsUnion(*loc.base).storage_bits,
                  .bit_offset = 0,
                  .view_type = union_info.members[up.member_index].type,
                  .root_union_type = current_type,
              };
              current_type = union_info.members[up.member_index].type;
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

    if (error) {
      return std::unexpected(std::move(*error));
    }
  }
  return loc;
}

auto Interpreter::ApplyProjectionsForWrite(
    ProcessState& state, const Place& place, RuntimeValue& root)
    -> Result<Location> {
  Location loc{
      .base = &root, .bit_slice = std::nullopt, .blob_view = std::nullopt};
  TypeId current_type = place.root.type;

  // Error to propagate from visitor
  std::optional<Diagnostic> error;

  for (const auto& proj : place.projections) {
    std::visit(
        common::Overloaded{
            [&](const FieldProjection& fp) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "Field projection after BitRange");
              }
              auto idx = static_cast<size_t>(fp.field_index);
              if (loc.blob_view) {
                Layout layout = LayoutOf(loc.blob_view->view_type, *types_);
                if (idx >= layout.fields.size()) {
                  throw common::InternalError(
                      "ApplyProjectionsForWrite",
                      "field index out of range in blob mode");
                }
                loc.blob_view->bit_offset += layout.fields[idx].bit_offset;
                loc.blob_view->view_type = layout.fields[idx].type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsStruct(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "field projection on non-struct");
              }
              auto& s = AsStruct(*loc.base);
              if (idx >= s.fields.size()) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite", "field index out of range");
              }
              loc.base = &s.fields[idx];
              const auto& struct_info =
                  (*types_)[current_type].AsUnpackedStruct();
              current_type = struct_info.fields[idx].type;
            },
            [&](const IndexProjection& ip) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "Index projection after BitRange");
              }
              auto idx_val_result = EvalOperand(state, ip.index);
              if (!idx_val_result) {
                error = std::move(idx_val_result).error();
                return;
              }
              auto idx_result =
                  ResolveIndex(*idx_val_result, ip.index, *arena_, *types_);
              if (!idx_result) {
                error = std::move(idx_result).error();
                return;
              }
              int64_t idx = *idx_result;

              if (loc.blob_view) {
                Layout layout = LayoutOf(loc.blob_view->view_type, *types_);
                const auto& arr_info =
                    (*types_)[loc.blob_view->view_type].AsUnpackedArray();
                if (idx < 0 ||
                    std::cmp_greater_equal(idx, arr_info.range.Size())) {
                  // TODO(hankhsu): SV semantics - should return X
                  error = Diagnostic::HostError("array index out of bounds");
                  return;
                }
                loc.blob_view->bit_offset +=
                    static_cast<uint32_t>(idx) * *layout.element_stride_bits;
                loc.blob_view->view_type = *layout.element_type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsArray(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "index projection on non-array");
              }
              auto& arr = AsArray(*loc.base);
              if (idx < 0 || static_cast<size_t>(idx) >= arr.elements.size()) {
                // TODO(hankhsu): SV semantics - should return X
                error = Diagnostic::HostError("array index out of bounds");
                return;
              }
              loc.base = &arr.elements[static_cast<size_t>(idx)];
              const auto& type = (*types_)[current_type];
              if (type.Kind() == TypeKind::kUnpackedArray) {
                current_type = type.AsUnpackedArray().element_type;
              } else if (type.Kind() == TypeKind::kDynamicArray) {
                current_type = type.AsDynamicArray().element_type;
              } else if (type.Kind() == TypeKind::kQueue) {
                current_type = type.AsQueue().element_type;
              }
            },
            [&](const BitRangeProjection& br) {
              if (error) {
                return;
              }
              auto offset_val_result = EvalOperand(state, br.bit_offset);
              if (!offset_val_result) {
                error = std::move(offset_val_result).error();
                return;
              }
              auto offset_result = ResolveBitOffset(*offset_val_result);
              if (!offset_result) {
                error = std::move(offset_result).error();
                return;
              }
              uint64_t offset = *offset_result;

              if (loc.blob_view) {
                loc.bit_slice = BitSlice{
                    .total_offset = loc.blob_view->bit_offset + offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
                return;
              }

              if (!loc.bit_slice && !IsIntegral(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "BitRangeProjection on non-integral base");
              }

              if (loc.bit_slice) {
                loc.bit_slice->total_offset += offset;
                loc.bit_slice->width = br.width;
                loc.bit_slice->element_type = br.element_type;
              } else {
                loc.bit_slice = BitSlice{
                    .total_offset = offset,
                    .width = br.width,
                    .element_type = br.element_type,
                };
              }
            },
            [&](const UnionMemberProjection& up) {
              if (error) {
                return;
              }
              if (loc.bit_slice) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "Union projection after BitRange");
              }
              if (loc.blob_view) {
                const auto& union_info =
                    (*types_)[loc.blob_view->view_type].AsUnpackedUnion();
                loc.blob_view->view_type =
                    union_info.members[up.member_index].type;
                current_type = loc.blob_view->view_type;
                return;
              }
              if (!IsUnion(*loc.base)) {
                throw common::InternalError(
                    "ApplyProjectionsForWrite",
                    "union projection on non-union");
              }
              const auto& union_info =
                  (*types_)[current_type].AsUnpackedUnion();
              loc.blob_view = BitBlobView{
                  .storage = &AsUnion(*loc.base).storage_bits,
                  .bit_offset = 0,
                  .view_type = union_info.members[up.member_index].type,
                  .root_union_type = current_type,
              };
              current_type = union_info.members[up.member_index].type;
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

    if (error) {
      return std::unexpected(std::move(*error));
    }
  }
  return loc;
}

auto Interpreter::ReadPlace(const ProcessState& state, PlaceId place_id)
    -> Result<RuntimeValue> {
  const auto& place = (*arena_)[place_id];
  const auto& root_value = ResolveRoot(state, place.root);

  if (place.projections.empty()) {
    return Clone(root_value);
  }

  auto loc_result = ApplyProjectionsForRead(state, place, root_value);
  if (!loc_result) {
    return std::unexpected(std::move(loc_result).error());
  }
  auto& loc = *loc_result;

  if (loc.bit_slice) {
    const auto& bs = *loc.bit_slice;
    const auto& container =
        loc.blob_view ? *loc.blob_view->storage : AsIntegral(*loc.base);
    if (bs.total_offset + bs.width > container.bit_width) {
      // TODO(hankhsu): SV semantics - should return X-filled value
      return std::unexpected(
          Diagnostic::HostError("bit slice exceeds container width"));
    }
    return IntegralExtractSlice(
        container, static_cast<uint32_t>(bs.total_offset), bs.width);
  }
  if (loc.blob_view) {
    return LoadFromBlob(
        loc.blob_view->view_type, *loc.blob_view->storage,
        loc.blob_view->bit_offset, *types_);
  }
  return Clone(*loc.base);
}

auto Interpreter::WritePlace(ProcessState& state, PlaceId place_id)
    -> Result<std::reference_wrapper<RuntimeValue>> {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    return std::ref(root_value);
  }

  auto loc_result = ApplyProjectionsForWrite(state, place, root_value);
  if (!loc_result) {
    return std::unexpected(std::move(loc_result).error());
  }
  auto& loc = *loc_result;

  if (loc.bit_slice) {
    throw common::InternalError(
        "WritePlace", "BitRange requires StoreToPlace path");
  }
  if (loc.blob_view) {
    throw common::InternalError(
        "WritePlace", "BlobView requires StoreToPlace path");
  }
  return std::ref(*loc.base);
}

auto Interpreter::StoreToPlace(
    ProcessState& state, PlaceId place_id, RuntimeValue value) -> Result<void> {
  const auto& place = (*arena_)[place_id];
  auto& root_value = ResolveRootMut(state, place.root);

  if (place.projections.empty()) {
    root_value = std::move(value);
    return {};
  }

  auto loc_result = ApplyProjectionsForWrite(state, place, root_value);
  if (!loc_result) {
    return std::unexpected(std::move(loc_result).error());
  }
  auto& loc = *loc_result;

  if (loc.bit_slice) {
    const auto& bs = *loc.bit_slice;
    auto& container =
        loc.blob_view ? *loc.blob_view->storage : AsIntegral(*loc.base);
    if (bs.total_offset + bs.width > container.bit_width) {
      // TODO(hankhsu): SV semantics - should return X-filled value
      return std::unexpected(
          Diagnostic::HostError("bit slice exceeds container width"));
    }
    if (!IsIntegral(value)) {
      throw common::InternalError(
          "StoreToPlace", "writing non-integral value to bit slice");
    }
    const auto& val_integral = AsIntegral(value);
    if (val_integral.bit_width != bs.width) {
      throw common::InternalError("StoreToPlace", "bit slice width mismatch");
    }
    if (loc.blob_view) {
      *loc.blob_view->storage = IntegralInsertSlice4State(
          container, val_integral, static_cast<uint32_t>(bs.total_offset),
          bs.width);
    } else {
      *loc.base = IntegralInsertSlice4State(
          container, val_integral, static_cast<uint32_t>(bs.total_offset),
          bs.width);
    }
  } else if (loc.blob_view) {
    // Float policy check on root union
    const auto& union_info =
        (*types_)[loc.blob_view->root_union_type].AsUnpackedUnion();
    if (union_info.contains_float &&
        ValueContainsXZ(value, loc.blob_view->view_type, *types_)) {
      return std::unexpected(
          Diagnostic::HostError("X/Z write to float-containing union"));
    }
    StoreToBlob(
        loc.blob_view->view_type, value, *loc.blob_view->storage,
        loc.blob_view->bit_offset, *types_);
  } else {
    *loc.base = std::move(value);
  }
  return {};
}

}  // namespace lyra::mir::interp
