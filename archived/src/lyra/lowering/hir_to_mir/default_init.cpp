#include "lyra/lowering/hir_to_mir/default_init.hpp"

#include <cstdint>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Build an X constant for a packed type with the given bit width.
auto MakePackedXConstant(TypeId type, uint32_t bw) -> Constant {
  IntegralConstant ic;
  if (bw <= 64) {
    ic.value.push_back(0);
    ic.unknown.push_back(bw == 64 ? ~0ULL : (1ULL << bw) - 1);
  } else {
    uint32_t full_limbs = bw / 64;
    uint32_t remaining = bw % 64;
    for (uint32_t i = 0; i < full_limbs; ++i) {
      ic.value.push_back(0);
      ic.unknown.push_back(~0ULL);
    }
    if (remaining > 0) {
      ic.value.push_back(0);
      ic.unknown.push_back((1ULL << remaining) - 1);
    }
  }
  return Constant{.type = type, .value = std::move(ic)};
}

// Build a zero constant for a packed type with the given bit width.
auto MakePackedZeroConstant(TypeId type, uint32_t bw) -> Constant {
  IntegralConstant ic;
  if (bw <= 64) {
    ic.value.push_back(0);
    ic.unknown.push_back(0);
  } else {
    uint32_t full_limbs = bw / 64;
    uint32_t remaining = bw % 64;
    for (uint32_t i = 0; i < full_limbs; ++i) {
      ic.value.push_back(0);
      ic.unknown.push_back(0);
    }
    if (remaining > 0) {
      ic.value.push_back(0);
      ic.unknown.push_back(0);
    }
  }
  return Constant{.type = type, .value = std::move(ic)};
}

// Emit an Initialize statement: first-write to uninitialized storage.
// Unlike Assign, the backend lowers this as a pure store with no
// destroy/release of previous contents.
void EmitInitStatement(
    mir::PlaceId place, mir::Operand operand, common::OriginId origin,
    std::vector<mir::Statement>& out) {
  out.push_back(
      mir::Statement{
          .data = mir::Initialize{.dest = place, .value = std::move(operand)},
          .origin = origin});
}

// Build a constant-index operand for array element projection.
auto MakeConstantIndex(int64_t idx, TypeId offset_type) -> mir::Operand {
  return mir::Operand::Const(
      Constant{
          .type = offset_type,
          .value = IntegralConstant{
              .value = {static_cast<uint64_t>(idx)}, .unknown = {0}}});
}

void AppendDefaultInitImpl(
    mir::PlaceId place, TypeId type_id, mir::Arena& arena,
    const TypeArena& types, TypeId offset_type, common::OriginId origin,
    std::vector<mir::Statement>& out) {
  const Type& type = types[type_id];

  // Packed scalar types (integral, packed array, packed struct, enum):
  // single constant assignment.
  if (IsPacked(type)) {
    uint32_t bw = PackedBitWidth(type, types);
    if (IsIntrinsicallyPackedFourState(type, types)) {
      EmitInitStatement(
          place, mir::Operand::Const(MakePackedXConstant(type_id, bw)), origin,
          out);
    } else {
      EmitInitStatement(
          place, mir::Operand::Const(MakePackedZeroConstant(type_id, bw)),
          origin, out);
    }
    return;
  }

  switch (type.Kind()) {
    case TypeKind::kReal: {
      EmitInitStatement(
          place,
          mir::Operand::Const(
              Constant{.type = type_id, .value = RealConstant{.value = 0.0}}),
          origin, out);
      return;
    }

    case TypeKind::kShortReal: {
      EmitInitStatement(
          place,
          mir::Operand::Const(
              Constant{.type = type_id, .value = RealConstant{.value = 0.0}}),
          origin, out);
      return;
    }

    case TypeKind::kString:
    case TypeKind::kChandle:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray: {
      // All handle types default to null (zero-bits pointer).
      // This matches the old backend path which stored ConstantPointerNull.
      EmitInitStatement(
          place,
          mir::Operand::Const(
              Constant{.type = type_id, .value = NullConstant{}}),
          origin, out);
      return;
    }

    case TypeKind::kUnpackedStruct: {
      const auto& info = type.AsUnpackedStruct();
      for (int i = 0; std::cmp_less(i, info.fields.size()); ++i) {
        mir::PlaceId field_place = arena.DerivePlace(
            arena[place],
            mir::Projection{.info = mir::FieldProjection{.field_index = i}});
        AppendDefaultInitImpl(
            field_place, info.fields[i].type, arena, types, offset_type, origin,
            out);
      }
      return;
    }

    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      for (int64_t idx = 0; std::cmp_less(idx, info.range.Size()); ++idx) {
        mir::PlaceId elem_place = arena.DerivePlace(
            arena[place],
            mir::Projection{
                .info = mir::IndexProjection{
                    .index = MakeConstantIndex(idx, offset_type)}});
        AppendDefaultInitImpl(
            elem_place, info.element_type, arena, types, offset_type, origin,
            out);
      }
      return;
    }

    case TypeKind::kUnpackedUnion: {
      // Unpacked union default is whole-storage zero-fill. Members overlap
      // in storage so memberwise init is not correct. Emit an explicit
      // ZeroInitStorageEffect that the backend lowers as memset(0).
      out.push_back(
          mir::Statement{
              .data =
                  mir::Effect{
                      .op =
                          mir::ZeroInitStorageEffect{
                              .target = place, .target_type = type_id}},
              .origin = origin});
      return;
    }

    default:
      return;
  }
}

}  // namespace

void AppendDefaultInitStatements(
    mir::PlaceId place, TypeId type, mir::Arena& arena, const TypeArena& types,
    TypeId offset_type, common::OriginId origin,
    std::vector<mir::Statement>& out) {
  AppendDefaultInitImpl(place, type, arena, types, offset_type, origin, out);
}

}  // namespace lyra::lowering::hir_to_mir
