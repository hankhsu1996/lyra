#include "lyra/llvm_backend/observable_descriptor_utils.hpp"

#include <format>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ClassifySlotStorageKind(const SlotStorageSpec& spec)
    -> runtime::SlotStorageKind {
  return std::visit(
      common::Overloaded{
          [](const PackedStorageSpec& s) {
            return s.is_four_state ? runtime::SlotStorageKind::kPacked4
                                   : runtime::SlotStorageKind::kPacked2;
          },
          [](const FloatStorageSpec&) {
            return runtime::SlotStorageKind::kPacked2;
          },
          [](const ArrayStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const StructStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const UnionStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const HandleStorageSpec& s) {
            return s.kind == HandleKind::kString
                       ? runtime::SlotStorageKind::kString
                       : runtime::SlotStorageKind::kHandle;
          },
      },
      spec.data);
}

auto MapSlotKindToTraceKind(mir::SlotKind kind) -> runtime::TraceSignalKind {
  switch (kind) {
    case mir::SlotKind::kVariable:
      return runtime::TraceSignalKind::kVariable;
    case mir::SlotKind::kNet:
      return runtime::TraceSignalKind::kNet;
    case mir::SlotKind::kParamConst:
      return runtime::TraceSignalKind::kParam;
  }
  throw common::InternalError(
      "MapSlotKindToTraceKind",
      std::format("unknown SlotKind {}", static_cast<int>(kind)));
}

auto ComputeTraceBitWidth(TypeId type_id, const TypeArena& types) -> uint32_t {
  const Type& type = types[type_id];
  if (IsPacked(type)) {
    return PackedBitWidth(type, types);
  }
  return 0;
}

auto ComputeCanonicalObservableShape(
    ObservableOwnerSlotId owner, const DesignLayout& layout, TypeId slot_type,
    mir::SlotKind slot_kind, const TypeArena& type_arena)
    -> CanonicalObservableShape {
  const uint32_t raw = owner.Raw();
  if (raw >= layout.slot_storage_specs.size()) {
    throw common::InternalError(
        "ComputeCanonicalObservableShape",
        std::format("slot_storage_specs missing canonical owner {}", raw));
  }

  const SlotStorageSpec& spec = layout.slot_storage_specs[raw];
  runtime::SlotStorageKind storage_kind = ClassifySlotStorageKind(spec);

  std::optional<Packed4LaneShape> packed4_lanes;
  if (storage_kind == runtime::SlotStorageKind::kPacked4) {
    const auto& packed = std::get<PackedStorageSpec>(spec.data);
    uint32_t lane_bytes = packed.LaneByteSize();
    packed4_lanes = Packed4LaneShape{
        .value_lane_byte_offset = 0,
        .value_lane_byte_size = lane_bytes,
        .unk_lane_byte_offset = packed.UnknownLaneOffset(),
        .unk_lane_byte_size = lane_bytes,
    };
  }

  return CanonicalObservableShape{
      .trace =
          CanonicalTraceShape{
              .bit_width = ComputeTraceBitWidth(slot_type, type_arena),
              .trace_kind = MapSlotKindToTraceKind(slot_kind),
          },
      .storage =
          CanonicalStorageShape{
              .storage_kind = storage_kind,
              .total_bytes = spec.TotalByteSize(),
              .packed4_lanes = packed4_lanes,
          },
  };
}

}  // namespace lyra::lowering::mir_to_llvm
