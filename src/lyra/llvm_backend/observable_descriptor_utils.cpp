#include "lyra/llvm_backend/observable_descriptor_utils.hpp"

#include <format>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/design.hpp"

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

namespace {

struct ValidatedCanonicalOwner {
  uint32_t raw_owner = 0;
};

auto ValidateCanonicalOwner(
    ObservableOwnerSlotId owner_id, const DesignLayout& layout,
    const RealizationData& realization) -> ValidatedCanonicalOwner {
  const uint32_t owner = owner_id.Raw();

  // Canonical-ness check first: storage_owner_slot_id is the authority.
  if (owner >= layout.storage_owner_slot_id.size()) {
    throw common::InternalError(
        "ValidateCanonicalOwner",
        std::format("storage_owner_slot_id missing canonical owner {}", owner));
  }
  if (layout.storage_owner_slot_id[owner] != owner) {
    throw common::InternalError(
        "ValidateCanonicalOwner",
        std::format(
            "owner {} is not self-owning; points to {}", owner,
            layout.storage_owner_slot_id[owner]));
  }

  // Companion table consistency checks.
  if (owner >= layout.slot_storage_specs.size()) {
    throw common::InternalError(
        "ValidateCanonicalOwner",
        std::format("slot_storage_specs missing canonical owner {}", owner));
  }
  if (owner >= realization.slot_kinds.size()) {
    throw common::InternalError(
        "ValidateCanonicalOwner",
        std::format("slot_kinds missing canonical owner {}", owner));
  }
  if (owner >= realization.slot_types.size()) {
    throw common::InternalError(
        "ValidateCanonicalOwner",
        std::format("slot_types missing canonical owner {}", owner));
  }

  return ValidatedCanonicalOwner{.raw_owner = owner};
}

auto ComputeCanonicalTraceShape(
    ValidatedCanonicalOwner validated, const RealizationData& realization,
    const TypeArena& type_arena) -> CanonicalTraceShape {
  return CanonicalTraceShape{
      .bit_width = ComputeTraceBitWidth(
          realization.slot_types[validated.raw_owner], type_arena),
      .trace_kind =
          MapSlotKindToTraceKind(realization.slot_kinds[validated.raw_owner]),
  };
}

auto ComputeCanonicalStorageShape(
    ValidatedCanonicalOwner validated, const DesignLayout& layout)
    -> CanonicalStorageShape {
  const SlotStorageSpec& spec = layout.slot_storage_specs[validated.raw_owner];
  runtime::SlotStorageKind storage_kind = ClassifySlotStorageKind(spec);

  std::optional<Packed4LaneShape> packed4_lanes;
  if (storage_kind == runtime::SlotStorageKind::kPacked4) {
    const PackedStorageSpec& packed = std::get<PackedStorageSpec>(spec.data);
    uint32_t lane_bytes = packed.LaneByteSize();
    packed4_lanes = Packed4LaneShape{
        .value_lane_byte_offset = 0,
        .value_lane_byte_size = lane_bytes,
        .unk_lane_byte_offset = packed.UnknownLaneOffset(),
        .unk_lane_byte_size = lane_bytes,
    };
  }

  return CanonicalStorageShape{
      .storage_kind = storage_kind,
      .total_bytes = spec.TotalByteSize(),
      .packed4_lanes = packed4_lanes,
  };
}

}  // namespace

auto ComputeCanonicalObservableShape(
    ObservableOwnerSlotId owner, const DesignLayout& layout,
    const RealizationData& realization, const TypeArena& type_arena)
    -> CanonicalObservableShape {
  ValidatedCanonicalOwner validated =
      ValidateCanonicalOwner(owner, layout, realization);
  return CanonicalObservableShape{
      .trace = ComputeCanonicalTraceShape(validated, realization, type_arena),
      .storage = ComputeCanonicalStorageShape(validated, layout),
  };
}

}  // namespace lyra::lowering::mir_to_llvm
