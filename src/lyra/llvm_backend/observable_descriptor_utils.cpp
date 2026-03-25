#include "lyra/llvm_backend/observable_descriptor_utils.hpp"

#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_queries.hpp"
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

}  // namespace lyra::lowering::mir_to_llvm
