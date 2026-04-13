#include "lyra/llvm_backend/type_ops/dispatch.hpp"

#include <variant>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/assign_core.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto DetermineOwnership(Context& context, const mir::Operand& source)
    -> OwnershipPolicy {
  if (!std::holds_alternative<mir::PlaceId>(source.payload)) {
    return OwnershipPolicy::kClone;
  }

  auto src_place_id = std::get<mir::PlaceId>(source.payload);
  const auto& arena = context.GetMirArena();
  const auto& src_place = arena[src_place_id];

  if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
    return OwnershipPolicy::kMove;
  }

  return OwnershipPolicy::kClone;
}

// Operand-based assignment adapter. Determines ownership policy, then
// delegates to DispatchWrite with OperandSource. Does not own semantic
// write-shape decisions.
auto AssignPlace(
    Context& context, const CuFacts& facts, const mir::WriteTarget& target,
    const mir::Operand& source) -> Result<void> {
  TypeId type_id = detail::ResolveDestType(context, facts, target);
  OwnershipPolicy policy = DetermineOwnership(context, source);
  return DispatchWrite(
      context, facts, target, OperandSource{&source}, type_id, policy);
}

}  // namespace lyra::lowering::mir_to_llvm
