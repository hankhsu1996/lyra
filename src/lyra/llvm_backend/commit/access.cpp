#include "lyra/llvm_backend/commit/access.hpp"

#include <cstdint>
#include <optional>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm::commit {

auto Access::GetWriteTarget(Context& ctx, mir::PlaceId target)
    -> Result<WriteTarget> {
  return ctx.GetWriteTarget(target);
}

auto Access::GetWriteTarget(Context& ctx, const mir::WriteTarget& target)
    -> Result<WriteTarget> {
  if (const auto* place = std::get_if<mir::PlaceId>(&target)) {
    return ctx.GetWriteTarget(*place);
  }
  return ctx.GetWriteTarget(std::get<mir::ExternalRefId>(target));
}

auto Access::GetMutationTargetSignalCoord(Context& ctx, mir::PlaceId target)
    -> std::optional<SignalCoordExpr> {
  return ctx.GetMutationTargetSignalCoord(target);
}

auto Access::IsNotifiableMutationTarget(Context& ctx, mir::PlaceId target)
    -> bool {
  return ctx.GetMutationTargetSignalCoord(target).has_value();
}

}  // namespace lyra::lowering::mir_to_llvm::commit
