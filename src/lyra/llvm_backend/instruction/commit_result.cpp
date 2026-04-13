#include "lyra/llvm_backend/instruction/commit_result.hpp"

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/llvm_backend/write_route.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

auto CommitRuntimeResult(
    Context& ctx, const CuFacts& facts, SlotAccessResolver& resolver,
    mir::PlaceId dest, llvm::Value* raw_value) -> Result<void> {
  mir::WriteTarget target{dest};
  auto route = RouteWriteTarget(ctx, facts, resolver, target);

  bool is_managed = common::TypeContainsManaged(route.dest_type, *facts.types);
  switch (route.kind) {
    case CommitRouteKind::kBitRange:
      throw common::InternalError(
          "CommitRuntimeResult",
          "runtime result destination has bit-range projection");
    case CommitRouteKind::kResolverRouted: {
      if (is_managed) {
        return resolver.CommitSlotValue(
            dest, raw_value, route.dest_type, OwnershipPolicy::kMove);
      }
      return resolver.CommitPlainSlotValue(dest, raw_value, route.dest_type);
    }
    case CommitRouteKind::kDirect: {
      if (is_managed) {
        return DispatchWrite(
            ctx, facts, target, RawValueSource{raw_value}, route.dest_type,
            OwnershipPolicy::kMove);
      }
      return DispatchPlainWrite(
          ctx, facts, target, RawValueSource{raw_value}, route.dest_type);
    }
  }
  throw common::InternalError(
      "CommitRuntimeResult", "unhandled CommitRouteKind");
}

}  // namespace lyra::lowering::mir_to_llvm
