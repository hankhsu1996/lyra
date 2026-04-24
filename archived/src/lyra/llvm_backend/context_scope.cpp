#include "lyra/llvm_backend/context_scope.hpp"

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

StatementScope::StatementScope(Context& ctx) : ctx_(ctx) {
  ctx_.ClearOwnedTemps();
}

StatementScope::~StatementScope() {
  ctx_.ReleaseOwnedTemps();
}

OriginScope::OriginScope(Context& ctx, common::OriginId origin)
    : ctx_(ctx),
      saved_origin_(common::OriginId::Invalid()),
      pushed_(origin.IsValid()) {
  if (pushed_) {
    saved_origin_ = ctx_.GetCurrentOrigin();
    ctx_.SetCurrentOrigin(origin);
  }
  // If Invalid: do nothing, preserve outer origin
}

OriginScope::~OriginScope() {
  if (pushed_) {
    ctx_.SetCurrentOrigin(saved_origin_);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
