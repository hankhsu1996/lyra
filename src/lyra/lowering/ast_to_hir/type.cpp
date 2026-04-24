#include "type.hpp"

#include "lyra/hir/type.hpp"
#include "lyra/support/unsupported.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerType(UnitLoweringState& unit, const slang::ast::Type& type)
    -> hir::TypeId {
  const auto& canonical = type.getCanonicalType();

  if (!canonical.isFourState() && canonical.getBitWidth() == 32 &&
      canonical.isSigned()) {
    return unit.AddType(hir::BuiltinIntType{});
  }

  if (canonical.isFourState() && canonical.getBitWidth() == 1) {
    return unit.AddType(hir::BuiltinLogicType{});
  }

  support::Unsupported("LowerType: only `int` and `logic` types supported");
}

}  // namespace lyra::lowering::ast_to_hir
