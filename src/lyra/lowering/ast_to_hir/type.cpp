#include "type.hpp"

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerTypeData(const slang::ast::Type& type, diag::SourceSpan decl_span)
    -> diag::Result<hir::TypeData> {
  const auto& canonical = type.getCanonicalType();

  if (!canonical.isFourState() && canonical.getBitWidth() == 32 &&
      canonical.isSigned()) {
    return hir::TypeData{hir::BuiltinIntType{}};
  }
  if (canonical.isFourState() && canonical.getBitWidth() == 1) {
    return hir::TypeData{hir::BuiltinLogicType{}};
  }
  return diag::Unsupported(
      decl_span, "only `int` and `logic` types are supported",
      diag::UnsupportedCategory::kType);
}

}  // namespace lyra::lowering::ast_to_hir
