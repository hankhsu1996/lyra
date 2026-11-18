#include "lyra/lowering/ast_to_hir/random_qualifier.hpp"

#include <slang/ast/SemanticFacts.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/random_qualifier.hpp"

namespace lyra::lowering::ast_to_hir {

auto DeclaredRandomQualifier(slang::ast::RandMode mode)
    -> support::RandomQualifier {
  switch (mode) {
    case slang::ast::RandMode::None:
      return support::RandomQualifier::kNone;
    case slang::ast::RandMode::Rand:
      return support::RandomQualifier::kRand;
    case slang::ast::RandMode::RandC:
      return support::RandomQualifier::kRandc;
  }
  throw InternalError("DeclaredRandomQualifier: unknown rand mode");
}

}  // namespace lyra::lowering::ast_to_hir
