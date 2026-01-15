#include "lyra/lowering/ast_to_hir/package.hpp"

#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerPackage(
    const slang::ast::PackageSymbol& package, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Package {
  SourceSpan span = ctx->SpanOf(GetSourceRange(package));

  SymbolId symbol =
      registrar.Register(package, SymbolKind::kPackage, kInvalidTypeId);

  // TODO(hankhsu): Lower package members when needed

  return hir::Package{
      .symbol = symbol,
      .span = span,
  };
}

}  // namespace lyra::lowering::ast_to_hir
