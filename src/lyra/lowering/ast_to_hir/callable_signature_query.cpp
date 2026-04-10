#include "lyra/lowering/ast_to_hir/callable_signature_query.hpp"

#include "lyra/common/diagnostic/diagnostic_sink.hpp"

namespace lyra::lowering::ast_to_hir {

auto GetHirCallableSignature(
    SymbolId callee_symbol, const hir::HirCallableSignatureTable& table,
    DiagnosticSink& sink, SourceSpan span) -> const hir::HirCallableSignature* {
  const auto* sig = table.Lookup(callee_symbol);
  if (sig == nullptr) {
    sink.Error(
        span, "cannot resolve callable signature for deferred assertion");
  }
  return sig;
}

}  // namespace lyra::lowering::ast_to_hir
