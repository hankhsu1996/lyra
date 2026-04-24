#pragma once

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/callable_signature.hpp"

namespace lyra {
class DiagnosticSink;
}

namespace lyra::lowering::ast_to_hir {

// Query a callable's signature from the persistent HIR callable table.
// Returns nullptr if the callable is not found (emits diagnostic).
auto GetHirCallableSignature(
    SymbolId callee_symbol, const hir::HirCallableSignatureTable& table,
    DiagnosticSink& sink, SourceSpan span) -> const hir::HirCallableSignature*;

}  // namespace lyra::lowering::ast_to_hir
