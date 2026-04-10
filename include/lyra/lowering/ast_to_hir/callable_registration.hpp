#pragma once

#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

// Register a callable symbol (function or task) and populate the
// callable signature table in one atomic step. This is the only path
// through which callable symbols should be registered.
//
// Returns the registered SymbolId, or kInvalidSymbolId on type error.
auto RegisterCallableSymbol(
    const slang::ast::SubroutineSymbol& sub, SymbolKind kind, Context& ctx,
    SymbolRegistrar& registrar, SourceSpan span) -> SymbolId;

}  // namespace lyra::lowering::ast_to_hir
