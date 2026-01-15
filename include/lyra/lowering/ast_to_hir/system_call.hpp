#pragma once

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/hir/fwd.hpp"

namespace lyra {
class DiagnosticSink;
}

namespace lyra::lowering::ast_to_hir {

class Context;
class SymbolRegistrar;

auto LowerSystemCall(
    const slang::ast::CallExpression& call, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
