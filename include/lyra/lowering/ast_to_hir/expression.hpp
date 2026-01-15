#pragma once

#include <slang/ast/Expression.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

auto LowerExpression(
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
