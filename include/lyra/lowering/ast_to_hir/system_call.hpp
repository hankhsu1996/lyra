#pragma once

#include <span>
#include <vector>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/common/format.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/system_call.hpp"

namespace lyra {
class DiagnosticSink;
}

namespace lyra::lowering::ast_to_hir {

class Context;
class SymbolRegistrar;

auto LowerSystemCall(
    const slang::ast::CallExpression& call, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId;

// Build FormatOps from a slice of call arguments.
// slang_args and hir_args must be the same length and already exclude
// any leading non-display arguments (e.g., file descriptor).
auto BuildDisplayFormatOps(
    std::span<const slang::ast::Expression* const> slang_args,
    std::span<const hir::ExpressionId> hir_args, FormatKind default_format,
    Context* ctx) -> std::vector<hir::FormatOp>;

}  // namespace lyra::lowering::ast_to_hir
