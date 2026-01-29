#include "lyra/lowering/ast_to_hir/expression_call.hpp"

#include <format>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/builtin_method.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerUserCall(
    const slang::ast::CallExpression& call, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto& registrar = *view.registrar;
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(call.sourceRange);

  const auto* user_sub =
      std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
  if (user_sub == nullptr) {
    ctx->sink->Error(span, "indirect function calls not supported");
    return hir::kInvalidExpressionId;
  }

  // Reject task calls from functions
  if ((*user_sub)->subroutineKind == slang::ast::SubroutineKind::Task) {
    ctx->sink->Error(span, "task calls not supported");
    return hir::kInvalidExpressionId;
  }

  SymbolId callee = registrar.Lookup(**user_sub);
  if (!callee) {
    ctx->ErrorFmt(span, "undefined function '{}'", (*user_sub)->name);
    return hir::kInvalidExpressionId;
  }

  // Check if the function was registered but marked as unsupported
  const Symbol& callee_sym = (*ctx->symbol_table)[callee];
  if (callee_sym.unsupported_reason.has_value()) {
    Diagnostic diag = Diagnostic::Error(
        span, std::format(
                  "cannot call function '{}': {}", callee_sym.name,
                  *callee_sym.unsupported_reason));
    // Add note pointing to definition if available
    if (callee_sym.definition_span.file_id) {
      diag = std::move(diag).WithNote(
          callee_sym.definition_span, "function declared here");
    }
    ctx->sink->Report(std::move(diag));
    return hir::kInvalidExpressionId;
  }

  // Lower arguments
  std::vector<hir::ExpressionId> args;
  args.reserve(call.arguments().size());
  for (const auto* arg_expr : call.arguments()) {
    hir::ExpressionId arg = LowerExpression(*arg_expr, view);
    if (!arg) {
      return hir::kInvalidExpressionId;
    }
    args.push_back(arg);
  }

  TypeId type = LowerType(*call.type, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCall,
          .type = type,
          .span = span,
          .data = hir::CallExpressionData{
              .callee = callee, .arguments = std::move(args)}});
}

}  // namespace

auto LowerCallExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  const auto& call = expr.as<slang::ast::CallExpression>();

  // 1) Builtin method calls (must check before isSystemCall - slang's
  //    isSystemCall() returns true for built-in array methods like size())
  if (auto info = ClassifyBuiltinMethod(call)) {
    return LowerBuiltinMethodCall(call, *info, view);
  }

  // 2) Desugarable system functions ($signed, $unsigned, $itor, etc.)
  if (auto classification = ClassifyDesugarableSystemFunction(call)) {
    return LowerDesugarableSystemFunction(call, *classification, view);
  }

  // 3) Non-desugarable system calls (registry + visitor)
  if (call.isSystemCall()) {
    return LowerSystemCall(call, view);
  }

  // 4) User function calls
  return LowerUserCall(call, view);
}

}  // namespace lyra::lowering::ast_to_hir
