#include "lyra/lowering/ast_to_hir/system_call.hpp"

#include <format>
#include <vector>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lower call arguments to HIR expressions.
// Returns nullopt on failure; diagnostics are emitted by LowerExpression.
auto LowerArguments(
    const slang::ast::CallExpression& call, SymbolRegistrar& registrar,
    Context* ctx) -> std::optional<std::vector<hir::ExpressionId>> {
  std::vector<hir::ExpressionId> args;
  for (const slang::ast::Expression* arg : call.arguments()) {
    hir::ExpressionId arg_id = LowerExpression(*arg, registrar, ctx);
    if (!arg_id) {
      return std::nullopt;
    }
    args.push_back(arg_id);
  }
  return args;
}

struct LowerVisitor {
  const slang::ast::CallExpression* call = nullptr;
  SymbolRegistrar* registrar = nullptr;
  Context* ctx = nullptr;
  TypeId result_type;

  auto operator()(const DisplayFunctionInfo& info) const -> hir::ExpressionId {
    SourceSpan span = ctx->SpanOf(call->sourceRange);

    auto args = LowerArguments(*call, *registrar, ctx);
    if (!args) {
      return hir::kInvalidExpressionId;
    }

    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = hir::DisplaySystemCallData{
                .radix = info.radix,
                .append_newline = info.append_newline,
                .args = std::move(*args)}});
  }

  auto operator()(const TerminationFunctionInfo& /*info*/) const
      -> hir::ExpressionId {
    // Termination calls should be handled in statement.cpp, not here.
    // If we reach here, something is wrong.
    SourceSpan span = ctx->SpanOf(call->sourceRange);
    ctx->sink->Error(
        span,
        "termination calls ($finish/$stop/$exit) should not be used as "
        "expressions");
    return hir::kInvalidExpressionId;
  }
};

}  // namespace

auto LowerSystemCall(
    const slang::ast::CallExpression& call, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  SourceSpan span = ctx->SpanOf(call.sourceRange);
  std::string_view name = call.getSubroutineName();

  const SystemFunctionInfo* info = FindSystemFunction(name);
  if (info == nullptr) {
    ctx->sink->Error(span, std::format("unsupported system call '{}'", name));
    return hir::kInvalidExpressionId;
  }

  // Validate argument count
  size_t arg_count = call.arguments().size();
  if (arg_count < info->min_args || arg_count > info->max_args) {
    ctx->sink->Error(
        span, std::format(
                  "'{}' expects {}-{} arguments, got {}", name, info->min_args,
                  info->max_args, arg_count));
    return hir::kInvalidExpressionId;
  }

  // Determine return type from registry metadata (exhaustive switch)
  TypeId result_type;
  switch (info->return_type) {
    case SystemFunctionReturnType::kVoid:
      result_type = ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      break;
    case SystemFunctionReturnType::kString:
      ctx->sink->Error(span, "string return type not yet supported");
      return hir::kInvalidExpressionId;
  }

  return std::visit(
      LowerVisitor{
          .call = &call,
          .registrar = &registrar,
          .ctx = ctx,
          .result_type = result_type},
      info->payload);
}

}  // namespace lyra::lowering::ast_to_hir
