#include "lyra/lowering/ast_to_hir/type_seeder.hpp"

#include <concepts>

#include <slang/ast/ASTVisitor.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Visitor that walks body-frontier AST nodes and interns all reachable types.
// Only produces TypeArena side effects; does not build HIR.
struct BodyTypeSeedVisitor
    : slang::ast::ASTVisitor<BodyTypeSeedVisitor, true, true> {
  Context* ctx;

  // Intercept all concrete expression types and intern their result type.
  // Uses a concept constraint so visitDefault receives the concrete final
  // type (Expression itself is not final).
  void handle(std::derived_from<slang::ast::Expression> auto& expr) {
    LowerType(*expr.type, SourceSpan{}, ctx);
    visitDefault(expr);
  }

  void handle(const slang::ast::VariableSymbol& var) {
    LowerType(var.getType(), SourceSpan{}, ctx);
    visitDefault(var);
  }

  void handle(const slang::ast::SubroutineSymbol& sub) {
    LowerType(sub.getReturnType(), SourceSpan{}, ctx);
    for (const auto* arg : sub.getArguments()) {
      LowerType(arg->getType(), SourceSpan{}, ctx);
    }
    visitDefault(sub);
  }

  // Do not recurse into child instances. The seeder walks the body frontier
  // provided by BodyLoweringInput, not the design hierarchy.
  void handle(const slang::ast::InstanceSymbol& /*unused*/) {
  }
};

}  // namespace

void SeedBodyTypes(const BodyLoweringInput& input, Context* ctx) {
  // Fork context with a local sink. This prepass is structural type
  // completion -- Phase 1 owns user-visible diagnostics.
  DiagnosticSink seed_sink;
  Context seed_ctx = *ctx;
  seed_ctx.sink = &seed_sink;

  BodyTypeSeedVisitor visitor{.ctx = &seed_ctx};

  for (const auto* proc : input.processes) {
    proc->visit(visitor);
  }

  for (const auto* ca : input.continuous_assigns) {
    ca->getAssignment().visit(visitor);
  }

  for (const auto* func : input.functions) {
    func->visit(visitor);
  }

  for (const auto* task : input.tasks) {
    task->visit(visitor);
  }

  for (const auto& var_init : input.var_inits) {
    var_init.initializer->visit(visitor);
  }
}

}  // namespace lyra::lowering::ast_to_hir
