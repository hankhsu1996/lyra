#include "lyra/lowering/ast_to_hir/type_seeder.hpp"

#include <concepts>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Visitor that walks body-frontier AST nodes and interns all reachable types
// and deterministic synthetic types derived from the Phase 1 frontier.
//
// Contract:
//   - Seed all body-reachable AST types (expression results, variables,
//     subroutine signatures).
//   - Seed all deterministic synthetic types that Phase 1 lowering will
//     construct from those AST types (e.g. DynamicArray<KeyType> for
//     foreach-over-associative-array desugaring).
//   - Do not recurse into child instances.
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

  // Assignment pattern expressions: slang's StructuredAssignmentPattern
  // visitExprs does not visit the base class elements(). Those resolved
  // per-field expressions may have types not reachable through setters
  // alone. Visit them explicitly.
  void handle(const slang::ast::StructuredAssignmentPatternExpression& expr) {
    LowerType(*expr.type, SourceSpan{}, ctx);
    for (const auto* elem : expr.elements()) {
      elem->visit(*this);
    }
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

  // VariableDeclStatement does not expose its initializer via visitExprs.
  // Explicitly visit the variable symbol so that initializer expressions
  // (and their types) are seeded.
  void handle(const slang::ast::VariableDeclStatement& stmt) {
    stmt.symbol.visit(*this);
  }

  // Seed synthetic DynamicArray<KeyType> for foreach-over-associative-array.
  // Phase 1 desugaring creates this wrapper; the seeder pre-interns it so
  // the arena can be frozen before body lowering.
  void handle(const slang::ast::ForeachLoopStatement& fs) {
    if (fs.arrayRef.type->isAssociativeArray()) {
      TypeId aa_type = LowerType(*fs.arrayRef.type, SourceSpan{}, ctx);
      if (aa_type) {
        TypeId key_type = ForeachSnapshotKeyType(aa_type, ctx);
        ctx->type_arena->Intern(
            TypeKind::kDynamicArray,
            DynamicArrayInfo{.element_type = key_type});
      }
    }
    visitDefault(fs);
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
