#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/lifetime_extension.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto FromSlangProceduralBlockKind(slang::ast::ProceduralBlockKind kind)
    -> hir::ProcessKind {
  switch (kind) {
    case slang::ast::ProceduralBlockKind::Initial:
      return hir::ProcessKind::kInitial;
    case slang::ast::ProceduralBlockKind::Final:
      return hir::ProcessKind::kFinal;
    case slang::ast::ProceduralBlockKind::Always:
      return hir::ProcessKind::kAlways;
    case slang::ast::ProceduralBlockKind::AlwaysComb:
      return hir::ProcessKind::kAlwaysComb;
    case slang::ast::ProceduralBlockKind::AlwaysLatch:
      return hir::ProcessKind::kAlwaysLatch;
    case slang::ast::ProceduralBlockKind::AlwaysFF:
      return hir::ProcessKind::kAlwaysFf;
  }
  throw InternalError(
      "FromSlangProceduralBlockKind: unknown ProceduralBlockKind");
}

}  // namespace

void ProcessLowerer::AnalyzeLifetimeExtended(
    const slang::ast::Statement& body) {
  lifetime_extended_ = CollectLifetimeExtendedVars(body);
}

ProcessLowerer::ProcessLowerer(
    ModuleLowerer& module, const slang::ast::Symbol& containing_symbol)
    : module_(&module), containing_symbol_(&containing_symbol) {
}

auto ProcessLowerer::Run(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame parent_frame)
    -> diag::Result<hir::Process> {
  hir::ProceduralBody body;

  // Open the root lexical scope accumulators. Every body-tree var declared
  // below feeds into these vectors and becomes the root scope's direct
  // declarations; nested named blocks / forks / foreach scopes push their
  // ids into the root scope's children list.
  std::vector<hir::ProceduralVarId> root_declarations;
  std::vector<hir::ProceduralScopeId> root_children;
  const WalkFrame frame =
      parent_frame.WithProceduralBody(&body, &body.exprs)
          .WithProceduralScopeAccumulators(&root_declarations, &root_children);

  AnalyzeLifetimeExtended(proc.getBody());
  auto root_stmt = LowerStmt(proc.getBody(), frame);
  if (!root_stmt) return std::unexpected(std::move(root_stmt.error()));
  body.root_stmt = body.stmts.Add(*std::move(root_stmt));

  if (parent_frame.current_structural_scope != nullptr) {
    body.root_scope =
        parent_frame.current_structural_scope->procedural_scopes.Add(
            hir::ProceduralScopeDecl{
                .kind = hir::ProceduralScopeKind::kProcessRoot,
                .label = std::nullopt,
                .direct_declarations = std::move(root_declarations),
                .direct_child_scopes = std::move(root_children)});
  }

  const auto& mapper = module_->SourceMapper();
  const auto span = mapper.PointSpanOf(proc.location);
  const auto kind = FromSlangProceduralBlockKind(proc.procedureKind);

  hir::Process out{
      .kind = kind,
      .span = span,
      .body = std::move(body),
      .implicit_sensitivity_list = {}};
  if (kind == hir::ProcessKind::kAlwaysComb ||
      kind == hir::ProcessKind::kAlwaysLatch) {
    // LRM 9.2.2.2.1 / 9.2.2.3: an always_comb / always_latch wakes on the reads
    // of its whole procedure, including reads inside any function it calls --
    // the procedure-level sensitivity, not the raw read set of the body node,
    // which reflects only call arguments across a function boundary.
    out.implicit_sensitivity_list = module_->TranslateSensitivityReads(
        module_->Sensitivity().AnalyzeProcedureSensitivity(proc), frame);
  }
  return out;
}

auto ProcessLowerer::AddProceduralVar(
    const WalkFrame& frame, hir::ProceduralBody& body,
    const slang::ast::VariableSymbol& var, hir::TypeId type)
    -> hir::ProceduralVarId {
  const hir::ProceduralVarId id = body.procedural_vars.Add(
      hir::ProceduralVarDecl{
          .name = std::string{var.name},
          .type = type,
          .lifetime = var.lifetime == slang::ast::VariableLifetime::Automatic
                          ? hir::VariableLifetime::kAutomatic
                          : hir::VariableLifetime::kStatic,
          .lifetime_extended = lifetime_extended_.contains(&var)});
  const auto [_, inserted] = procedural_var_bindings_.emplace(&var, id);
  if (!inserted) {
    throw InternalError(
        "ProcessLowerer::AddProceduralVar: procedural variable symbol "
        "already mapped");
  }
  frame.current_scope_declarations->push_back(id);
  return id;
}

auto ProcessLowerer::LookupProceduralVar(const slang::ast::VariableSymbol& var)
    const -> std::optional<hir::ProceduralVarId> {
  const auto it = procedural_var_bindings_.find(&var);
  if (it == procedural_var_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

}  // namespace lyra::lowering::ast_to_hir
