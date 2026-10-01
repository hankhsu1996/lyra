#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/ast_to_hir/lifetime_extension.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
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
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& containing_symbol,
    ConsumedBodyExpressions consumed_body_exprs)
    : owner_(&unit_lowerer),
      containing_symbol_(&containing_symbol),
      consumed_body_exprs_(std::move(consumed_body_exprs)) {
}

auto ProcessLowerer::Run(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame parent_frame)
    -> diag::Result<hir::Process> {
  hir::ProceduralBody body = owner_->MakeProceduralBody(proc);

  // A process is anonymous in SV (LRM 9.2), so its root scope takes a
  // synthesized segment.
  OpenProceduralScope root{
      owner_->LookupProceduralScope(proc),
      hir::ProceduralScopeKind::kProcessRoot, std::nullopt};
  const WalkFrame frame =
      parent_frame.WithProceduralBody(&body).WithOpenScope(&root);

  AnalyzeLifetimeExtended(proc.getBody());
  auto root_stmt_or = LowerStmt(proc.getBody(), frame);
  if (!root_stmt_or) return std::unexpected(std::move(root_stmt_or.error()));
  const hir::StmtId root_stmt = body.stmts.Add(*std::move(root_stmt_or));

  // LRM 9.2.2.2.1 / 9.2.2.3: an always_comb / always_latch wakes on the reads
  // of its whole procedure, including reads inside any function it calls -- the
  // procedure-level sensitivity, not the reads of the body node, which reflect
  // only call arguments across a function boundary. What it watches is
  // stated in the body, so it is stated while the body is still open.
  const auto kind = FromSlangProceduralBlockKind(proc.procedureKind);
  std::vector<hir::SensitivityEntry> implicit_sensitivity;
  if (kind == hir::ProcessKind::kAlwaysComb ||
      kind == hir::ProcessKind::kAlwaysLatch) {
    auto sensitivity = owner_->TranslateSensitivityReads(
        *this, owner_->Sensitivity().AnalyzeProcedureSensitivity(proc), frame);
    if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
    implicit_sensitivity = *std::move(sensitivity);
  }
  body.root_scope = parent_frame.SealScope(std::move(root));

  return hir::Process{
      .kind = kind,
      .span = owner_->SourceMapper().PointSpanOf(proc.location),
      .body = std::move(body),
      .root_stmt = root_stmt,
      .implicit_sensitivity_list = std::move(implicit_sensitivity)};
}

auto ProcessLowerer::RunConcurrentAssertion(
    const slang::ast::ProceduralBlockSymbol& proc,
    const slang::ast::ConcurrentAssertionStatement& as,
    const slang::ast::StatementBlockSymbol* named_block, WalkFrame parent_frame)
    -> diag::Result<hir::ConcurrentAssertionDecl> {
  hir::ProceduralBody body = owner_->MakeProceduralBody(proc);

  OpenProceduralScope root{
      owner_->LookupProceduralScope(proc),
      hir::ProceduralScopeKind::kProcessRoot, std::nullopt};
  const WalkFrame root_frame =
      parent_frame.WithProceduralBody(&body).WithOpenScope(&root);

  // A label puts a `begin` / `end` around the assertion (LRM 9.3.5), and the
  // statements an outcome selects stand inside it, so the walk descends into it
  // the way it descends into any block the source wrote.
  std::optional<OpenProceduralScope> labelled;
  if (named_block != nullptr) {
    labelled.emplace(
        owner_->LookupProceduralScope(*named_block),
        hir::ProceduralScopeKind::kBlock, std::string{named_block->name});
  }
  const WalkFrame frame =
      labelled.has_value() ? root_frame.WithOpenScope(&*labelled) : root_frame;

  AnalyzeLifetimeExtended(proc.getBody());
  const auto span = owner_->SourceMapper().PointSpanOf(proc.location);
  auto assertion_or = LowerConcurrentAssertion(*this, frame, as, span);
  if (!assertion_or) return std::unexpected(std::move(assertion_or.error()));

  std::optional<hir::ProceduralScopeId> labelled_id;
  if (labelled.has_value()) {
    labelled_id = root_frame.SealScope(*std::move(labelled));
  }
  body.root_scope = parent_frame.SealScope(std::move(root));

  return hir::ConcurrentAssertionDecl{
      .span = span,
      .assertion = *std::move(assertion_or),
      .action = std::move(body),
      .standing_scope = labelled_id.value_or(body.root_scope)};
}

auto ProcessLowerer::DeclareProceduralVar(
    const WalkFrame& frame, hir::ProceduralBody& body,
    const slang::ast::ValueSymbol& var) -> hir::ProceduralVarId {
  const auto declared = owner_->LookupProceduralStatic(var);
  const hir::ProceduralVarId id =
      declared.has_value() ? declared->var : body.procedural_vars.Declare();
  const auto [_, inserted] = procedural_var_bindings_.emplace(&var, id);
  if (!inserted) {
    throw InternalError(
        "ProcessLowerer::DeclareProceduralVar: procedural variable symbol "
        "already mapped");
  }
  frame.OpenScope().declarations.push_back(id);
  return id;
}

void ProcessLowerer::DefineProceduralVar(
    hir::ProceduralBody& body, hir::ProceduralVarId id,
    const slang::ast::VariableSymbol& var, hir::TypeId type,
    std::optional<hir::ExprId> init) {
  body.procedural_vars.Define(
      id,
      hir::ProceduralVarDecl{
          .name = std::string{var.name},
          .type = type,
          .lifetime = var.lifetime == slang::ast::VariableLifetime::Automatic
                          ? hir::VariableLifetime::kAutomatic
                          : hir::VariableLifetime::kStatic,
          .lifetime_extended = lifetime_extended_.contains(&var),
          .init = init});
}

auto ProcessLowerer::AddProceduralVar(
    const WalkFrame& frame, hir::ProceduralBody& body,
    const slang::ast::VariableSymbol& var, hir::TypeId type,
    std::optional<hir::ExprId> init) -> hir::ProceduralVarId {
  const hir::ProceduralVarId id = DeclareProceduralVar(frame, body, var);
  DefineProceduralVar(body, id, var, type, init);
  return id;
}

auto ProcessLowerer::DeclarePerObjectConstants(
    const slang::ast::Scope& scope, const WalkFrame& frame)
    -> diag::Result<void> {
  hir::ProceduralBody& body = *frame.current_procedural_body;
  for (const auto& member : scope.members()) {
    const auto* constant = member.as_if<slang::ast::ParameterSymbol>();
    if (constant == nullptr ||
        !owner_->LookupProceduralStatic(*constant).has_value()) {
      continue;
    }
    const hir::ProceduralVarId id =
        DeclareProceduralVar(frame, body, *constant);

    const auto span = owner_->SourceMapper().PointSpanOf(constant->location);
    auto type = owner_->InternType(constant->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    const slang::ast::Expression* initializer = constant->getInitializer();
    if (initializer == nullptr) {
      throw InternalError(
          "ProcessLowerer::DeclarePerObjectConstants: a constant whose value "
          "differs per object states no expression it is written from");
    }
    auto init = LowerExpr(*initializer, frame);
    if (!init) return std::unexpected(std::move(init.error()));
    body.procedural_vars.Define(
        id, hir::ProceduralVarDecl{
                .name = std::string{constant->name},
                .type = *type,
                .lifetime = hir::VariableLifetime::kStatic,
                .lifetime_extended = false,
                .init = frame.Exprs().Add(*std::move(init))});
  }
  return {};
}

auto ProcessLowerer::LookupProceduralVar(const slang::ast::ValueSymbol& var)
    const -> std::optional<hir::ProceduralVarId> {
  const auto it = procedural_var_bindings_.find(&var);
  if (it == procedural_var_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

}  // namespace lyra::lowering::ast_to_hir
