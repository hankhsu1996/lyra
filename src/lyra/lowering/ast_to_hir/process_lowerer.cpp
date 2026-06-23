#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

#include <algorithm>
#include <expected>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
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

// LRM 9.2.2.2.1: a symbol is local to `proc` if it is an automatic-lifetime
// variable, or if it is declared inside one of `proc`'s statement blocks.
// `ProceduralBlockSymbol` itself is not a `Scope`, so its statement blocks
// hang off the surrounding scope -- we walk the symbol's parent-scope chain
// up to the outermost `StatementBlockSymbol` and check whether that block is
// one of `proc.getBlocks()`.
auto IsLocalTo(
    const slang::ast::ValueSymbol& sym,
    const slang::ast::ProceduralBlockSymbol& proc) -> bool {
  if (const auto* var = sym.as_if<slang::ast::VariableSymbol>();
      var != nullptr &&
      var->lifetime == slang::ast::VariableLifetime::Automatic) {
    return true;
  }
  const slang::ast::Scope* scope = sym.getParentScope();
  const slang::ast::StatementBlockSymbol* root_block = nullptr;
  while (scope != nullptr &&
         scope->asSymbol().kind == slang::ast::SymbolKind::StatementBlock) {
    root_block = &scope->asSymbol().as<slang::ast::StatementBlockSymbol>();
    scope = root_block->getParentScope();
  }
  if (root_block == nullptr) return false;
  const auto blocks = proc.getBlocks();
  return std::ranges::any_of(
      blocks, [&](const auto* block) { return block == root_block; });
}

}  // namespace

ProcessLowerer::ProcessLowerer(
    ModuleLowerer& module, const slang::ast::Symbol& containing_symbol)
    : module_(&module), containing_symbol_(&containing_symbol) {
}

auto ProcessLowerer::Run(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame parent_frame)
    -> diag::Result<hir::Process> {
  hir::ProceduralBody body;
  const WalkFrame frame = parent_frame.WithProceduralBody(&body, &body.exprs);

  auto root_stmt = LowerStmt(proc.getBody(), frame);
  if (!root_stmt) return std::unexpected(std::move(root_stmt.error()));
  body.root_stmt = body.stmts.Add(*std::move(root_stmt));

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
    // LRM 9.2.2.2.1: implicit sensitivity is a property of the always_comb /
    // always_latch procedure itself (no `@*` token in source). Compute the
    // procedure-body read set excluding locals.
    const auto& raw = module_->Sensitivity().AnalyzeReads(proc.getBody(), proc);
    std::vector<SensitivityRead> filtered;
    filtered.reserve(raw.size());
    for (const auto& read : raw) {
      if (!IsLocalTo(*read.symbol, proc)) {
        filtered.push_back(read);
      }
    }
    out.implicit_sensitivity_list =
        module_->TranslateSensitivityReads(filtered, frame);
  }
  return out;
}

auto ProcessLowerer::AddProceduralVar(
    hir::ProceduralBody& body, const slang::ast::VariableSymbol& var,
    hir::TypeId type) -> hir::ProceduralVarId {
  const hir::ProceduralVarId id = body.procedural_vars.Add(
      hir::ProceduralVarDecl{
          .name = std::string{var.name},
          .type = type,
          .lifetime = var.lifetime == slang::ast::VariableLifetime::Automatic
                          ? hir::VariableLifetime::kAutomatic
                          : hir::VariableLifetime::kStatic});
  const auto [_, inserted] = procedural_var_bindings_.emplace(&var, id);
  if (!inserted) {
    throw InternalError(
        "ProcessLowerer::AddProceduralVar: procedural variable symbol "
        "already mapped");
  }
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
