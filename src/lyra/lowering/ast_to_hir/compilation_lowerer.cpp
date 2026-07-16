#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Collects the distinct unit bodies reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy. The canonical body keys the dedup so a
// module instantiated many times is lowered once.
struct UnitCollector
    : slang::ast::ASTVisitor<UnitCollector, slang::ast::VisitFlags::Canonical> {
  std::unordered_set<const slang::ast::InstanceBodySymbol*> seen;
  std::vector<const slang::ast::InstanceBodySymbol*> order;

  void handle(const slang::ast::InstanceSymbol& inst) {
    const auto* canonical = inst.getCanonicalBody();
    const auto& body = canonical != nullptr ? *canonical : inst.body;
    if (seen.insert(&body).second) {
      order.push_back(&body);
      visitDefault(inst);
    }
  }
};

}  // namespace

// A DPI-C export (LRM 35.5) makes an SV subroutine callable from C. Lyra emits
// no C-export ABI, so an exported subroutine cannot be honored; rejecting it is
// what keeps it from lowering as an ordinary subroutine that silently drops the
// export contract. slang collects exports only from elaborated scopes, so every
// entry belongs to the design under compilation.
auto RejectDpiExports(const LowerCompilationFacts& facts)
    -> diag::Result<void> {
  for (const auto& dpi : facts.Compilation().getDPIExports()) {
    if (dpi.subroutine == nullptr) continue;
    return diag::Fail(
        facts.SourceMapper().PointSpanOf(dpi.subroutine->location),
        diag::DiagCode::kUnsupportedDpi,
        "DPI-C export of a subroutine is not yet supported");
  }
  return {};
}

auto CollectUnitBodies(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::InstanceBodySymbol*> {
  const auto& root = facts.Compilation().getRoot();
  UnitCollector collector;
  for (const auto* top : root.topInstances) {
    top->visit(collector);
  }
  return std::move(collector.order);
}

auto LowerUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::InstanceBodySymbol& body)
    -> diag::Result<hir::CompilationUnit> {
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), facts.DisableAssertions());
  UnitLowerer lowerer(unit_facts, body, SpecializationName(body));
  return lowerer.Run();
}

auto CollectPackages(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::PackageSymbol*> {
  return facts.Compilation().getPackages();
}

auto LowerPackageUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::PackageSymbol& package)
    -> diag::Result<hir::CompilationUnit> {
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), facts.DisableAssertions());
  UnitLowerer lowerer(unit_facts, package, std::string{package.name});
  return lowerer.Run();
}

auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string> {
  const auto& root = compilation.getRoot();
  std::vector<std::string> names;
  names.reserve(root.topInstances.size());
  for (const auto* inst : root.topInstances) {
    names.emplace_back(SpecializationName(*inst));
  }
  return names;
}

}  // namespace lyra::lowering::ast_to_hir
