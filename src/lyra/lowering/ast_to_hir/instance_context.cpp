#include "lyra/lowering/ast_to_hir/instance_context.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// One instance as a path spells it: its name, or its array's name with the
// element's indices (LRM 23.3.2).
auto InstanceStep(const slang::ast::InstanceSymbol& inst) -> std::string {
  std::string step{inst.getArrayName()};
  for (const std::uint32_t at : inst.arrayPath) {
    step += std::format("[{}]", at);
  }
  return step;
}

// An instance a scope holds, with its path from that scope.
struct HeldInstance {
  const slang::ast::InstanceSymbol* instance;
  std::string path;
};

// Collects the instances standing in the scope it is handed the members of,
// through the generate blocks of that scope. A held instance's body is not
// entered.
struct HeldInstances
    : slang::ast::ASTVisitor<HeldInstances, slang::ast::VisitFlags::Symbols> {
  std::vector<HeldInstance> held;
  std::string blocks;

  void handle(const slang::ast::InstanceSymbol& inst) {
    held.push_back(
        HeldInstance{.instance = &inst, .path = blocks + InstanceStep(inst)});
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    if (block.isUninstantiated) return;
    const std::size_t outer = blocks.size();
    blocks += BlockInstancePathName(block);
    blocks += '.';
    visitDefault(block);
    blocks.resize(outer);
  }
};

// The instances standing in `scope`, an instance's body or a generate block,
// in the order the scope holds them.
auto InstancesHeldIn(const slang::ast::Scope& scope)
    -> std::vector<HeldInstance> {
  HeldInstances reader;
  for (const auto& member : scope.members()) {
    member.visit(reader);
  }
  return std::move(reader.held);
}

// Reads the names one instance's own body writes that leave it. What an
// instantiation hands an instance is written in the body holding it, so those
// expressions are read here and the held instance's body is not entered.
struct LeavingNames
    : slang::ast::ASTVisitor<LeavingNames, slang::ast::VisitFlags::AllGood> {
  explicit LeavingNames(const slang::ast::InstanceBodySymbol& body)
      : body(&body) {
  }

  const slang::ast::InstanceBodySymbol* body;
  std::vector<ClimbAnchor> climbs;

  void handle(const slang::ast::InstanceSymbol& inst) {
    inst.visitExprs(*this);
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    if (block.isUninstantiated) return;
    visitDefault(block);
  }

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Note(e.ref);
    visitDefault(e);
  }

  void handle(const slang::ast::ArbitrarySymbolExpression& e) {
    Note(e.hierRef);
    visitDefault(e);
  }

  void handle(const slang::ast::CallExpression& e) {
    Note(e.lookupInfo.hierRef);
    visitDefault(e);
  }

  void Note(const slang::ast::HierarchicalReference& ref) {
    if (auto climb = ClimbOutOf(ref, *body)) {
      climbs.push_back(*std::move(climb));
    }
  }
};

}  // namespace

auto InstanceContextOf(
    const slang::ast::InstanceSymbol& inst, InstanceContexts& known)
    -> const InstanceContext& {
  if (const auto kept = known.find(&inst); kept != known.end()) {
    return kept->second;
  }

  LeavingNames names(inst.body);
  std::vector<HeldInstance> holds;
  {
    const profiling::TimeTraceScope span(
        "read instance context", [&] { return std::string{inst.name}; });
    inst.body.visit(names);
    holds = InstancesHeldIn(inst.body);
  }

  InstanceContext context{.climbs = std::move(names.climbs), .below = {}};
  for (const HeldInstance& held : holds) {
    for (OverrideEffect& effect : OverridesOn(*held.instance)) {
      context.below.push_back(
          FixedBelow{.path = held.path, .what = std::move(effect)});
    }
    const InstanceContext& theirs = InstanceContextOf(*held.instance, known);
    // A name stops at the instance it lands in and concerns none above it.
    for (std::uint32_t written = 0; written < theirs.climbs.size(); ++written) {
      const ClimbAnchor& climb = theirs.climbs[written];
      if (climb.instance == &inst.body) continue;
      context.below.push_back(
          FixedBelow{
              .path = held.path,
              .what = NameLanding{
                  .written = written,
                  .scope = climb.scope,
                  .instance = climb.instance}});
    }
    for (const FixedBelow& fixed : theirs.below) {
      const bool lands_here = std::visit(
          Overloaded{
              [&](const NameLanding& name) {
                return name.instance == &inst.body;
              },
              [](const OverrideEffect&) { return false; }},
          fixed.what);
      if (lands_here) continue;
      context.below.push_back(
          FixedBelow{
              .path = std::format("{}.{}", held.path, fixed.path),
              .what = fixed.what});
    }
  }
  return known.emplace(&inst, std::move(context)).first->second;
}

auto OverridesBelow(
    const slang::ast::GenerateBlockSymbol& block, InstanceContexts& known)
    -> std::vector<OverriddenBelow> {
  std::vector<OverriddenBelow> overrides;
  for (const HeldInstance& held : InstancesHeldIn(block)) {
    for (OverrideEffect& effect : OverridesOn(*held.instance)) {
      overrides.push_back(
          OverriddenBelow{.path = held.path, .effect = std::move(effect)});
    }
    for (const FixedBelow& fixed :
         InstanceContextOf(*held.instance, known).below) {
      if (const auto* effect = std::get_if<OverrideEffect>(&fixed.what)) {
        overrides.push_back(
            OverriddenBelow{
                .path = std::format("{}.{}", held.path, fixed.path),
                .effect = *effect});
      }
    }
  }
  return overrides;
}

}  // namespace lyra::lowering::ast_to_hir
