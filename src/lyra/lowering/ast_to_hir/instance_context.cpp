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

// Reads one instance's own body: the names it writes that leave it, and the
// instances it holds, each with its path from the body. What an instantiation
// hands an instance is written in the body holding it, so those expressions
// are read here and the held instance's body is not entered.
struct BodyReader
    : slang::ast::ASTVisitor<BodyReader, slang::ast::VisitFlags::AllGood> {
  struct Held {
    const slang::ast::InstanceSymbol* instance;
    std::string path;
  };

  explicit BodyReader(const slang::ast::InstanceBodySymbol& body)
      : body(&body) {
  }

  const slang::ast::InstanceBodySymbol* body;
  std::vector<ClimbAnchor> climbs;
  std::vector<Held> held;
  std::string blocks;

  void handle(const slang::ast::InstanceSymbol& inst) {
    inst.visitExprs(*this);
    held.push_back(
        Held{.instance = &inst, .path = blocks + InstanceStep(inst)});
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    if (block.isUninstantiated) return;
    const std::size_t outer = blocks.size();
    blocks += GenerateBlockStep(block);
    blocks += '.';
    visitDefault(block);
    blocks.resize(outer);
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

  BodyReader reader(inst.body);
  {
    const profiling::TimeTraceScope span(
        "read instance context", [&] { return std::string{inst.name}; });
    inst.body.visit(reader);
  }

  InstanceContext context{.climbs = std::move(reader.climbs), .below = {}};
  for (const BodyReader::Held& held : reader.held) {
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

}  // namespace lyra::lowering::ast_to_hir
