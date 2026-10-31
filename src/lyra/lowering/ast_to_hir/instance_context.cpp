#include "lyra/lowering/ast_to_hir/instance_context.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
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

// Reads what one instance's own body reaches outside itself: every name it
// writes and where each lands, the classes another instance declares that its
// types hold, and the parameters of another instance it reads. It visits
// everything the body's lowering translates, so the lowering finds each of
// these here. What an instantiation hands an instance is written in the body
// holding it, so those expressions are read here and the held instance's body
// is not entered.
struct ReachedOutside
    : slang::ast::ASTVisitor<ReachedOutside, slang::ast::VisitFlags::AllGood> {
  explicit ReachedOutside(const slang::ast::InstanceBodySymbol& body)
      : body(&body) {
  }

  const slang::ast::InstanceBodySymbol* body;
  std::vector<NameUse> uses;
  std::vector<const slang::ast::Scope*> class_scopes;
  std::vector<const slang::ast::ParameterSymbol*> parameters;

  // What the instantiation writes for a child is this body's text: its
  // connections, and the values it assigns the child's parameters, which the
  // front end keeps under the child's own parameters (LRM 23.10.2).
  void handle(const slang::ast::InstanceSymbol& inst) {
    inst.visitExprs(*this);
    for (const slang::ast::ParameterSymbolBase* base :
         inst.body.getParameters()) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr) continue;
      if (const slang::ast::Expression* written =
              ValueWrittenAtInstantiation(inst, *param)) {
        written->visit(*this);
      }
    }
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    if (block.isUninstantiated) return;
    visitDefault(block);
  }

  // A loop's scheme is run by the construction as written (LRM 27.4), which
  // the front end's walk does not visit.
  void handle(const slang::ast::GenerateBlockArraySymbol& loop) {
    for (const slang::ast::Expression* scheme :
         {loop.initialExpression, loop.stopExpression, loop.iterExpression}) {
      if (scheme != nullptr) scheme->visit(*this);
    }
    visitDefault(loop);
  }

  // A name a view defines with an expression of its own (LRM 25.5.4) is
  // computed in the scope declaring the view.
  void handle(const slang::ast::ModportPortSymbol& port) {
    if (port.explicitConnection != nullptr) {
      port.explicitConnection->visit(*this);
    }
    visitDefault(port);
  }

  // A value an instantiation hands a parameter is written in the body holding
  // the instance and is read there (LRM 23.10), whatever scope the front end
  // keeps it under.
  void handle(const slang::ast::ParameterSymbol& param) {
    NoteType(param.getType());
    if (param.isOverridden()) return;
    visitDefault(param);
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    NoteType(*e.type);
    Note(e.symbol, {});
    visitDefault(e);
  }

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    NoteType(*e.type);
    Note(e.symbol, e.ref);
    visitDefault(e);
  }

  void handle(const slang::ast::ArbitrarySymbolExpression& e) {
    NoteType(*e.type);
    Note(*e.symbol, e.hierRef);
    visitDefault(e);
  }

  // A disable names its target as a name like any other (LRM 9.6.2), which the
  // front end's walk does not visit, so it is visited here.
  void handle(const slang::ast::DisableStatement& s) {
    s.target.visit(*this);
  }

  // A sequence or property reached by a hierarchical name leaves the instance
  // the way any other name does, and what its body names beside it is reached
  // from where that name landed (LRM 16.8).
  void handle(const slang::ast::AssertionInstanceExpression& e) {
    NoteType(*e.type);
    if (e.hierRef.target != nullptr) Note(e.symbol, e.hierRef);
    visitDefault(e);
  }

  void handle(const slang::ast::CallExpression& e) {
    NoteType(*e.type);
    if (const auto* const* subroutine =
            std::get_if<const slang::ast::SubroutineSymbol*>(&e.subroutine)) {
      Note(**subroutine, e.lookupInfo.hierRef);
    }
    visitDefault(e);
  }

  // Every other node: the type a declaration states or an expression has.
  template <typename T>
  void handle(const T& node) {
    if constexpr (requires { node.getType(); }) {
      NoteType(node.getType());
    }
    if constexpr (requires { node.type; }) {
      if (node.type != nullptr) NoteType(*node.type);
    }
    visitDefault(node);
  }

  void Note(
      const slang::ast::Symbol& found,
      const slang::ast::HierarchicalReference& written) {
    NoteDeclaration(found);
    std::vector<const slang::ast::Symbol*> path;
    path.reserve(written.path.size());
    for (const auto& element : written.path) {
      path.push_back(element.symbol);
    }
    uses.push_back(
        NameUse{
            .found = &found,
            .path = std::move(path),
            .landing = ClimbOutOf(found, written, *body)});
  }

  // A parameter another instance declares, and the class a member another
  // instance's class declares belongs to.
  void NoteDeclaration(const slang::ast::Symbol& found) {
    const slang::ast::Scope* holder = found.getParentScope();
    if (holder == nullptr) return;
    if (const auto* param = found.as_if<slang::ast::ParameterSymbol>();
        param != nullptr && InAnotherInstance(*holder) &&
        !std::ranges::contains(parameters, param)) {
      parameters.push_back(param);
    }
    if (const auto* cls = holder->asSymbol().as_if<slang::ast::ClassType>()) {
      NoteType(*cls);
    }
  }

  void NoteType(const slang::ast::Type& type) {
    const slang::ast::Type& canonical = type.getCanonicalType();
    if (canonical.isClass() || canonical.isCovergroup()) {
      const slang::ast::Scope& scope = ReplicatingScope(canonical);
      if (InAnotherInstance(scope) &&
          !std::ranges::contains(class_scopes, &scope)) {
        class_scopes.push_back(&scope);
      }
      if (const auto* cls = canonical.as_if<slang::ast::ClassType>()) {
        if (const slang::ast::Type* base = cls->getBaseClass()) {
          NoteType(*base);
        }
      }
      return;
    }
    if (const slang::ast::Type* element = canonical.getArrayElementType()) {
      NoteType(*element);
    }
    if (const auto* assoc = canonical.as_if<slang::ast::AssociativeArrayType>();
        assoc != nullptr && assoc->indexType != nullptr) {
      NoteType(*assoc->indexType);
    }
    if (const auto* record =
            canonical.as_if<slang::ast::UnpackedStructType>()) {
      for (const slang::ast::FieldSymbol* field : record->fields) {
        NoteType(field->getType());
      }
    }
  }

  // Whether `scope` stands in an instance other than the one whose body is
  // walked; a namespace's scope stands in none.
  [[nodiscard]] auto InAnotherInstance(const slang::ast::Scope& scope) const
      -> bool {
    const slang::ast::InstanceBodySymbol* holding =
        scope.getContainingInstance();
    return holding != nullptr && holding != body;
  }
};

// Adds what is written elsewhere about `held` and about each instance below
// it, the second known to `held` as `theirs`, each under its path from the
// scope holding `held`.
void AppendOverridesAtAndBelow(
    const HeldInstance& held, const InstanceContext& theirs,
    std::vector<OverriddenBelow>& overridden) {
  for (OverrideEffect& effect : OverridesOn(*held.instance)) {
    overridden.push_back(
        OverriddenBelow{.path = held.path, .effect = std::move(effect)});
  }
  for (const OverriddenBelow& below : theirs.overridden_below) {
    overridden.push_back(
        OverriddenBelow{
            .path = std::format("{}.{}", held.path, below.path),
            .effect = below.effect});
  }
}

}  // namespace

auto InstanceContext::Climbs() const -> std::vector<ClimbAnchor> {
  std::vector<ClimbAnchor> climbs;
  for (const NameUse& use : uses) {
    if (use.landing.has_value()) climbs.push_back(*use.landing);
  }
  return climbs;
}

auto InstanceContextOf(
    const slang::ast::InstanceSymbol& inst, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own) -> const InstanceContext& {
  if (const auto kept = known.find(&inst); kept != known.end()) {
    return kept->second;
  }
  if (!has_body_of_its_own(inst)) {
    // The front end shares a body only between instances every name leaving
    // it resolves alike from, so what the shared body states holds for this
    // instance too.
    return InstanceContextOf(
        *inst.getCanonicalBody()->parentInstance, known, has_body_of_its_own);
  }

  ReachedOutside reached(inst.body);
  std::vector<HeldInstance> holds;
  {
    const profiling::TimeTraceScope span(
        "read instance context", [&] { return std::string{inst.name}; });
    inst.body.visit(reached);
    holds = InstancesHeldIn(inst.body);
  }

  InstanceContext context{
      .uses = std::move(reached.uses),
      .class_scopes = std::move(reached.class_scopes),
      .foreign_parameters = std::move(reached.parameters),
      .names_below = {},
      .overridden_below = {}};
  for (const HeldInstance& held : holds) {
    const InstanceContext& theirs =
        InstanceContextOf(*held.instance, known, has_body_of_its_own);
    // A name stops at the instance it lands in and concerns none above it.
    const std::vector<ClimbAnchor> their_climbs = theirs.Climbs();
    for (std::uint32_t written = 0; written < their_climbs.size(); ++written) {
      const ClimbAnchor& climb = their_climbs[written];
      if (climb.instance == &inst.body) continue;
      context.names_below.push_back(
          NameLandingBelow{
              .path = held.path,
              .written = written,
              .scope = climb.scope,
              .blocks_passed = climb.blocks_passed,
              .instance = climb.instance,
              .reads = climb.reads});
    }
    for (const NameLandingBelow& name : theirs.names_below) {
      if (name.instance == &inst.body) continue;
      context.names_below.push_back(
          NameLandingBelow{
              .path = std::format("{}.{}", held.path, name.path),
              .written = name.written,
              .scope = name.scope,
              .blocks_passed = name.blocks_passed,
              .instance = name.instance,
              .reads = name.reads});
    }
    AppendOverridesAtAndBelow(held, theirs, context.overridden_below);
  }
  return known.emplace(&inst, std::move(context)).first->second;
}

auto OverridesBelow(
    const slang::ast::GenerateBlockSymbol& block, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own)
    -> std::vector<OverriddenBelow> {
  std::vector<OverriddenBelow> overrides;
  for (const HeldInstance& held : InstancesHeldIn(block)) {
    AppendOverridesAtAndBelow(
        held, InstanceContextOf(*held.instance, known, has_body_of_its_own),
        overrides);
  }
  return overrides;
}

}  // namespace lyra::lowering::ast_to_hir
