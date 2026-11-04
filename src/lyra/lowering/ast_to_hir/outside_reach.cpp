#include "lyra/lowering/ast_to_hir/outside_reach.hpp"

#include <algorithm>
#include <iterator>
#include <optional>
#include <string>
#include <unordered_set>
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

#include "lyra/lowering/ast_to_hir/canonical_body.hpp"
#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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
  std::unordered_set<const slang::ast::ClassType*> classes_noted;

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

  // A class the body declares reaches what its declaration names, whether or
  // not anything in the body names the class (LRM 8.13, 8.26.2).
  void handle(const slang::ast::ClassType& cls) {
    NoteType(cls);
    visitDefault(cls);
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
      // A class reaches the instance of what it extends and of each interface
      // class it names (LRM 6.22, 8.26.2), which may be another's. Interface
      // classes may extend one another along many paths to one (LRM
      // 8.26.6.3), so each class is gone into once.
      if (const auto* cls = canonical.as_if<slang::ast::ClassType>();
          cls != nullptr && classes_noted.insert(cls).second) {
        if (const slang::ast::Type* base = cls->getBaseClass()) {
          NoteType(*base);
        }
        for (const slang::ast::Type* iface : cls->getDeclaredInterfaces()) {
          NoteType(*iface);
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

}  // namespace

auto InstancesHeldIn(const slang::ast::Scope& scope)
    -> std::vector<const slang::ast::InstanceSymbol*> {
  std::vector<const slang::ast::InstanceSymbol*> held;
  for (const slang::ast::Symbol& member : scope.members()) {
    if (const auto* inst = member.as_if<slang::ast::InstanceSymbol>()) {
      held.push_back(inst);
    } else if (
        const auto* array = member.as_if<slang::ast::InstanceArraySymbol>()) {
      std::ranges::move(InstancesHeldIn(*array), std::back_inserter(held));
    } else if (
        const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>()) {
      if (!block->isUninstantiated) {
        std::ranges::move(InstancesHeldIn(*block), std::back_inserter(held));
      }
    } else if (
        const auto* loop =
            member.as_if<slang::ast::GenerateBlockArraySymbol>()) {
      for (const slang::ast::GenerateBlockSymbol* entry : loop->entries) {
        std::ranges::move(InstancesHeldIn(*entry), std::back_inserter(held));
      }
    }
  }
  return held;
}

auto OutsideReach::Climbs() const -> std::vector<ClimbAnchor> {
  std::vector<ClimbAnchor> climbs;
  for (const NameUse& use : uses) {
    if (use.landing.has_value()) climbs.push_back(*use.landing);
  }
  return climbs;
}

auto OutsideReachOf(
    const slang::ast::InstanceSymbol& inst, OutsideReaches& known)
    -> const OutsideReach& {
  // The front end shares a body only between instances every name leaving it
  // resolves alike from, so what the shared body states holds for every
  // instance read through it, and is kept under the instance it was
  // elaborated for.
  const slang::ast::InstanceSymbol& own = CanonicalInstanceOf(inst);
  if (const auto kept = known.find(&own); kept != known.end()) {
    return kept->second;
  }

  ReachedOutside reached(own.body);
  std::vector<const slang::ast::InstanceSymbol*> holds;
  {
    const profiling::TimeTraceScope span(
        "read instance context", [&] { return std::string{own.name}; });
    own.body.visit(reached);
    holds = InstancesHeldIn(own.body);
  }

  OutsideReach context{
      .uses = std::move(reached.uses),
      .class_scopes = std::move(reached.class_scopes),
      .foreign_parameters = std::move(reached.parameters),
      .names_below = {}};
  for (const slang::ast::InstanceSymbol* held : holds) {
    const OutsideReach& theirs = OutsideReachOf(*held, known);
    // A name stops at the instance it lands in and concerns none above it.
    for (const ClimbAnchor& climb : theirs.Climbs()) {
      if (climb.instance != &own.body) context.names_below.push_back(climb);
    }
    for (const ClimbAnchor& name : theirs.names_below) {
      if (name.instance != &own.body) context.names_below.push_back(name);
    }
  }
  return known.emplace(&own, std::move(context)).first->second;
}

}  // namespace lyra::lowering::ast_to_hir
