#include "lyra/lowering/ast_to_hir/climb.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The body of the instance `scope` stands in, or nothing above the top.
auto InstanceBodyAround(const slang::ast::Scope* scope)
    -> const slang::ast::InstanceBodySymbol* {
  for (const slang::ast::Scope* at = scope; at != nullptr;
       at = at->asSymbol().getHierarchicalParent()) {
    if (const auto* body =
            at->asSymbol().as_if<slang::ast::InstanceBodySymbol>()) {
      return body;
    }
  }
  return nullptr;
}

// Whether `scope` is `above` or stands somewhere below it.
auto StandsIn(const slang::ast::Scope& scope, const slang::ast::Scope& above)
    -> bool {
  for (const slang::ast::Scope* at = &scope; at != nullptr;
       at = at->asSymbol().getHierarchicalParent()) {
    if (at == &above) return true;
  }
  return false;
}

// The body of the instance enclosing `body`, or nothing at the top.
auto EnclosingBody(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceBodySymbol* {
  const slang::ast::InstanceSymbol* instance = body.parentInstance;
  return instance == nullptr ? nullptr
                             : InstanceBodyAround(instance->getParentScope());
}

// Whether `scope` is `reader` or a scope inside it, as opposed to one of
// another instance. An instance's body is where one instance ends, so the walk
// stops at the first it meets.
auto Within(
    const slang::ast::Scope& scope,
    const slang::ast::InstanceBodySymbol& reader) -> bool {
  for (const slang::ast::Scope* at = &scope; at != nullptr;
       at = at->asSymbol().getHierarchicalParent()) {
    const slang::ast::Symbol& symbol = at->asSymbol();
    if (&symbol == &reader) return true;
    if (symbol.kind == slang::ast::SymbolKind::InstanceBody) return false;
  }
  return false;
}

// Whether `body` is `reader` or the body of an instance `reader` stands in.
auto Encloses(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::InstanceBodySymbol& reader) -> bool {
  for (const slang::ast::InstanceBodySymbol* at = &reader; at != nullptr;
       at = EnclosingBody(*at)) {
    if (at == &body) return true;
  }
  return false;
}

// Every hierarchical name a body writes, in source order, stopping at a child
// instance's body.
struct EveryHierarchicalName
    : slang::ast::ASTVisitor<
          EveryHierarchicalName, slang::ast::VisitFlags::AllGood> {
  std::vector<const slang::ast::HierarchicalReference*> found;

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    found.push_back(&e.ref);
    visitDefault(e);
  }

  void handle(const slang::ast::ArbitrarySymbolExpression& e) {
    found.push_back(&e.hierRef);
    visitDefault(e);
  }

  void handle(const slang::ast::CallExpression& e) {
    found.push_back(&e.lookupInfo.hierRef);
    visitDefault(e);
  }

  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
  }
};

// The scopes the classes `ref` passes through belong to (LRM 6.22): the class
// of each value its path holds a handle of, and the class each member it names
// is declared in.
auto ClassScopesAlong(const slang::ast::HierarchicalReference& ref)
    -> std::vector<const slang::ast::Scope*> {
  std::vector<const slang::ast::Scope*> scopes;
  const auto note = [&](const slang::ast::ClassType& cls) {
    const slang::ast::Scope* scope = &ReplicatingScope(cls);
    if (std::ranges::find(scopes, scope) == scopes.end()) {
      scopes.push_back(scope);
    }
  };
  for (const auto& element : ref.path) {
    const slang::ast::Symbol& symbol = *element.symbol;
    if (const auto* value = symbol.as_if<slang::ast::ValueSymbol>()) {
      if (const auto* cls = value->getType()
                                .getCanonicalType()
                                .as_if<slang::ast::ClassType>()) {
        note(*cls);
      }
    }
    if (const slang::ast::Scope* owner = symbol.getParentScope()) {
      if (const auto* cls = owner->asSymbol().as_if<slang::ast::ClassType>()) {
        note(*cls);
      }
    }
  }
  return scopes;
}

// The scope `ref`, written in `reader`, lands in once it leaves `reader`, or
// nothing when it never does.
auto LandingScope(
    const slang::ast::HierarchicalReference& ref,
    const slang::ast::InstanceBodySymbol& reader) -> const slang::ast::Scope* {
  if (ref.path.empty() || ref.isViaIfacePort()) return nullptr;
  const slang::ast::Symbol& head = *ref.path.front().symbol;
  // The upward search matches only a scope or an instance (LRM 23.8); a name
  // that starts at a value -- a virtual interface, a class handle -- reaches
  // through whatever that value holds and climbs nowhere.
  if (head.kind != slang::ast::SymbolKind::Root &&
      head.kind != slang::ast::SymbolKind::Instance &&
      head.kind != slang::ast::SymbolKind::InstanceArray && !head.isScope()) {
    return nullptr;
  }

  // A path written from `$root` starts at the top-level instance it names
  // next (LRM 23.6).
  if (head.kind == slang::ast::SymbolKind::Root) {
    if (ref.path.size() < 2) return nullptr;
    const auto* top = ref.path[1].symbol->as_if<slang::ast::InstanceSymbol>();
    if (top == nullptr || &top->body == &reader) return nullptr;
    return &top->body;
  }

  // The search matched the definition name of an instance the reader stands
  // in, or the name of a top-level instance, and continues from that instance
  // (LRM 23.8).
  if (const auto* instance = head.as_if<slang::ast::InstanceSymbol>()) {
    const slang::ast::Scope* holder = instance->getParentScope();
    const bool top_level =
        holder != nullptr &&
        holder->asSymbol().kind == slang::ast::SymbolKind::Root;
    if (top_level || Encloses(instance->body, reader)) {
      if (&instance->body == &reader) return nullptr;
      return &instance->body;
    }
  }

  // The search found the name in a scope enclosing the reader's instance,
  // and continues from that scope.
  const slang::ast::Scope* found_in = head.getHierarchicalParent();
  if (found_in == nullptr || Within(*found_in, reader)) return nullptr;
  const slang::ast::SymbolKind kind = found_in->asSymbol().kind;
  if (kind != slang::ast::SymbolKind::InstanceBody &&
      kind != slang::ast::SymbolKind::GenerateBlock) {
    return nullptr;
  }
  return found_in;
}

}  // namespace

auto ClimbOutOf(
    const slang::ast::HierarchicalReference& reference,
    const slang::ast::InstanceBodySymbol& reader)
    -> std::optional<ClimbAnchor> {
  const slang::ast::Scope* landed = LandingScope(reference, reader);
  if (landed == nullptr) return std::nullopt;
  const slang::ast::InstanceBodySymbol* instance = InstanceBodyAround(landed);
  if (instance == nullptr) {
    throw InternalError(
        "ClimbOutOf: a name lands in an instance's body or a generate block "
        "inside one");
  }
  return ClimbAnchor{
      .scope = landed,
      .instance = instance,
      .carries = ClassScopesAlong(reference)};
}

auto ClimbsOutOf(const slang::ast::InstanceBodySymbol& reader)
    -> std::vector<ClimbAnchor> {
  EveryHierarchicalName names;
  reader.visit(names);
  std::vector<ClimbAnchor> climbs;
  for (const slang::ast::HierarchicalReference* ref : names.found) {
    if (auto climb = ClimbOutOf(*ref, reader)) {
      climbs.push_back(*std::move(climb));
    }
  }
  return climbs;
}

auto StartOfReach(
    const slang::ast::Scope& target,
    const slang::ast::InstanceBodySymbol& reader,
    std::span<const ClimbAnchor> climbs) -> std::optional<ReachStart> {
  if (StandsIn(target, reader)) return FromReader{};
  for (const ClimbAnchor& climb : climbs) {
    if (std::ranges::find(climb.carries, &target) != climb.carries.end()) {
      return FromInstance{.body = climb.instance};
    }
  }
  const slang::ast::InstanceBodySymbol* holder = InstanceBodyAround(&target);
  if (holder != nullptr && Encloses(*holder, reader)) {
    return FromInstance{.body = holder};
  }
  return std::nullopt;
}

}  // namespace lyra::lowering::ast_to_hir
