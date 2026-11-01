#include "lyra/lowering/ast_to_hir/canonical_body.hpp"

#include <algorithm>
#include <format>
#include <ranges>
#include <string>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Where a member sits in its scope: the index the front end gave it, which
// numbers members in order but with room left for what elaboration inserts,
// so it is matched and never counted along; and its kind, since an implicit
// net shares the index of the member it was declared for.
struct MemberSite {
  slang::ast::SymbolIndex index;
  slang::ast::SymbolKind kind;
};

// The scope `symbol` is read as, on the way down a hierarchy: an instance as
// its body, and any other scope as itself.
auto ScopeOf(const slang::ast::Symbol& symbol) -> const slang::ast::Scope& {
  if (const auto* instance = symbol.as_if<slang::ast::InstanceSymbol>()) {
    return instance->body;
  }
  return symbol.as<slang::ast::Scope>();
}

// The instance standing at the same place under `body` as `inst` stands under
// `shared`: `inst` is below an occurrence the front end left pointing at
// `body`, and the front end elaborated the occurrence's own body only because
// a name reached into it, marking nothing below it as shared. Both bodies
// elaborated alike, so the member at each site on the way down -- an instance
// in its scope, a generate block in its body or its loop, an element in its
// array -- is the same declaration in both.
auto CounterpartUnder(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::InstanceBodySymbol& shared,
    const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol& {
  // A body is a member of nothing: the front end parents it to the scope
  // declaring its definition, and its place in the hierarchy is its instance's.
  std::vector<MemberSite> sites;
  for (const slang::ast::Symbol* at = &inst; at != &shared;) {
    if (const auto* enclosing = at->as_if<slang::ast::InstanceBodySymbol>()) {
      at = &InstantiationOf(*enclosing);
      continue;
    }
    sites.push_back(MemberSite{.index = at->getIndex(), .kind = at->kind});
    at = &at->getParentScope()->asSymbol();
  }
  const slang::ast::Symbol* found = &body;
  for (const MemberSite& site : std::views::reverse(sites)) {
    const auto members = ScopeOf(*found).members();
    const auto member =
        std::ranges::find_if(members, [&](const slang::ast::Symbol& candidate) {
          return candidate.getIndex() == site.index &&
                 candidate.kind == site.kind;
        });
    if (member == members.end()) {
      throw InternalError(
          std::format(
              "CounterpartUnder: '{}' stands under a body shared with '{}', "
              "which has no member at the same site",
              inst.getHierarchicalPath(),
              InstantiationOf(body).getHierarchicalPath()));
    }
    found = &*member;
  }
  return found->as<slang::ast::InstanceSymbol>();
}

// The nearest instance above `inst` in the hierarchy that the front end left
// pointing at another's body, or null where none is.
auto PointedAtInstanceAbove(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::InstanceSymbol* {
  for (const slang::ast::Scope* level = inst.getParentScope(); level != nullptr;
       level = level->asSymbol().getHierarchicalParent()) {
    const auto* body =
        level->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
    if (body == nullptr) continue;
    if (body->parentInstance != nullptr &&
        body->parentInstance->getCanonicalBody() != nullptr) {
      return body->parentInstance;
    }
  }
  return nullptr;
}

}  // namespace

auto CanonicalBodyOf(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::InstanceBodySymbol& {
  if (const slang::ast::InstanceBodySymbol* shared = inst.getCanonicalBody()) {
    return *shared;
  }
  if (const slang::ast::InstanceSymbol* above = PointedAtInstanceAbove(inst)) {
    return CanonicalBodyOf(
        CounterpartUnder(inst, above->body, *above->getCanonicalBody()));
  }
  return inst.body;
}

auto CanonicalInstanceOf(const slang::ast::InstanceSymbol& inst)
    -> const slang::ast::InstanceSymbol& {
  return InstantiationOf(CanonicalBodyOf(inst));
}

auto LoweringNameOf(const slang::ast::InstanceSymbol& inst) -> std::string {
  return CanonicalInstanceOf(inst).getHierarchicalPath();
}

}  // namespace lyra::lowering::ast_to_hir
