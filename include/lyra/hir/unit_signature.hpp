#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/class_registry.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/port_direction.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/scope_class_id.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// A port part carrying data across the boundary: which way it flows, the type
// of what crosses, and what inside this unit's instance it reaches. The type is
// the port expression's own self-determined type (LRM 23.2.2.2), which is
// narrower than the storage behind it whenever that expression names part of an
// internal name.
struct DataPortPart {
  PortDirection direction{};
  TypeId type{};
  ConnectionTarget target;

  auto operator==(const DataPortPart&) const -> bool = default;
};

// A port part naming a scope rather than carrying data -- an interface port
// (LRM 25.3). Nothing flows across it in any direction, so it has no direction
// and no value type; what crosses is which member of this unit's instance the
// connection binds, and the type of that member says which unit's objects it
// must be bound to and how many of them.
struct InterfacePortPart {
  PublishedMemberId member;

  auto operator==(const InterfacePortPart&) const -> bool = default;
};

// One point of a port that a connection reaches individually. A port bundling
// several internal names (LRM 23.2.2.1) carries data separately across each,
// under directions and types that need not agree, so it has one part per
// bundled name; every other port has exactly one, which is the same shape with
// one entry.
using PortPart = std::variant<DataPortPart, InterfacePortPart>;

// One port a unit publishes (LRM 23.2.2). `name` is the external name -- what
// another unit connects to, which the LRM lets differ from the name of
// whatever the port reaches inside the unit, so what is published is the port
// and never the declaration behind it.
struct PortDecl {
  std::string name;
  // Least significant first, since LRM 23.2.2.1 gives the first bundled name
  // written the most significant bits and a connection reaches them in bit
  // order.
  std::vector<PortPart> parts;
  // What an instance leaving the port unconnected receives (LRM 23.2.2.4). The
  // default is an expression of this unit, evaluated in its own scope and not
  // in the instantiator's, so what crosses is the subroutine that evaluates it.
  // Only an input port declared in the header may have one.
  std::optional<PublishedCallableId> default_value;

  auto operator==(const PortDecl&) const -> bool = default;
};

// What a design element (LRM 23.2.1) publishes beside its classes, which exists
// to be instantiated and wired: its ports, what an instance is handed when it
// is built, the object an instance of it is, and the class of every generate
// block an instance holds.
struct PublishedDesignElement {
  // In declaration order, which is the order a positional connection counts
  // through (LRM 23.3.2.1). A consumer walking a unit's connections walks these
  // parts in step with them rather than searching for each, so the two cannot
  // disagree about which point is which.
  std::vector<PortDecl> ports;
  // The type of each parameter an instance is handed when it is built (LRM
  // 23.10), in the order its object entry takes them: what an entry another
  // unit names is declared with, as a class's constructor states what a
  // construction of it is entered with.
  std::vector<TypeId> handed;
  ScopeClassSignature instance_class;
  // However deep (LRM 27), each reached from the scope holding its construct.
  std::vector<ScopeClassSignature> blocks;

  auto operator==(const PublishedDesignElement&) const -> bool = default;

  // The class of a scope an instance of this unit is or holds, published under
  // `class_path`, or nothing where the unit published no such scope.
  [[nodiscard]] auto FindScopeClass(const support::DefPath& class_path) const
      -> const ScopeClassSignature* {
    if (instance_class.class_path == class_path) return &instance_class;
    for (const ScopeClassSignature& block : blocks) {
      if (block.class_path == class_path) return &block;
    }
    return nullptr;
  }
};

// What a namespace unit -- a package (LRM 26.2) or the `$unit` scope (LRM
// 3.12.1) -- publishes beside its classes: the subroutines another unit calls
// by name on no object (LRM 26.3), what a call to each passes and awaits. It
// roots no object, so nothing reaches it through a receiver.
struct PublishedNamespace {
  std::vector<SubroutinePrototype> subroutines;

  auto operator==(const PublishedNamespace&) const -> bool = default;

  // The subroutine published under `name`, or nothing where the unit published
  // no such name.
  [[nodiscard]] auto FindSubroutine(std::string_view name) const
      -> const SubroutinePrototype* {
    for (const SubroutinePrototype& published : subroutines) {
      if (published.name == name) return &published;
    }
    return nullptr;
  }
};

// What a unit publishes: the declarations another unit may name. Derived by the
// unit from its own declarations alone, so nothing it states can contradict
// what the unit is, and nothing about any other unit is needed to produce it --
// which is what lets every unit's be derived at once, in any order.
//
// A unit that publishes nothing has an empty signature rather than none.
struct UnitSignature {
  std::string unit_name;
  // The types the published declarations name, held here rather than named in
  // the publishing unit's pool: a signature is read where that unit's arenas
  // are not, so an identity on one has to index storage the signature carries.
  TypePool types;
  // Every class anything here names, under the pair every unit names it by. A
  // class named in here is named by its position in this list: an identity of
  // the publishing unit means nothing where the signature is read, so the
  // signature carries its own, and a reader maps each to one of its own in one
  // step, as rustc remaps the crate numbers of the metadata it decodes.
  base::Interner<ClassKey, ClassId, ClassKey::Hash> class_keys;
  // The same for every covergroup anything here names.
  base::Interner<CovergroupKey, CovergroupId, CovergroupKey::Hash>
      covergroup_keys;
  // The same for every scope class anything here names.
  base::Interner<ScopeClassKey, ScopeClassId, ScopeClassKey::Hash>
      scope_class_keys;
  // The classes of the source language this unit declares and other units may
  // name (LRM 26.2), each reached by its own name rather than through any
  // instance.
  std::vector<ClassDeclaration> classes;
  // The covergroups this unit declares and other units may name (LRM 19.3,
  // 26.2), each by its own path, as a class is.
  std::vector<PublishedCovergroup> covergroups;
  std::variant<PublishedDesignElement, PublishedNamespace> unit;

  auto operator==(const UnitSignature&) const -> bool = default;

  // The covergroup published under `path`, or nothing where the unit published
  // no such covergroup.
  [[nodiscard]] auto FindCovergroup(const support::DefPath& path) const
      -> const PublishedCovergroup* {
    for (const PublishedCovergroup& published : covergroups) {
      if (published.path == path) return &published;
    }
    return nullptr;
  }

  // The pair the scope class named `id` here is named by everywhere.
  [[nodiscard]] auto ScopeClassKeyOf(ScopeClassId id) const
      -> const ScopeClassKey& {
    return scope_class_keys.Get(id);
  }

  // The identity `key` has here, taken the first time it is named.
  auto ScopeClassIdOf(const ScopeClassKey& key) -> ScopeClassId {
    return scope_class_keys.Intern(key);
  }

  // The pair the covergroup named `id` here is named by everywhere.
  [[nodiscard]] auto CovergroupKeyOf(CovergroupId id) const
      -> const CovergroupKey& {
    return covergroup_keys.Get(id);
  }

  // The identity `key` has here, taken the first time it is named.
  auto CovergroupIdOf(const CovergroupKey& key) -> CovergroupId {
    return covergroup_keys.Intern(key);
  }

  // The pair the class named `id` here is named by everywhere.
  [[nodiscard]] auto ClassKeyOf(ClassId id) const -> const ClassKey& {
    return class_keys.Get(id);
  }

  // The identity `key` has here, taken the first time it is named.
  auto ClassIdOf(const ClassKey& key) -> ClassId {
    return class_keys.Intern(key);
  }

  // The class published under `class_path`, or nothing where the unit published
  // no such class.
  [[nodiscard]] auto FindClass(const support::DefPath& class_path) const
      -> const ClassDeclaration* {
    for (const ClassDeclaration& published : classes) {
      if (published.class_path == class_path) return &published;
    }
    return nullptr;
  }
};

// What `signature` publishes as a design element. A unit that is instantiated
// is one, so a caller holding the signature of a unit it instantiates reaches
// it without a case for a namespace.
[[nodiscard]] inline auto DesignElementOf(const UnitSignature& signature)
    -> const PublishedDesignElement& {
  const auto* element = std::get_if<PublishedDesignElement>(&signature.unit);
  if (element == nullptr) {
    throw InternalError(
        "hir::DesignElementOf: a unit that is instantiated is a design "
        "element");
  }
  return *element;
}

// An importer taking `signature`'s types into a reader's pool `into`, naming
// every class and covergroup the way the reader's registries do: an identity
// on a signature indexes the lists the signature carries, so each is answered
// again by the reader, once.
[[nodiscard]] auto ReaderImporter(
    const UnitSignature& signature, TypePool& into, ClassRegistry& classes,
    CovergroupRegistry& covergroups, ScopeClassRegistry& scope_classes,
    TypeImportMemo& memo) -> TypeImporter;

// What `declared` is in the context `importer` takes it into: the same
// declaration with every type and every identity in it answered again there.
// Publishing a class and reading one back are this one step, in opposite
// directions.
[[nodiscard]] auto ImportClassDeclaration(
    TypeImporter& importer, const ClassDeclaration& declared)
    -> ClassDeclaration;

// `interface` and `callable` with every type they name taken into the pool
// `importer` writes, which is the part of each that cannot cross from one pool
// to another as it stands.
[[nodiscard]] auto ImportCalleeInterface(
    TypeImporter& importer, CalleeInterface interface) -> CalleeInterface;
[[nodiscard]] auto ImportCallable(
    TypeImporter& importer, SubroutinePrototype callable)
    -> SubroutinePrototype;

// The same for the part of a declaration a port or a view names, whose descent
// states the type each step lands on.
[[nodiscard]] auto ImportProjection(
    TypeImporter& importer, MemberProjection projection) -> MemberProjection;

// The same for a whole scope class, every type anywhere on it. The whole
// published list crosses, not the part a referrer happens to name: a member's
// position is counted out of that list.
[[nodiscard]] auto ImportScopeClass(
    TypeImporter& importer, ScopeClassSignature published)
    -> ScopeClassSignature;

// A class's properties and methods taken the same way, in the order given.
[[nodiscard]] auto ImportTypes(
    TypeImporter& importer, std::span<const TypeId> types)
    -> std::vector<TypeId>;
[[nodiscard]] auto ImportProperties(
    TypeImporter& importer, const DeclaredProperties& properties)
    -> DeclaredProperties;
[[nodiscard]] auto ImportStaticProperties(
    TypeImporter& importer, const DeclaredStaticProperties& properties)
    -> DeclaredStaticProperties;
[[nodiscard]] auto ImportMethods(
    TypeImporter& importer, const DeclaredMethods& methods) -> DeclaredMethods;

}  // namespace lyra::hir
