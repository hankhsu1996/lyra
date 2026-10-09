#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/symbol_table.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::hir_to_mir {

// What one HIR class declaration became: its MIR identity, and the interned
// object type that names it. Both are settled together before any type is
// translated, so a class handle type resolves to its managed-reference pointee
// while the class body is still being built.
struct ClassTranslation {
  mir::ClassId id{};
  mir::TypeId object_type{};
  // The callable identity of each method. An overriding method states its
  // dispatch role (LRM 8.20) against the slot its base was given, so a class's
  // declaration names another class's method identity and these are settled
  // before any declaration is.
  base::Translation<hir::MethodId, mir::CallableId> methods;
};

// One field of the class a scope publishes: the type it holds, and the name a
// referrer reaches it by, which only a member has.
struct PublishedField {
  std::optional<std::string> name;
  mir::TypeId type;
};

// The field of a scope's published class each thing the scope published
// occupies: one per member, one per generate construct holding what it built,
// and one per disable target holding what a `disable` of it ends.
struct PublishedScopeLayout {
  base::Translation<hir::PublishedMemberId, mir::FieldId> members;
  base::Translation<hir::PublishedGenerateId, mir::FieldId> generates;
  base::Translation<hir::PublishedDisableTargetId, mir::FieldId>
      disable_targets;
};

// The method of a scope's published class that enters the subroutine the scope
// published at `published`: the class reserves one per published subroutine,
// at the position the signature gave it. The declaring scope builds the method
// there and a body of the same unit calls it there, so both read it from
// here.
[[nodiscard]] inline auto PublishedMethodOf(hir::PublishedCallableId published)
    -> mir::CallableId {
  return mir::CallableId{published.value};
}

// One class realizing a class the unit published of its scopes: what a scope
// lowered to, extending the published class with what the lowering adds, and
// its body for each subroutine the class published, by the position the
// signature gave the subroutine.
struct ScopeRealization {
  mir::ClassId id{};
  base::Translation<hir::PublishedCallableId, mir::CallableId> subroutines;
};

// What this unit knows of a scope class some unit published: the class, the
// type each of its fields holds, and where it placed each thing the scope
// published.
struct ScopeClassLayout {
  mir::DeclaredClassRef cls;
  std::vector<mir::TypeId> field_types;
  PublishedScopeLayout published;
};

// A class this unit published of its scopes. Several scopes may be published
// as one class -- the block instances of one application of a generate block
// that lowered apart (LRM 27.3) -- so what the signature fixes is held once:
// where it placed what was published, and the identifier a referrer spells
// each published subroutine by, in the order the signature gave them. The
// classes realizing it follow, in the order their scopes were lowered.
struct PublishedScope {
  ScopeClassLayout layout;
  std::vector<std::string> subroutine_names;
  std::vector<ScopeRealization> realizations;
};

// Lowers one HIR compilation unit into one MIR compilation unit, holding that
// unit as it is built along with everything the declaration stages settled
// about it. A body reads a peer's answer from here rather than from the
// lowering that produced it, which is what leaves the bodies free of any order
// among themselves. The finished unit is moved out, and nothing here points
// into it afterwards.
class UnitLowerer {
 public:
  UnitLowerer(
      const hir::CompilationUnit& hir,
      const diag::SourceManager& source_manager)
      : hir_(&hir), source_manager_(&source_manager) {
  }

  // Lowers a unit whose instances exist as objects: its body (variables,
  // processes, instances, subroutines) composes the unit's top class.
  auto RunObjectRoot() -> diag::Result<mir::CompilationUnit>;

  // Lowers the synthetic design-root unit: a module whose root class is the
  // one nothing else constructs, so it alone carries the design's way in, and
  // whose Initialize phase also brings up the namespaces' variables (LRM 26.2 /
  // 10.5). Which namespaces those are is read off the signatures the root
  // consumes and passed in; the lowering turns each into cross-unit calls, so
  // this input stays at the design-root boundary and never reaches a source
  // unit's lowering.
  auto RunDesignRoot(DesignNamespaces namespaces)
      -> diag::Result<mir::CompilationUnit>;

  // Lowers a unit that roots no object -- a package (LRM 26) or the `$unit`
  // file-set scope (LRM 3.12.1). Its functions and tasks lower to
  // receiver-less callables owned by the unit's namespace, so the produced unit
  // has no root class.
  auto RunNamespace() -> diag::Result<mir::CompilationUnit>;

  // Access to the in-progress compilation unit. The const overload is the
  // read-only view downstream consumers see once lowering finishes; the mutable
  // overload lets a handler append unit-wide output -- a synthesized type, a
  // deferred-check site -- to the unit, the same discipline by which nested IR
  // is written through the frame's current targets.
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return unit_;
  }
  [[nodiscard]] auto Unit() -> mir::CompilationUnit& {
    return unit_;
  }

  [[nodiscard]] auto Hir() const -> const hir::CompilationUnit& {
    return *hir_;
  }

  // Where a construct was written, which a diagnostic the lowering emits names
  // its origin by (LRM 20.10).
  [[nodiscard]] auto SourceManager() const -> const diag::SourceManager& {
    return *source_manager_;
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    return type_translations_.Get(hir_id);
  }

  [[nodiscard]] auto TranslateClass(hir::ClassId hir_id) const -> mir::ClassId {
    return class_translations_.Get(hir_id).id;
  }

  // The class the scope `hir_id` records is -- an instance of some unit, or a
  // generate block inside one -- as this unit names it.
  [[nodiscard]] auto ScopeClassIdentity(hir::ExternalScopeClassId hir_id) const
      -> mir::DeclaredClassRef;

  // The type of such an object.
  [[nodiscard]] auto UnitObjectType(hir::ExternalScopeClassId hir_id) const
      -> mir::TypeId;

  // A class a unit published of one of its scopes, `class_path` of
  // `unit_name`, as this unit names it: one of its own where `unit_name` is
  // this unit, however the name reaching it was written, and otherwise the
  // other unit's. One class has one identity here (LRM 23.6 lets a name leave
  // an instance and reach another instance of the same unit).
  [[nodiscard]] auto ClassIdentityOf(
      const std::string& unit_name, const support::DefPath& class_path) const
      -> mir::DeclaredClassRef;

  // The class this unit published of the scopes it published under
  // `class_path`. Every one is minted before any type translates, since a type
  // a signature states may name one.
  [[nodiscard]] auto PublishedScopeClassAt(
      const support::DefPath& class_path) const -> mir::ClassId;

  // Takes the identity of the class the unit published of one of its scopes,
  // which is one class for every scope published under one path. The lowering
  // of the scope takes it as it is built, which is before any type translates.
  auto TakePublishedScopeClass(const hir::ScopePublication& published)
      -> mir::ClassId;

  // Where the class `published` places what `scope` published, laying the
  // class out from the scope's signature the first time a scope published as
  // it asks. Every scope published as one class states one signature, so
  // whichever asks first settles the same shape.
  auto SettlePublishedScope(
      mir::ClassId published, const hir::StructuralScope& scope)
      -> const PublishedScopeLayout&;

  // Records that `realization` realizes the class `published`, once its own
  // class is settled in the unit.
  void AddRealization(mir::ClassId published, ScopeRealization realization);

  // Every class the unit published of its scopes.
  [[nodiscard]] auto PublishedScopes() const
      -> const std::map<mir::ClassId, PublishedScope>& {
    return published_scopes_;
  }

  // What this unit knows of the scope class `hir_id` records: of one of its
  // own, where that scope's shape placed what was published; of another
  // unit's, its record of what that unit published, laid out where the object
  // is consumed. Either is there before any body lowers, so every reach into
  // one finds it.
  [[nodiscard]] auto ScopeClassLayoutOf(hir::ExternalScopeClassId hir_id) const
      -> const ScopeClassLayout&;

  // The fields of the class a scope publishes, in the order `signature`
  // states them: one per member, named as it was published, then one per
  // generate construct, then one per disable target. The scope declaring the
  // class and every unit reaching it lay it out from this, so the two cannot
  // disagree about where anything sits or what it holds.
  [[nodiscard]] auto PublishedFieldsOf(
      const hir::ScopeClassSignature& signature) const
      -> std::vector<PublishedField>;

  // Where each thing `signature` publishes sits, once its published fields were
  // added in order at `slots`.
  [[nodiscard]] static auto PublishedLayoutAt(
      const hir::ScopeClassSignature& signature,
      std::span<const mir::FieldId> slots) -> PublishedScopeLayout;

  // The cell a member of `storage` holds, over a value of `value_type`. A unit
  // that declares the member and a unit reading its signature both reach it
  // through here, so they cannot disagree about what a published member holds.
  [[nodiscard]] auto MemberCellType(
      mir::TypeId value_type, const hir::PublishedStorage& storage) const
      -> mir::TypeId;

  // The callable identity a class's method was given. Answered from what the
  // declaration pass took, so a class that overrides can state its dispatch
  // role whether or not its base has settled -- no order among declarations.
  [[nodiscard]] auto TranslateMethod(
      hir::ClassId owner, hir::MethodId method) const -> mir::CallableId {
    return class_translations_.Get(owner).methods.Get(method);
  }

  [[nodiscard]] auto ClassObjectType(hir::ClassId hir_id) const -> mir::TypeId {
    return class_translations_.Get(hir_id).object_type;
  }

  // The pointee object type a managed handle to an imported runtime-library
  // class names. Each imported class is a fixed library class, so its object
  // type is a well-known type interned once on the unit.
  [[nodiscard]] auto ImportedRuntimeObjectType(
      support::ImportedRuntimeClass klass) const -> mir::TypeId {
    switch (klass) {
      case support::ImportedRuntimeClass::kProcess:
        return unit_.builtins.process_object;
    }
    throw InternalError(
        "UnitLowerer::ImportedRuntimeObjectType: unknown imported class");
  }

  // A class some unit declared, `class_path` of `unit_name`, as this unit names
  // it: one of its own where `unit_name` is this unit -- a signature of another
  // unit names it so -- and otherwise the other unit's. Naming one records no
  // dependency; the builders below are what record one.
  [[nodiscard]] auto DeclaredClassIdentityOf(
      const std::string& unit_name, const support::DefPath& class_path) const
      -> mir::DeclaredClassRef;

  // What a signature names of a class, in MIR's terms. Reading a signature
  // records no dependency, so neither does this; a reference that names the
  // class records it where it takes the name.
  [[nodiscard]] auto PublishedClass(const hir::ExternalClassRef& ref) const
      -> mir::DeclaredClassRef;

  // Cross-unit reference builders. Each converts a HIR-level cross-unit
  // reference into its MIR peer AND records that this unit consumed the named
  // unit's signature, in one operation, so a caller never records the
  // dependency for itself and cannot build a reference that leaves one
  // unrecorded. Which reference it was is not kept, because what a unit depends
  // on another for is that it read the signature. A reference naming this
  // unit's own class is to that class, and depends on nothing.
  //
  // The type is one of them, which is easy to read as too much: building a
  // value of a class names that class outright, and the type is the whole of
  // what a construction states it by, so a unit holding the type without ever
  // naming the class is indistinguishable from one about to build one.
  auto MakeExternalClassPointee(const hir::ExternalClassRef& ref)
      -> mir::TypeId;

  auto MakeExternalClassRef(const hir::ExternalClassRef& ref)
      -> mir::DeclaredClassRef;

  // Convenience that dispatches a HIR class reference to its MIR peer: an
  // intra-unit reference translates through the class registry, and a
  // cross-unit one is recorded as this unit's dependency in the same call. A
  // caller that needs to name a class in any position (base, interface
  // contract, receiver type) reads one entry point instead of visiting the
  // variant itself.
  auto TranslateClassRef(const hir::ClassRef& ref) -> mir::DeclaredClassRef;

  auto MakeCrossUnitClassFieldTarget(
      const hir::ExternalClassPropertyTarget& target) -> mir::ClassFieldTarget;

  // Takes one record this unit read of what another unit published about its
  // class into MIR. Every such record the unit read is taken, before anything
  // names one: a place reaching a member of a class an ancestor declares walks
  // the lineage over these records, so a class the walk only passes through has
  // to be among them, and which classes those are is not a question any one
  // reference can answer.
  //
  // Which of them this unit depends on is a narrower set and is not decided
  // here: a signature is read as soon as the elaborating design asks anything
  // of it, so what is taken includes classes this unit names nowhere.
  auto TakePublishedClass(const hir::ExternalClass& published) -> void;

  // Takes this unit's record of the class a scope of another unit is -- one of
  // its instances, or a generate block inside one -- into MIR, once per class
  // reached, laying out a field for each thing the scope published. That class
  // is a class of that unit like any other, so what a reference to one reads is
  // the same record; it is taken where the object is consumed rather than where
  // a member is reached, because consuming it is what makes the unit a
  // dependency.
  auto RecordPublishedScopeClass(
      hir::ExternalScopeClassId hir_id, const hir::ExternalScopeClass& scope)
      -> void;

  // A property of a class of this unit as its MIR field, translated through the
  // class registry.
  [[nodiscard]] auto TranslateClassPropertyTarget(
      const hir::LocalClassPropertyTarget& local) const
      -> mir::ClassFieldTarget;

  auto MakeExternalStaticPropertyRef(
      const hir::ExternalStaticPropertyTarget& target)
      -> mir::ExternalStaticPropertyRef;

  auto MakeExternalMethodTarget(const hir::ExternalClassMethodTarget& target)
      -> mir::ExternalUnitClassMethodTarget;

  auto MakeExternalMethodOverride(const hir::ExternalDispatchSlot& slot)
      -> mir::OverridesExternalSlot;

  // The behavior a call dispatches on when the class introducing it belongs to
  // another compilation unit, named by the coordinate that class published.
  // Records the class dependency so the referring artifact names the unit it
  // reaches into.
  auto MakeExternalVirtualSlot(const hir::ExternalDispatchSlot& slot)
      -> mir::ExternalVirtualSlot;

  // The behavior a method of this unit's class answers, named by the class
  // that introduced it, or nothing where the method is in no dispatch. A call
  // that dispatches and a method answering an interface class's behavior name
  // the behavior the same way, through here.
  [[nodiscard]] auto LocalVirtualSlotOf(
      const hir::LocalClassMethodTarget& method) const
      -> std::optional<mir::VirtualSlot>;

  // Receiver-less callable of a unit's namespace (LRM 26.3 package function or
  // task). A body of that same unit names the position its declaration sits at,
  // because it holds the arena; a body outside it has only the identifier the
  // namespace published, and reading that signature is what records the
  // dependency -- the callable one, never the class one.
  auto MakeNamespaceCallableTarget(const hir::ExternalUnitSubroutineRef& ref)
      -> mir::DirectTarget;

  // A subroutine a unit published on the object its instances are (LRM 23.6,
  // 25.7): a method of that unit's published class, called directly, since the
  // unit an instance is of is settled where the call compiles. A method of
  // this unit's own class is named by its position, as any of its callables
  // is; another unit's by what that unit published. Reaching the object is
  // already the dependency, so nothing further is recorded here.
  [[nodiscard]] auto MakeExternalUnitMethodTarget(
      hir::ExternalScopeClassId scope_class,
      hir::PublishedCallableId callable) const -> mir::DirectTarget;

  // Mints a fresh owner-site id for a synthesized binding origin -- a carrier a
  // lowering creates that has no source-level variable (an activation handle, a
  // non-blocking-assignment snapshot). The id only has to be unit-unique and
  // deterministic so the carrier's `BindingOriginId::Synthesized` is a stable,
  // collision-free key across every synthesizer in the unit; a monotonic count
  // over the deterministic lowering walk provides that (never a global
  // cross-unit counter, so identity stays stable under incremental / parallel
  // compilation).
  [[nodiscard]] auto NextSynthesizedSite() -> std::uint32_t {
    return next_synthesized_site_++;
  }

  // Settles one class's structural declaration; it is written once and read
  // back by every peer body that names the class.
  void DefineClassShape(mir::ClassId id, ClassShape shape) {
    declarations_.Define(id, std::move(shape));
  }

  [[nodiscard]] auto GetClassShape(mir::ClassId id) const -> const ClassShape& {
    return declarations_.Get(id);
  }

  // The function answering the assignment-pattern text of one type (LRM
  // 21.2.1.6). Every such text is settled with this unit's declarations, so a
  // site that has established the type has one finds it here; a site that has
  // not established it is asking a question it has no answer for.
  //
  // The type is the SystemVerilog one because the declaration is what decides,
  // and a lowering answers facts it then stops carrying: a packed tagged union
  // reads as its tag and the member that tag names where an untagged one reads
  // as its first member, while both project onto one vector below the front
  // end.
  [[nodiscard]] auto AssignmentPatternTextOf(hir::TypeId type) const
      -> mir::UnitCallableTarget {
    const auto it = assignment_pattern_texts_.find(type);
    if (it == assignment_pattern_texts_.end()) {
      throw InternalError(
          "UnitLowerer::AssignmentPatternTextOf: this type states no "
          "assignment-pattern text");
    }
    return mir::UnitCallableTarget{.slot = it->second};
  }

 private:
  // The finished unit, and the one way out of this pass. Every value the unit
  // holds is settled here, so a consumer reads a complete set however the
  // lowering reached its end.
  auto Finish() -> mir::CompilationUnit;

  // Lowers a scope whose root is an object type into the unit's top class,
  // which the unit then names as its root. The namespace list is empty for a
  // source module and names every namespace of the design for the design root
  // (LRM 26.2 / 10.5), whose Initialize phase turns each into a cross-unit
  // install and initialize call.
  auto PopulateModuleRoot(DesignNamespaces namespaces) -> diag::Result<void>;

  // Everything one class declaration can be named by before it settles: its
  // own identity, the object type that names it, and one identity per method a
  // deriving class may state a dispatch role against. Reads nothing but this
  // declaration, so classes take theirs in any order and none waits on another.
  auto TakeClassIdentities(const hir::ClassDecl& decl) -> ClassTranslation;

  // Publishes everything the unit declares before any root-scope body lowers:
  // every class identity and body, every interned type and the functions each
  // structure type answers a whole-value operation with, this unit's record of
  // each object it reaches in another unit, and the prototype of every foreign
  // symbol the unit takes part in. Shared prologue of every unit
  // kind -- a module and a package own the same declaration kinds; they differ
  // only in whether the root scope becomes a top class or a set of namespace
  // callables.
  auto PublishUnitDeclarations() -> diag::Result<void>;

  // Settles the assignment-pattern text of every type this unit names that
  // states one. Whether a type does follows from that type on its own, so this
  // is one answer per type rather than a fact gathered over the unit, and it
  // stands whether or not anything in the unit goes on to print a value of that
  // type. Each is a function the unit owns: it takes the value and no object,
  // so no class is its owner.
  void PublishAssignmentPatternTexts();

  [[nodiscard]] auto TranslateType(const hir::Type& type) -> mir::Type;

  // The struct type naming `src`. A struct this unit declares takes its
  // identity here and its declaration once its type is interned, since a
  // method names the type of the value it is asked of; one another unit
  // declares is named by that declaration, and holding a value of it depends
  // on that unit.
  [[nodiscard]] auto TranslateStructType(const hir::UnpackedStructType& src)
      -> mir::StructType;

  // Settles the declaration `id` of a struct this unit declares, over values of
  // the interned type `structure` naming it.
  void DefineOwnStruct(
      const hir::UnpackedStructType& src, mir::StructId id,
      mir::TypeId structure);

  const hir::CompilationUnit* hir_;
  const diag::SourceManager* source_manager_;
  mir::CompilationUnit unit_;
  base::Translation<hir::TypeId, mir::TypeId> type_translations_;
  base::Translation<hir::ClassId, ClassTranslation> class_translations_;
  // This unit's record of each of those classes, by the HIR record of it.
  std::map<hir::ExternalScopeClassId, ScopeClassLayout> external_scope_layouts_;
  // Keyed by path, because a signature names a unit's scope class by path and
  // nothing else: a class of this unit reached through another unit's
  // signature arrives as one, and is resolved to this unit's class here.
  std::map<support::DefPath, mir::ClassId> published_scope_classes_;
  // The same, for the classes this unit declares: a signature of another unit
  // names one by path -- a specialization of another unit's generic holding a
  // value of it, say -- and it is resolved to this unit's class here.
  std::map<support::DefPath, mir::ClassId> declared_classes_;
  std::map<mir::ClassId, PublishedScope> published_scopes_;
  std::uint32_t next_synthesized_site_ = 0;
  // What the declare stage settled about each class, read by every body that
  // names a peer. Lives only on the lowerer; the finished compilation unit
  // holds the only authoritative class representation.
  base::SymbolTable<mir::ClassId, ClassShape> declarations_;
  std::unordered_map<hir::TypeId, mir::CallableId> assignment_pattern_texts_;
};

}  // namespace lyra::lowering::hir_to_mir
