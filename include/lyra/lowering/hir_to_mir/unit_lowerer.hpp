#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/symbol_table.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/design_namespaces.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

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

  [[nodiscard]] auto TranslateExternalUnitObject(
      hir::ExternalUnitObjectId hir_id) const -> mir::ExternalUnitObjectId {
    return external_unit_object_translations_.Get(hir_id);
  }

  // What a name of another unit's object stands for here. A unit reaches such
  // an object by holding what that unit published, and recording that is the
  // act that declares the dependency. A name this unit never recorded arrived
  // on a member of some other unit's object -- one this unit holds a pointer to
  // and never reaches through -- so what it stands for is the pointer's
  // representation, with the pointee left unspecified. Naming the pointee would
  // claim a dependency this unit does not have and pull an artifact it never
  // references.
  [[nodiscard]] auto UnitObjectNamed(const std::string& unit_name) const
      -> mir::Type;

  // Which of a promise's behaviors answers with a published member, in the
  // record this unit kept. The HIR and MIR records list the same members in the
  // same order, so the position crosses unchanged.
  [[nodiscard]] static auto TranslatePublishedMember(hir::PublishedMemberId id)
      -> mir::FieldId {
    return mir::FieldId{id.value};
  }

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

  // Cross-unit reference builders. Each converts a HIR-level cross-unit
  // reference into its MIR peer AND records that this unit consumed the named
  // unit's signature, in one operation, so a caller never records the
  // dependency for itself and cannot build a reference that leaves one
  // unrecorded. Which reference it was is not kept, because what a unit depends
  // on another for is that it read the signature.
  //
  // The type is one of them, which is easy to read as too much: building a
  // value of a class names that class outright, and the type is the whole of
  // what a construction states it by, so a unit holding the type without ever
  // naming the class is indistinguishable from one about to build one.
  auto MakeExternalClassPointee(const hir::ExternalClassRef& ref)
      -> mir::TypeId;

  auto MakeExternalClassRef(const hir::ExternalClassRef& ref) -> mir::ClassRef;

  // Convenience that dispatches a HIR class reference to its MIR peer: an
  // intra-unit reference translates through the class registry, and a
  // cross-unit one is recorded as this unit's dependency in the same call. A
  // caller that needs to name a class in any position (base, interface
  // contract, receiver type) reads one entry point instead of visiting the
  // variant itself.
  auto TranslateClassRef(const hir::ClassRef& ref) -> mir::ClassRef;

  auto MakeCrossUnitClassFieldTarget(
      const hir::ExternalClassPropertyTarget& target)
      -> mir::CrossUnitClassFieldTarget;

  // Takes one promise this unit read of another unit's class into MIR. Every
  // promise the unit read is taken, before anything names one: a place reaching
  // a member of a class an ancestor declares walks the lineage over these
  // records, so a class the walk only passes through has to be among them, and
  // which classes those are is not a question any one reference can answer.
  //
  // Which of them this unit depends on is a narrower set and is not decided
  // here: a promise is read as soon as the elaborating design asks anything of
  // it, so what is taken includes classes this unit names nowhere.
  auto TakeClassPromise(const hir::ExternalClass& published) -> void;

  // Takes this unit's record of what another unit promised about its object
  // into MIR, once per unit reached. A promise is a class of that unit like any
  // other, so what a reference to one reads is the same record; it is taken
  // where the promise is consumed rather than where a member is reached,
  // because consuming it is what makes the unit a dependency.
  auto RecordPromisedClass(const hir::ExternalUnitObject& promised) -> void;

  // Convenience that dispatches a HIR class property reference to its MIR
  // `FieldRef` peer: the intra-unit arm translates the owner class and the
  // field slot through the class registry, the cross-unit arm records this
  // unit's dependency on the declaring unit in the same call. A caller reading
  // a class property reaches for one entry point instead of visiting the
  // variant at each access site.
  auto TranslateClassPropertyTarget(const hir::ClassPropertyTarget& target)
      -> mir::FieldRef;

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

  // Receiver-less callable of a unit's namespace (LRM 26.3 package function or
  // task). A body of that same unit names the position its declaration sits at,
  // because it holds the arena; a body outside it has only the identifier the
  // namespace published, and reading that promise is what records the
  // dependency -- the callable one, never the class one.
  auto MakeNamespaceCallableTarget(const hir::ExternalUnitSubroutineRef& ref)
      -> mir::DirectTarget;

  // A callable another unit published on the object its instances are (LRM
  // 25.7), as one of the behaviors that unit promised of it: the promise is
  // what a referrer holds, so the call reaches the implementation the object
  // turns out to have rather than one named outright. Reaching the object is
  // already the dependency, so nothing further is recorded here.
  [[nodiscard]] auto MakeExternalUnitMethodSlot(
      hir::ExternalUnitObjectId object, hir::PublishedCallableId callable) const
      -> mir::ExternalVirtualSlot;

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

  // The instance `ref`'s class belongs to, where it belongs to one. A class
  // another unit declares is reached through that unit's signature, which
  // carries no instance of a scope inside it, so it belongs to none that a
  // construction or call here could hand it.
  [[nodiscard]] auto DeclaringInstanceOf(const hir::ClassRef& ref) const
      -> std::optional<DeclaringInstance> {
    return std::visit(
        Overloaded{
            [&](const hir::LocalClassRef& local) {
              return GetClassShape(TranslateClass(local.class_id))
                  .declaring_instance;
            },
            [](const hir::ExternalClassRef&)
                -> std::optional<DeclaringInstance> { return std::nullopt; }},
        ref);
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

  auto BuildExternalUnitObject(const hir::ExternalUnitObject& object) const
      -> mir::ExternalUnitObject;

  // Publishes everything the unit declares before any root-scope body lowers:
  // every class identity and body, every interned type, this unit's record of
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

  const hir::CompilationUnit* hir_;
  const diag::SourceManager* source_manager_;
  mir::CompilationUnit unit_;
  base::Translation<hir::TypeId, mir::TypeId> type_translations_;
  base::Translation<hir::ClassId, ClassTranslation> class_translations_;
  base::Translation<hir::ExternalUnitObjectId, mir::ExternalUnitObjectId>
      external_unit_object_translations_;
  std::unordered_map<std::string, mir::ExternalUnitObjectId>
      external_unit_objects_by_name_;
  std::uint32_t next_synthesized_site_ = 0;
  // What the declare stage settled about each class, read by every body that
  // names a peer. Lives only on the lowerer; the finished compilation unit
  // holds the only authoritative class representation.
  base::SymbolTable<mir::ClassId, ClassShape> declarations_;
  std::unordered_map<hir::TypeId, mir::CallableId> assignment_pattern_texts_;
};

}  // namespace lyra::lowering::hir_to_mir
