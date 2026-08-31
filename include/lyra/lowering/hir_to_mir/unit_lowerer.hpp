#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/symbol_table.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
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

  // Lowers the synthetic design-root unit. It roots an object like any other
  // such unit, except that its Initialize phase also installs and initializes
  // the packages' variables (LRM 26.2 / 10.5). The plan is a whole-design fact
  // the assembly resolves and passes in; the lowering only realizes it into
  // cross-unit calls, so this special input stays at the design-root boundary
  // and never reaches a source unit's lowering.
  auto RunDesignRoot(PackageInitializationPlan package_init_plan)
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
      -> mir::TypeData;

  // Where a published member sits in the object this unit recorded. The HIR and
  // MIR records list the same members in the same order, so the position
  // crosses unchanged.
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
  // reference into its MIR peer AND records the referenced unit's name on
  // the unit's cross-unit dependency list in one operation, so a caller
  // never touches the raw dependency-list mutators. The class-side builders
  // below record on the class dependency list; the trailing callable
  // builder records on the callable dependency list, since a package
  // callable and a class member of another unit are two independent
  // include axes for a backend.
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

  auto MakeExternalFieldTarget(const hir::ExternalClassPropertyTarget& target)
      -> mir::ExternalFieldTarget;

  // Convenience that dispatches a HIR class property reference to its MIR
  // `FieldRef` peer: the intra-unit arm translates the owner class and the
  // field slot through the class registry, the cross-unit arm runs through
  // `MakeExternalFieldTarget` so the external dependency is recorded in the
  // same call. A caller reading a class property reaches for one entry
  // point instead of visiting the variant at each access site.
  auto TranslateClassPropertyTarget(const hir::ClassPropertyTarget& target)
      -> mir::FieldRef;

  auto MakeExternalStaticPropertyRef(
      const hir::ExternalStaticPropertyTarget& target)
      -> mir::ExternalStaticPropertyRef;

  auto MakeExternalMethodTarget(const hir::ExternalClassMethodTarget& target)
      -> mir::ExternalUnitClassMethodTarget;

  auto MakeExternalMethodOverride(const hir::ExternalClassMethodTarget& target)
      -> mir::OverridesExternalSlot;

  // The virtual-call-site counterpart of `MakeExternalMethodTarget`: the
  // referring unit reaches a slot introduced by a class in another
  // compilation unit and dispatches through the target language's own
  // virtual-call machinery reached by including the declaring unit's
  // header. Records the class dependency so the header include is emitted.
  auto MakeExternalVirtualSlot(const hir::ExternalClassMethodTarget& target)
      -> mir::ExternalVirtualSlot;

  // Receiver-less callable of another compilation unit (LRM 26.3 package
  // function or task). Recorded on the callable dependency list, not the
  // class one.
  auto MakeExternalCallableTarget(const hir::ExternalUnitSubroutineRef& ref)
      -> mir::ExternalUnitCallableTarget;

  // Mints a collision-free class name for one generate scope, tagged by its
  // arm kind (`loop` / `then` / `else` / ...). The name is only an
  // implementation handle for the emitted type -- a generate scope's runtime
  // identity is its HierarchySegment -- so it need only be unit-unique and
  // deterministic, which a monotonic count over the deterministic lowering walk
  // provides.
  [[nodiscard]] auto NextGenerateScopeName(std::string_view arm_tag)
      -> std::string;

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

  // Per-unit dedup of the callables synthesized for the LRM 6.19.5 `name` and
  // shared `next` / `prev` step operations, keyed by the enum's MIR type value.
  // One callable per enum is reused across every call site.
  [[nodiscard]] auto EnumNameHelpers()
      -> std::unordered_map<std::uint32_t, mir::CallableTarget>& {
    return enum_name_helpers_;
  }
  [[nodiscard]] auto EnumStepHelpers()
      -> std::unordered_map<std::uint32_t, mir::CallableTarget>& {
    return enum_step_helpers_;
  }

 private:
  // Lowers a scope whose root is an object type into the unit's top class. The
  // package initialization plan is empty for a source module and carries the
  // design root's resolved plan (LRM 26.2 / 10.5), which the root scope's
  // Initialize phase realizes into cross-unit install and initialize calls.
  auto LowerModuleUnit(PackageInitializationPlan package_init_plan)
      -> diag::Result<mir::CompilationUnit>;

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

  [[nodiscard]] auto TranslateTypeData(const hir::TypeData& data)
      -> mir::TypeData;

  const hir::CompilationUnit* hir_;
  const diag::SourceManager* source_manager_;
  mir::CompilationUnit unit_;
  base::Translation<hir::TypeId, mir::TypeId> type_translations_;
  base::Translation<hir::ClassId, ClassTranslation> class_translations_;
  base::Translation<hir::ExternalUnitObjectId, mir::ExternalUnitObjectId>
      external_unit_object_translations_;
  std::unordered_map<std::string, mir::ExternalUnitObjectId>
      external_unit_objects_by_name_;
  std::uint32_t next_generate_scope_name_ = 0;
  std::uint32_t next_synthesized_site_ = 0;
  // What the declare stage settled about each class, read by every body that
  // names a peer. Lives only on the lowerer; the finished compilation unit
  // holds the only authoritative class representation.
  base::SymbolTable<mir::ClassId, ClassShape> declarations_;
  std::unordered_map<std::uint32_t, mir::CallableTarget> enum_name_helpers_;
  std::unordered_map<std::uint32_t, mir::CallableTarget> enum_step_helpers_;
};

}  // namespace lyra::lowering::hir_to_mir
