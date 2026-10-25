#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
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
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/design_root_inputs.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/scope_execution.hpp"

namespace lyra::lowering::hir_to_mir {

// Which functions every object's part declares for `randomize()` (LRM 18.6) a
// class that is no interface class gives a body: declaring its random
// variables, stating its constraints and naming the random objects its handles
// reach, which every such class does, and calling the `pre_randomize` and
// `post_randomize` it declares, where it declares one (LRM 18.6.2). A class of
// this unit and one another unit published answer it alike.
[[nodiscard]] inline auto RandomizationOverrides(
    bool declares_pre_randomize, bool declares_post_randomize)
    -> std::vector<support::LibraryVirtual> {
  std::vector<support::LibraryVirtual> out{
      support::LibraryVirtual::kClassRandVariables,
      support::LibraryVirtual::kClassConstrain,
      support::LibraryVirtual::kClassRandObjects};
  if (declares_pre_randomize) {
    out.push_back(support::LibraryVirtual::kClassPreRandomize);
  }
  if (declares_post_randomize) {
    out.push_back(support::LibraryVirtual::kClassPostRandomize);
  }
  return out;
}

// The bodies a class of this unit gives those functions: the three every class
// gives, which a class extending another of the same unit calls the one it
// extends by, and one calling each hook it declares.
struct RandomizationHooks {
  // A hook calling a method the class declares: the hook's body, and the
  // method it calls.
  struct CallsDeclared {
    mir::CallableId body;
    hir::MethodId method;
  };

  mir::CallableId rand_variables;
  mir::CallableId constrain;
  mir::CallableId rand_objects;
  std::optional<CallsDeclared> pre_randomize;
  std::optional<CallsDeclared> post_randomize;

  // Each body, with the function of the library it overrides.
  [[nodiscard]] auto Overrides() const
      -> std::vector<std::pair<mir::CallableId, support::LibraryVirtual>> {
    std::vector<std::pair<mir::CallableId, support::LibraryVirtual>> out;
    for (const support::LibraryVirtual function : RandomizationOverrides(
             pre_randomize.has_value(), post_randomize.has_value())) {
      out.emplace_back(BodyOf(function), function);
    }
    return out;
  }

  // The body the class gives `function`, one of those it overrides.
  [[nodiscard]] auto BodyOf(support::LibraryVirtual function) const
      -> mir::CallableId {
    switch (function) {
      case support::LibraryVirtual::kClassRandVariables:
        return rand_variables;
      case support::LibraryVirtual::kClassConstrain:
        return constrain;
      case support::LibraryVirtual::kClassRandObjects:
        return rand_objects;
      case support::LibraryVirtual::kClassPreRandomize:
        if (pre_randomize.has_value()) {
          return pre_randomize->body;
        }
        break;
      case support::LibraryVirtual::kClassPostRandomize:
        if (post_randomize.has_value()) {
          return post_randomize->body;
        }
        break;
      case support::LibraryVirtual::kScopeResolve:
      case support::LibraryVirtual::kScopeInitialize:
      case support::LibraryVirtual::kScopeCreateProcesses:
      case support::LibraryVirtual::kScopeDescribe:
        break;
    }
    throw InternalError(
        "RandomizationHooks: a function the class gives no body is asked for "
        "its body -- please report this as a bug");
  }
};

// What a class this unit declares takes beyond its identity before any
// declaration settles, so a class extending it can name it.
struct ClassTranslation {
  // The callable identity of each forward the class answers an interface
  // class's behavior with, after every method the class declares, which is at
  // the position its declaration gives it.
  std::vector<mir::CallableId> interface_forwards;
  // Absent for an interface class, which has no object (LRM 8.26).
  std::optional<RandomizationHooks> randomization;
};

// One field of a class as every unit lays it out: the type it holds, and the
// name a referrer reaches it by, where the source declared one.
struct PublishedField {
  std::optional<std::string> name;
  mir::TypeId type;
};

// The field a property of a class is, named as the class declares it.
struct PropertyField {
  std::string name;
  mir::TypeId type;
};

// The fields of an object of a class, as the class this unit defines and every
// unit declaring it lay them out: each property where the declaration places
// it, then the instance the object belongs to (LRM 6.22) where the class takes
// one. The instance comes after the properties, since what another unit counts
// a property's position out of is the properties alone.
struct ClassFields {
  std::vector<PropertyField> properties;
  std::optional<mir::TypeId> declaring_instance;
};

// The field of a scope's published class each thing the scope published
// occupies: one per member, one per generate construct holding what it built,
// and one per disable target holding what a `disable` of it ends.
struct PublishedScopeLayout {
  base::Translation<hir::PublishedMemberId, mir::FieldId> members;
  base::Translation<hir::PublishedLoopId, mir::FieldId> loops;
  base::Translation<hir::PublishedChoiceId, mir::FieldId> choices;
  base::Translation<hir::PublishedDisableTargetId, mir::FieldId>
      disable_targets;
};

// The virtual method of a scope's published class the subroutine the scope
// published at `published` is: the class introduces one per published
// subroutine, at the position the signature gave it, and leaves it to each
// class realizing the scope. A realizing class overrides it there and a call
// names it there, so both read it from here.
[[nodiscard]] inline auto PublishedMethodOf(hir::PublishedCallableId published)
    -> mir::CallableId {
  return mir::CallableId{published.value};
}

// What this unit knows of a scope class some unit published: the class, the
// type each of its fields holds, and where it placed each thing the scope
// published.
struct ScopeClassLayout {
  mir::ClassId cls;
  std::vector<mir::TypeId> field_types;
  PublishedScopeLayout published;
};

// A subroutine a scope published, as the class it published states it: the
// identifier a referrer spells it by, and what a call of it passes and yields.
struct PublishedSubroutine {
  std::string name;
  mir::CallableCode prototype;
};

// A class this unit published of its scopes. Several scopes may be published
// as one class -- the bodies of one generate block whose code uses its index
// (LRM 27.3) -- so what the signature fixes is held once: where it
// placed what was published, and each subroutine it published, in the order
// the signature gave them.
struct PublishedScope {
  ScopeClassLayout layout;
  base::Translation<hir::PublishedCallableId, PublishedSubroutine> subroutines;
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
      : hir_(&hir),
        source_manager_(&source_manager),
        root_instantiations_(hir, hir.root_scope) {
    // The class of each scope of the design hierarchy the unit names -- its
    // own scopes' and those of other units it reaches -- takes its identity
    // first: building the tree of the unit's scopes names its own, and a type
    // may name any of them.
    for (const hir::ScopeClassId scope_class : hir.scope_classes.Ids()) {
      scope_class_translations_.emplace(scope_class, unit_.DeclareClass());
    }
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
  // unit's lowering. So do the instantiations, from which the root states
  // every body's construction table, its own among them.
  auto RunDesignRoot(DesignRootInputs design_root)
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

  // How the runtime runs what an instance of one of this unit's scopes
  // starts: as a program's, where the unit is one (LRM 24.3.1).
  [[nodiscard]] auto ScopeExecution() const -> support::ScopeExecution;

  // What the root scope of this unit's objects makes at each instantiation it
  // declares, and its blocks the same, in the order their construction tables
  // list them.
  [[nodiscard]] auto RootInstantiations() const
      -> const hir::ScopeInstantiations& {
    return root_instantiations_;
  }

  // Where a construct was written, which a diagnostic the lowering emits names
  // its origin by (LRM 20.10).
  [[nodiscard]] auto SourceManager() const -> const diag::SourceManager& {
    return *source_manager_;
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    return type_translations_.Get(hir_id);
  }

  // The class the covergroup `covergroup` is written as (LRM 19.3): this
  // unit's own for one it declares, and otherwise the class the declaring unit
  // writes, declared here.
  [[nodiscard]] auto CovergroupClassIdentity(hir::CovergroupId covergroup) const
      -> mir::ClassId {
    return covergroup_identities_.Get(covergroup);
  }

  // The class the scope class `hir_id` is -- of an instance of some unit, this
  // one included, or a generate block inside one -- as this unit names it.
  [[nodiscard]] auto ScopeClassIdentity(hir::ScopeClassId hir_id) const
      -> mir::ClassId;

  // The type of such an object.
  [[nodiscard]] auto UnitObjectType(hir::ScopeClassId hir_id) const
      -> mir::TypeId;

  // Where the class `published` places what its scopes published, laying the
  // class out from `signature` the first time it is asked. Every scope
  // published as one class states one signature, so whichever asks first
  // settles the same shape.
  auto SettlePublishedScope(
      mir::ClassId published, const hir::ScopeClassSignature& signature)
      -> const PublishedScopeLayout&;

  // Every class the unit published of its scopes.
  [[nodiscard]] auto PublishedScopes() const
      -> const std::map<mir::ClassId, PublishedScope>& {
    return published_scopes_;
  }

  // What this unit knows of the scope class `hir_id` records: of one of its
  // own, where that scope's shape placed what was published; of another
  // unit's, the declaration this unit took of it, laid out where the object
  // is consumed. Either is there before any body lowers, so every reach into
  // one finds it.
  [[nodiscard]] auto ScopeClassLayoutOf(hir::ScopeClassId hir_id) const
      -> const ScopeClassLayout&;

  // The fields of an object of the class `declared` describes.
  [[nodiscard]] auto ClassFieldsOf(const hir::ClassDeclaration& declared) const
      -> ClassFields;

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

  // The callable identity of each forward a class this unit declares answers
  // an interface class's behavior with, in the order the class lists them.
  [[nodiscard]] auto InterfaceForwardsOf(hir::ClassId owner) const
      -> std::span<const mir::CallableId> {
    return OwnTranslation(owner).interface_forwards;
  }

  [[nodiscard]] auto RandomizationHooksOf(hir::ClassId owner) const
      -> const std::optional<RandomizationHooks>& {
    return OwnTranslation(owner).randomization;
  }

  // The method of the source an instance-method call runs: the one it names,
  // or for a hook a class has without declaring it, the one the nearest class
  // of its lineage declares, since neither hook is virtual (LRM 18.6.2).
  // Nothing where the library runs the hook: no class of the lineage declares
  // one, or the class is an interface class, whose hooks are virtual
  // (LRM 8.26.9).
  [[nodiscard]] auto MethodRunBy(const hir::InstanceMethodCallee& callee) const
      -> std::optional<hir::ClassMethodTarget>;

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

  // The class `cls` is, in MIR's terms, whichever unit defines it.
  [[nodiscard]] auto ClassIdentity(hir::ClassId cls) const -> mir::ClassId {
    return class_identities_.Get(cls);
  }

  // The object type of a class, and of the class a covergroup is written as
  // (LRM 19.3).
  auto ClassPointee(hir::ClassId cls) const -> mir::TypeId;
  auto CovergroupObjectType(hir::CovergroupId covergroup) const -> mir::TypeId;

  // The pointer a body of the class `cls` reaches its object through.
  auto SelfPointerTo(mir::ClassId cls) const -> mir::TypeId;

  // The declaration of a class `class_path` of `unit_name` stating nothing of
  // what it declares, as an incomplete type is, at `id`: what a class this unit
  // only names is, and what every other declaration of another unit's class
  // starts from.
  auto IncompleteClass(
      mir::ClassId id, std::string unit_name, support::DefPath class_path)
      -> mir::Class;

  // A property of a class as its MIR field. The properties a class of another
  // unit publishes are a prefix of its own storage, so the position counted out
  // of its signature is the slot that class gave.
  auto TranslateClassPropertyTarget(const hir::ClassPropertyTarget& target)
      -> mir::ClassFieldTarget;

  // Where a static property of a class lies: where the class this unit
  // declares placed its cell, which follows what replicates the class (LRM
  // 6.22, 8.9), and for a class of another unit, the cell the class keeps at
  // the position its declaration gives the property.
  auto TranslateStaticPropertyTarget(
      const hir::ClassStaticPropertyTarget& target) -> StaticStorageHome;

  // A method of a class as the callable it is, whichever unit defines the
  // class: a class of another unit declares here every method it published, in
  // the order it published them.
  auto TranslateMethodTarget(const hir::ClassMethodTarget& target)
      -> mir::CallableTarget;

  // The part a method takes in its class's dispatch table (LRM 8.20), as the
  // class declares it: an override names the slot of the behavior it
  // overrides, an introducer names itself, and a method in neither takes none.
  // It is read off the declaration rather than off any class's lowered shape,
  // so it is answered the same whatever order the classes are lowered in -- a
  // base declared in an enclosing scope may be lowered after the class
  // extending it.
  [[nodiscard]] auto DispatchRoleOf(const hir::ClassMethodTarget& method) const
      -> std::optional<mir::VirtualDispatchRole>;

  // The slot a behavior names, as a call dispatching on it and a method
  // answering an interface class's behavior name it. Every method a behavior
  // names is in dispatch.
  [[nodiscard]] auto VirtualSlotOf(const hir::ClassMethodTarget& behavior) const
      -> mir::VirtualSlot;

  // Receiver-less callable of a unit's namespace (LRM 26.3 package function or
  // task). A body of that same unit names the position its declaration sits at,
  // because it holds the arena; a body outside it has only the identifier the
  // namespace published, under which a body is declared here once.
  auto MakeNamespaceCallableTarget(const hir::NamespaceSubroutineRef& ref)
      -> mir::DirectTarget;

  // The cell a variable of a unit's namespace is (LRM 26.2), whose cell is of
  // `cell_type`: this unit's own, or one declared here for the identifier
  // another unit published, once per variable.
  auto NamespaceVariable(
      const hir::NamespaceValueRef& ref, mir::TypeId cell_type)
      -> mir::StaticVariableId;

  // A subroutine a unit published on the object its instances are (LRM 23.6,
  // 25.7): a virtual method of that unit's published class, which the class
  // the object is answers. A slot of this unit's own class is named by its
  // position, as any of its callables is; another unit's by the position its
  // signature gave the subroutine.
  [[nodiscard]] auto PublishedSubroutineSlot(
      hir::ScopeClassId scope_class, hir::PublishedCallableId callable) const
      -> mir::VirtualSlot;

  // Mints a fresh owner-site id for a synthesized binding origin -- a carrier a
  // lowering creates that has no source-level variable (an activation handle, a
  // non-blocking-assignment snapshot). The id only has to be unit-unique and
  // deterministic so the carrier's origin is a stable, collision-free key
  // across every synthesizer in the unit; a monotonic count
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

  // What the class `id` extends, read while bodies lower: off the shape this
  // unit settled for a class it defines, and off the declaration of one another
  // unit defines.
  [[nodiscard]] auto BaseOf(mir::ClassId id) const
      -> std::optional<mir::ClassRef> {
    if (const ClassShape* shape = declarations_.Find(id)) {
      return shape->base;
    }
    return unit_.GetClass(id).base;
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

  // The generic function that makes a closure in the storage a keeper of
  // deferred effects hands out and keeps it there, as C++'s `emplace` builds
  // its element where the container holds it: given the keeper and a closure
  // making the effect, it allocates the effect's storage, builds the effect
  // there by calling the maker, and commits it. The unit defines it the first
  // time a body asks for it.
  [[nodiscard]] auto EmplaceInto() -> mir::UnitCallableTarget;

 private:
  // What the class `hir_id` this unit declares became.
  [[nodiscard]] auto OwnTranslation(hir::ClassId hir_id) const
      -> const ClassTranslation& {
    const auto it = class_translations_.find(hir_id);
    if (it == class_translations_.end()) {
      throw InternalError(
          "UnitLowerer::OwnTranslation: only a class this unit declares is "
          "translated -- please report this as a bug");
    }
    return it->second;
  }

  // Declares in MIR a class another unit defines, as that unit published it.
  // Every class of another unit this unit read is declared, before anything
  // names one: a place reaching a member of a class an ancestor declares walks
  // the lineage over these declarations, so a class the walk only passes
  // through has to be among them, and which classes those are is not a
  // question any one reference can answer. Each takes its identity first, so
  // a declaration naming another names it by that identity.
  //
  // Which of them this unit depends on is a narrower set and is not decided
  // here: a signature is read as soon as the elaborating design asks anything
  // of it, so what is declared includes classes this unit names nowhere.
  auto DeclarePublishedClass(hir::ClassId cls) -> void;

  // A class this unit only names -- the type of a handle or an object never
  // reached into -- declared incomplete.
  auto DeclareIncompleteClass(
      mir::ClassId id, std::string unit_name, support::DefPath class_path)
      -> void;

  // Declares in MIR the class a scope of another unit is -- one of its
  // instances, or a generate block inside one -- once per class reached, with a
  // field for each thing the scope published. That class is a class of that
  // unit like any other, so what a reference to one reads is the same
  // declaration.
  auto DeclarePublishedScopeClass(
      hir::ScopeClassId hir_id, const hir::ScopeClassSignature& scope) -> void;

  // Lowers a scope whose root is an object type into the unit's top class,
  // which the unit then names as its root. `design_root` is present for the
  // design root alone, whose Initialize phase turns each namespace of the
  // design into a cross-unit install and initialize call (LRM 26.2 / 10.5) and
  // which states every construction table.
  auto PopulateModuleRoot(std::optional<DesignRootInputs> design_root)
      -> diag::Result<void>;

  // The callables a class this unit declares has beyond the methods it
  // declares, each at a position after them: a forward per interface class
  // behavior it answers by inheritance, then the bodies it gives `randomize()`.
  // Reads nothing but this class, so classes take theirs in any order and none
  // waits on another.
  auto TakeClassIdentities(hir::ClassId cls) -> ClassTranslation;

  // Publishes everything the unit declares before any root-scope body lowers:
  // every class identity and body, every interned type and the functions each
  // structure type answers a whole-value operation with, the default of each
  // type an array of any kind holds, this unit's record of each object it
  // reaches in another unit, and the prototype of every foreign symbol the unit
  // takes part in. Shared prologue of every unit
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

  // States every operation on a whole value of each union, tagged union and
  // fixed-size unpacked array the unit names (LRM 7.3, 7.4.2), and the default
  // of every type the unit holds an array of, whatever the array's kind
  // (LRM 7.4.5). Which types those are follows from the source's types alone,
  // every one of which is translated before any body lowers, so the set is
  // whole here and no body adds to it.
  void PublishTypeOperations();

  [[nodiscard]] auto TranslateType(const hir::Type& type) -> mir::Type;

  // The record type naming the declaration `source`, a structure or a union
  // of `kind` whose members are of `elements`. One this unit declares is
  // settled here and its methods once its type is interned, since a method
  // names the type of the value it is asked of; one another unit declares is
  // named by that declaration, and holding a value of it depends on that unit.
  [[nodiscard]] auto TranslateRecordType(
      const hir::TypeDeclarationRef& source, mir::RecordKind kind,
      std::vector<mir::TypeId> elements) -> mir::RecordType;

  // The types of a union's members, a `void` one holding no information.
  [[nodiscard]] auto UnionMemberTypes(const hir::UnpackedUnionType& src)
      -> std::vector<mir::TypeId>;

  // States the methods of the struct or the union `id` this unit declares from
  // `source`, over values of `type`, the interned type naming it.
  void DefineRecordMethods(
      const hir::Type& source, mir::RecordId id, mir::TypeId type);

  const hir::CompilationUnit* hir_;
  const diag::SourceManager* source_manager_;
  hir::ScopeInstantiations root_instantiations_;
  mir::CompilationUnit unit_;
  base::Translation<hir::TypeId, mir::TypeId> type_translations_;
  // The class each class the unit names is, of this unit or another, and what
  // each one this unit declares takes beyond that.
  base::Translation<hir::ClassId, mir::ClassId> class_identities_;
  std::map<hir::ClassId, ClassTranslation> class_translations_;
  // Where each record the unit names sits in its record registry, by the
  // declaration any unit names it by.
  std::map<mir::TypeDeclarationRef, mir::RecordId> record_translations_;
  // The callable and the cell each member of a unit's namespace this unit names
  // is, by that unit and the name the member was published under.
  std::map<std::pair<std::string, std::string>, mir::CallableId>
      namespace_callables_;
  std::map<std::pair<std::string, std::string>, mir::StaticVariableId>
      namespace_variables_;
  // The class each covergroup the unit names is written as, of this unit or
  // another.
  base::Translation<hir::CovergroupId, mir::ClassId> covergroup_identities_;
  // The class each scope class the unit names is, of this unit or another.
  std::map<hir::ScopeClassId, mir::ClassId> scope_class_translations_;
  // This unit's layout of each class a scope of another unit is, by the HIR
  // record of it.
  std::map<hir::ScopeClassId, ScopeClassLayout> scope_layouts_;
  std::map<mir::ClassId, PublishedScope> published_scopes_;
  std::uint32_t next_synthesized_site_ = 0;
  // What the declare stage settled about each class, read by every body that
  // names a peer. Lives only on the lowerer; the finished compilation unit
  // holds the only authoritative class representation.
  base::SymbolTable<mir::ClassId, ClassShape> declarations_;
  std::unordered_map<hir::TypeId, mir::CallableId> assignment_pattern_texts_;
  std::optional<mir::CallableId> emplace_into_;
};

}  // namespace lyra::lowering::hir_to_mir
