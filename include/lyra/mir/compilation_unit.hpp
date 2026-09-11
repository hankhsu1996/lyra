#pragma once

#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/external_unit_object.hpp"
#include "lyra/mir/external_unit_object_id.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/namespace_storage_phase.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// A unit-level static variable: a mutable value the unit's namespace owns with
// static storage -- one program-global cell, shared across the whole
// simulation, not a member of any instance (LRM 26.2 package variables, LRM
// 6.21 static lifetime). The mutable, observable counterpart of a
// `StaticConstantDecl`: a backend emits it as a namespace-scope observable
// cell, the storage dual of the unit's receiver-less callables. A body of the
// declaring unit reaches the cell by its position; another unit has only the
// identifier this one published, and spells `unit::name`. `type` is the
// observable-cell type; the
// declared value type is its inner value. The initializer is not here: it runs
// in the unit's synthesized initializer at time zero (LRM 10.5), the way a
// class member's initializer runs in its Initialize phase, never as a field on
// the declaration.
// It carries no name, for the reason a field carries none: the arena also takes
// the static-lifetime cells of bodies the unit's namespace declares, and those
// the source never wrote.
struct StaticVariableDecl {
  TypeId type;
};

// One entry of the relation between a unit's namespace and the storage it
// answers by name: the identifier the source declared the variable under, and
// the cell it reaches. This is also the unit's published surface for it -- what
// another unit has instead of the arena.
struct NamedStaticVariable {
  std::string name;
  StaticVariableId variable;
};

// The identifier `variable` answers to among `named`, or nothing where nothing
// names it.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedStaticVariable> named, StaticVariableId variable)
    -> std::optional<std::string_view> {
  for (const NamedStaticVariable& entry : named) {
    if (entry.variable == variable) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

// The storage `name` reaches among `named`, or nothing where the unit publishes
// no such identifier. The relation read the other way: a reference written
// inside the declaring unit arrives carrying what the source spelled, and what
// it names is a position in that unit's own arena.
[[nodiscard]] inline auto StaticVariableNamed(
    std::span<const NamedStaticVariable> named, std::string_view name)
    -> std::optional<StaticVariableId> {
  for (const NamedStaticVariable& entry : named) {
    if (entry.name == name) {
      return entry.variable;
    }
  }
  return std::nullopt;
}

// A unit whose instances are a tree of objects the runtime drives (LRM 23.3):
// the class at the root of that tree.
struct RootedTree {
  ClassId root;
};

// A unit that is a namespace rather than a hierarchy -- a package (LRM 26.2) or
// the compilation-unit scope (LRM 3.12.1). Such a namespace owns every cell of
// the program no instance holds: its own variables (LRM 26.2), the
// static-lifetime locals of its subroutines (LRM 6.21), and the
// type-associated cells of the classes it declares (LRM 8.9). These are the two
// bodies the design root calls at time zero to bring all of them up.
//
// `install_storage` gives every cell its declared representation and language
// default and fires nothing; `initialize_storage` then runs each value
// initializer through its cell (LRM 10.5). The design root runs the first for
// every unit of the design before the second for any, so an initializer that
// reads another unit's cell always reaches installed storage -- at worst a
// default. Every such unit publishes both -- one that owns no cell runs a body
// that brings up none -- so the design root calls both without first asking
// what this one supplied.
//
// Neither answers to a name. The source declares neither, and SystemVerilog
// leaves no spelling reserved to the compiler (LRM 5.6.1), so a word minted for
// them is a word the unit's own namespace could also answer; the design root
// reaches them by which of the two they are instead.
struct BroughtUpNamespace {
  CallableId install_storage;
  CallableId initialize_storage;
};

// What a unit contributes to the program it links into. A hierarchy's storage
// comes up with the instances that hold it and a namespace has no instances, so
// a unit is one or the other, and neither can be asked of a unit that is not
// it.
using UnitContent = std::variant<RootedTree, BroughtUpNamespace>;

// Named TypeIds the lowering and rendering reuse. Most are language or runtime
// atomic types (the literal `int` type, the 1-bit selector type, the `void`
// result of system tasks). `scope_ptr` and `coroutine_void` are not atomic:
// they are convenience aliases for canonical instances of composite types
// (`Pointer<Scope>`, `Coroutine<void>`) that nearly every scope and process
// materializes -- the interner gives them their identity, these fields only
// name them. Populated by `CompilationUnit`'s constructor; consumers read them
// off the unit.
struct BuiltinMirTypes {
  TypeId int_type;
  TypeId int_unsigned;
  TypeId integer;
  TypeId bit1;
  TypeId machine_bool;
  // The machine integer a runtime entry hands back as a plain value. It is the
  // widest one, so a narrower machine integer is reached by reading this as
  // that narrower type rather than by an entry of its own.
  TypeId machine_int64;
  // The machine word a packed value's storage is laid out in. A literal too
  // wide for one integer carrier states its bits as a run of these, which is
  // the same word the runtime's own planes are made of.
  TypeId machine_word;
  // The two machine floats a real-family value wraps: single precision for a
  // `shortreal`, double for a `real` or a `realtime` (LRM 6.12).
  TypeId machine_float32;
  TypeId machine_float64;
  TypeId string;
  TypeId void_type;
  TypeId realtime;
  TypeId time;
  TypeId effects;
  TypeId scope_ptr;
  // The object type an imported runtime-library class handle (LRM 9.7
  // `process`) references. A fixed library class named by its qualified name,
  // exactly as the scope class is.
  TypeId process_object;
  TypeId files;
  TypeId diagnostic;
  // The descriptor an integral type reaches a factory as. Every such operand
  // has this one type, whichever integral it describes: which integral it is
  // is what the operand names, not part of what type the operand is.
  TypeId packed_type;
  // One dimension of that descriptor, named so the stack a descriptor is built
  // from is spelled through the type dispatch like every other type.
  TypeId packed_range;
  TypeId channel_cancellation;
  TypeId print_item;
  TypeId print_literal_item;
  TypeId print_value_item;
  TypeId format_spec;
  TypeId format_arg;
  TypeId time_format;
  TypeId hierarchy_segment;
  TypeId trigger;
  TypeId observation;
  TypeId coroutine_void;
  TypeId wildcard_index;
};

struct CompilationUnit {
  // The unit's own name -- its module, package, or interface name. A backend
  // names the unit's emitted artifact from this, so a unit whose root is a
  // namespace rather than a class (a package) still has a stable identity
  // independent of any member.
  std::string name;
  TypePool types;
  BuiltinMirTypes builtins;
  // Every class declaration of this unit, owned here exactly once and reached
  // by its identity, with a declare-then-define lifecycle so a class can be
  // named before its body is built.
  base::Registry<Class, ClassId> classes;
  UnitContent content;
  // One entry per unit this one reaches an object of, with what each promised
  // taken into this unit's types, under the same declare-then-define lifecycle
  // a class has: a type may name one of these objects -- an interface port's
  // does -- so the identity exists before the members are filled in.
  base::Registry<ExternalUnitObject, ExternalUnitObjectId>
      external_unit_objects;
  // One entry per class of another unit this one reaches a property or a
  // behavior on. Found by the pair that names the class, which is the pair
  // every reference to one carries, so a reference and its record cannot come
  // apart.
  std::vector<ExternalClass> external_classes;
  // Callables the unit's namespace owns directly rather than through one of its
  // classes -- a package's functions and tasks (LRM 26.3), and both directions
  // of the DPI-C boundary (LRM 35.5): the prototype of every import the unit
  // takes part in, and the program-global symbol of every export it defines.
  // All are receiver-less. A DPI-C name is program-global, in a name space no
  // compilation-unit scope contains (LRM 35.4, 35.7), so no class ever owns
  // one. Which direction a foreign callable is reads off its body: an import is
  // the declaration the user's C defines, an export symbol the definition the
  // user's C calls. Where an export's subroutine belongs to a scope, the scope
  // publishes an entry per specialization and the symbol dispatches over those
  // entries, so the entry is not one of these and the symbol belongs to the
  // unit that reads the whole design. A class's own callables live on that
  // class; these are the unit-level namespace's, one scope up.
  base::Arena<CallableDecl, CallableId> callables;
  // The names this unit's namespace answers and which body each reaches (LRM
  // 26.3). A subroutine the source declared is here because another unit spells
  // it; a body the compiler synthesized is not.
  std::vector<NamedCallable> named_callables;
  // Static variables the unit's namespace owns directly rather than through one
  // of its classes -- a package's variables (LRM 26.2), one program-global cell
  // each, shared and reached by name. A class's own static storage lives on
  // that class; these are the unit-level namespace's, one scope up. Their
  // initializers run in the unit's synthesized initializer at time zero.
  base::Arena<StaticVariableDecl, StaticVariableId> static_variables;
  // The identifiers the source declared for storage this unit's namespace
  // holds. A package variable takes part; the cell a subroutine's
  // static-lifetime local keeps does not.
  std::vector<NamedStaticVariable> named_static_variables;
  // The foreign names this unit takes part in (LRM 35), in declaration order.
  // The program's foreign surface is the composition of these across units: a
  // name is program-global and lives in its own name space, so no single unit
  // owns it, and each states only its own part.
  std::vector<ForeignSymbol> foreign_surface;
  // Every compiler-generated nominal struct of this unit -- a promoted
  // automatic scope's storage. Its `StructId` is the struct's type identity; a
  // backend derives the C++ emission host from the struct's lexical synthesis
  // site.
  base::Registry<StructDecl, StructId> structs;
  // Every closure of this unit, one per closure site -- an anonymous concrete
  // callable value (capture fields plus one invoke). Its `ClosureId` is the
  // closure's type identity, in a separate registry from `structs`: a closure
  // is its own callable-value category, not a struct.
  base::Registry<ClosureDecl, ClosureId> closures;
  // Names of other compilation units this unit reaches a namespace-level symbol
  // of by name -- a package function or task called (LRM 26.3), or a package
  // variable read or written (LRM 26.2). Such a reference carries no
  // value-typed object of the target unit, so unlike an instantiation it
  // interns no `ExternalUnitObjectType`; a backend reads this dependency list
  // to emit the include and link edge to each referenced unit. Recorded once
  // per distinct unit name.
  std::vector<std::string> external_referenced_units;
  // Names of other compilation units this unit references a class of (LRM 8,
  // as a handle type, a `new` target, a field / method / static access, or a
  // super-extended base). A backend reads this list -- a separate axis from
  // `external_referenced_units`, because a package callable / variable and a
  // class member of another unit are independent include edges -- to emit the
  // include and link edge to each referenced unit. Recorded once per distinct
  // unit name.
  std::vector<std::string> external_class_units;
  // The units whose namespace storage this unit's own initializers read
  // directly (LRM 26.2 / 8.9 / 10.5) -- the by-name dependency the design root
  // uses to pick a stable order to bring namespaces up in. It records only
  // reads written directly in an initializer expression; a read reached through
  // a called function does not contribute yet. This is a preference, not a
  // correctness input: every cell is installed with its default before any
  // initializer runs, so a missed or cyclic dependency only means a read
  // observes a default, never an uninstalled cell. Empty for a unit that roots
  // an object tree and for one no initializer of which reaches another.
  std::vector<std::string> direct_initializer_unit_reads;

  CompilationUnit()
      : builtins{
            .int_type = types.Intern(
                Type{PackedArrayType{
                    .state_kind = IntegralStateKind::kTwoState,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}}}}),
            .int_unsigned = types.Intern(
                Type{PackedArrayType{
                    .state_kind = IntegralStateKind::kTwoState,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 31, .right = 0}}}}),
            .integer = types.Intern(
                Type{PackedArrayType{
                    .state_kind = IntegralStateKind::kFourState,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}}}}),
            .bit1 = types.Intern(
                Type{PackedArrayType{
                    .state_kind = IntegralStateKind::kTwoState,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 0, .right = 0}}}}),
            .machine_bool = types.Intern(Type{MachineBoolType{}}),
            .machine_int64 = types.Intern(
                Type{MachineIntType{
                    .width = MachineIntWidth::k64,
                    .signedness = Signedness::kSigned}}),
            .machine_word = types.Intern(
                Type{MachineIntType{
                    .width = MachineIntWidth::k64,
                    .signedness = Signedness::kUnsigned}}),
            .machine_float32 = types.Intern(
                Type{MachineFloatType{.width = MachineFloatWidth::k32}}),
            .machine_float64 = types.Intern(
                Type{MachineFloatType{.width = MachineFloatWidth::k64}}),
            .string = types.Intern(Type{StringType{}}),
            .void_type = types.Intern(Type{VoidType{}}),
            .realtime = types.Intern(Type{RealTimeType{}}),
            .time = types.Intern(
                Type{PackedArrayType{
                    .state_kind = IntegralStateKind::kFourState,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 63, .right = 0}}}}),
            .effects = types.Intern(Type{RuntimeEffectsType{}}),
            .scope_ptr = types.Intern(
                Type{PointerType{
                    .pointee = types.Intern(
                        Type{RuntimeClassType{
                            .symbol = "lyra::runtime::Scope"}}),
                    .ownership = PointerOwnership::kBorrowed}}),
            .process_object = types.Intern(
                Type{RuntimeClassType{
                    .symbol = "lyra::runtime::RuntimeProcess"}}),
            .files = types.Intern(Type{FilesType{}}),
            .diagnostic = types.Intern(Type{DiagnosticType{}}),
            .packed_type = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPackedType}}),
            .packed_range = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPackedRange}}),
            .channel_cancellation = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kChannelCancellation}}),
            .print_item = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintItem}}),
            .print_literal_item = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintLiteralItem}}),
            .print_value_item = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintValueItem}}),
            .format_spec = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kFormatSpec}}),
            .format_arg = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kFormatArg}}),
            .time_format = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kTimeFormat}}),
            .hierarchy_segment = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kHierarchySegment}}),
            .trigger = types.Intern(
                Type{RuntimeLibraryType{.kind = RuntimeLibraryKind::kTrigger}}),
            .observation = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kObservation}}),
            .coroutine_void = TypeId{},
            .wildcard_index = types.Intern(Type{WildcardIndexType{}}),
        } {
    // `Coroutine<void>` is the completion type of a process or void task. It is
    // built in the constructor body because it reads back the already-interned
    // `void_type`; the member-list entry above is an unused placeholder
    // overwritten here. The field is a convenience alias for the canonical
    // instance, not a deduplication mechanism -- interning `Coroutine<void>`
    // anywhere returns this same id.
    builtins.coroutine_void = types.Intern(
        mir::Type{mir::CoroutineType{.payload = builtins.void_type}});
  }

  [[nodiscard]] auto GetClass(ClassId id) const -> const Class& {
    return classes.Get(id);
  }

  auto DeclareClass() -> ClassId {
    return classes.Declare();
  }

  void DefineClass(ClassId id, Class value) {
    classes.Define(id, std::move(value));
  }

  [[nodiscard]] auto GetStruct(StructId id) const -> const StructDecl& {
    return structs.Get(id);
  }

  auto AddStruct(StructDecl value) -> StructId {
    const StructId id = structs.Declare();
    structs.Define(id, std::move(value));
    return id;
  }

  [[nodiscard]] auto GetClosure(ClosureId id) const -> const ClosureDecl& {
    return closures.Get(id);
  }

  // Mint a closure id before its declaration is built, so the closure's
  // receiver type can name it while the invoke body captures into its fields.
  auto DeclareClosure() -> ClosureId {
    return closures.Declare();
  }

  void DefineClosure(ClosureId id, ClosureDecl value) {
    closures.Define(id, std::move(value));
  }

  // Records a cross-unit namespace-symbol dependency, deduplicated. Called from
  // HIR-to-MIR when a reference names a receiver-less callable or a static
  // variable of another unit.
  //
  // This unit is not a dependency of itself, and that is settled here rather
  // than by each caller: whether a reference crosses the boundary is a property
  // of the list, so a site that reaches a sibling of its own namespace needs no
  // rule of its own and a site added later cannot forget one.
  void AddExternalReferencedUnit(std::string unit_name) {
    if (unit_name == name) {
      return;
    }
    for (const std::string& existing : external_referenced_units) {
      if (existing == unit_name) {
        return;
      }
    }
    external_referenced_units.push_back(std::move(unit_name));
  }

  // Records a cross-unit class-reference dependency, deduplicated. Called
  // from HIR-to-MIR when a class handle type, a `new`, a field / method /
  // static access, or a base extension names a class of another unit. Self is
  // excluded for the reason above.
  void AddExternalClassUnit(std::string unit_name) {
    if (unit_name == name) {
      return;
    }
    for (const std::string& existing : external_class_units) {
      if (existing == unit_name) {
        return;
      }
    }
    external_class_units.push_back(std::move(unit_name));
  }
};

// The tree this unit's instances are, or the namespace it brings up --
// whichever it is, and nothing where it is the other. Neither can be answered
// for a unit that is not it, so asking for the one a reader can use is also how
// it learns which kind it has; a reader that wants only the kind asks and drops
// the answer.
[[nodiscard]] inline auto RootedTreeOf(const CompilationUnit& unit)
    -> const RootedTree* {
  return std::get_if<RootedTree>(&unit.content);
}

[[nodiscard]] inline auto BroughtUpNamespaceOf(const CompilationUnit& unit)
    -> const BroughtUpNamespace* {
  return std::get_if<BroughtUpNamespace>(&unit.content);
}

// What reaches one body of a unit's namespace: the linkage name the source
// wrote in the DPI-C name space, which is program-global and belongs to no unit
// (LRM 35.4); the identifier the unit's own namespace answers (LRM 26.3); or
// which of the two bring-up entries it is.
//
// Every namespace body is one of these three. That is what separates a body
// from a cell, whose pool also takes the static-lifetime storage of the unit's
// subroutines and so holds storage nothing names -- a distinction worth having
// in front of you, because it is the whole reason a cell is reached by its
// position while a body is reached by what answers for it.
struct ReachedByLinkageName {
  std::string_view name;
};
struct ReachedByName {
  std::string_view name;
};
struct ReachedByStoragePhase {
  NamespaceStoragePhase phase;
};

using NamespaceReach =
    std::variant<ReachedByLinkageName, ReachedByName, ReachedByStoragePhase>;

// How `id` is reached. Every target names it from this one answer, so no two
// arrive at different names for one body and none works out for itself which
// kind of body it is looking at.
[[nodiscard]] inline auto NamespaceReachOf(
    const CompilationUnit& unit, CallableId id) -> NamespaceReach {
  const CallableDecl& callable = unit.callables.Get(id);
  if (callable.foreign.has_value()) {
    return ReachedByLinkageName{callable.foreign->foreign_name};
  }
  if (const BroughtUpNamespace* ns = BroughtUpNamespaceOf(unit)) {
    if (id == ns->install_storage) {
      return ReachedByStoragePhase{NamespaceStoragePhase::kInstall};
    }
    if (id == ns->initialize_storage) {
      return ReachedByStoragePhase{NamespaceStoragePhase::kInitialize};
    }
  }
  if (const std::optional<std::string_view> name =
          NameOf(unit.named_callables, id)) {
    return ReachedByName{*name};
  }
  throw InternalError(
      "NamespaceReachOf: a unit's namespace holds a body that answers to no "
      "identifier, no linkage name and neither bring-up entry, so nothing "
      "could call it -- please report this as a bug");
}

[[nodiscard]] inline auto MakeStringLiteral(
    TypeId string_type, std::string text) -> Expr {
  return Expr{
      .data = StringLiteral{.value = std::move(text)}, .type = string_type};
}

// The component type a tagged union carries at `tag_index` (LRM 7.3.2).
// Positions are the tag, so a component is reached by index and never by
// type -- two members may share one type.
[[nodiscard]] inline auto TaggedComponentType(
    const CompilationUnit& unit, TypeId tagged_union,
    base::ComponentIndex tag_index) -> TypeId {
  const auto* tu = unit.types.Get(tagged_union).As<TaggedUnionType>();
  if (tu == nullptr) {
    throw InternalError("TaggedComponentType: type is not a tagged union");
  }
  if (tag_index.value >= tu->members.size()) {
    throw InternalError("TaggedComponentType: tag index out of range");
  }
  return tu->members[tag_index.value].type;
}

}  // namespace lyra::mir
