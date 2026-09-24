#pragma once

#include <algorithm>
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
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/minted_entry.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor_pool.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_build.hpp"

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
// what the unit promised of one of those objects, the class at the root of the
// tree that realizes it, and the one body that brings one into existence.
//
// The promise is the whole of what another unit may name here. A design element
// exists to be instantiated and wired (LRM 23.2.1), so everything else it
// declares -- the class realizing the promise, the scopes below it, and any
// class of the source language it declares inside itself -- is its own, and no
// referrer has a name for one.
//
// That body exists because a unit instantiating this one consumed what this one
// promised, and a promise states what may be reached and never how much storage
// an object takes -- so an instantiator cannot make one and asks for one
// instead. The party that builds the design's tops asks the same way, having no
// more than any other referrer. It answers to no name, which is why the unit
// holds it as the body it is rather than among the bodies an identifier
// reaches.

// What one unit read of another's signature. A unit publishes its namespace --
// the cells and bodies it declares outside any class of it (LRM 26.2) -- and
// each class it promised, and a referrer reads one of those rather than the
// whole, so what it depends on is that much and no more.
struct ConsumedNamespace {
  std::string unit_name;

  auto operator==(const ConsumedNamespace&) const -> bool = default;
};

struct ConsumedClass {
  std::string unit_name;
  std::string class_name;

  auto operator==(const ConsumedClass&) const -> bool = default;
};

using ConsumedSignature = std::variant<ConsumedNamespace, ConsumedClass>;

// Which unit a consumption reached, whichever of the two it was.
[[nodiscard]] inline auto UnitConsumed(const ConsumedSignature& consumed)
    -> const std::string& {
  return std::visit(
      [](const auto& one) -> const std::string& { return one.unit_name; },
      consumed);
}

struct RootedTree {
  ClassId promise;
  ClassId root;
  CallableId object_entry;
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
// Neither answers to a name, which is why the unit holds each as the body it is
// rather than among the bodies an identifier reaches.
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
  // What an unpacked array's description is: its declared range, which an
  // operation walking the array in its declared coordinates is handed.
  TypeId unpacked_range;
  // What an enumeration's description is: its members in declared order, which
  // the questions LRM 6.19.5 and 6.24.2 ask about a value are answered against.
  TypeId enumeration;
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
  // The values this unit settles before the program runs, each held once and
  // named by every occurrence: the constant integral values the source wrote,
  // and what an operation on a value asks of its declaration. Both fill as
  // bodies are lowered, and `builds` says how each one is brought into
  // existence -- settled once, when the unit is finished, so every consumer
  // reads one finished set.
  IntegralConstantPool integral_constants;
  TypeDescriptorPool type_descriptors;
  UnitValueBuilds builds;
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
  // classes -- a package's functions and tasks (LRM 26.3), both directions of
  // the DPI-C boundary (LRM 35.5) (the prototype of every import the unit takes
  // part in, and the program-global symbol of every export it defines), and the
  // type-associated functions the compiler synthesizes for the readings a type
  // decides about a value of it (LRM 6.19.5, 21.2.1.6).
  // All are receiver-less, which is what puts them here rather than on a class:
  // a type-associated function takes the value and no object, so no class owns
  // one even where the unit has classes, and every unit has a namespace whether
  // or not it has any. A DPI-C name is program-global, in a name space no
  // compilation-unit scope contains (LRM 35.4, 35.7), so no class ever owns
  // one. Which direction a foreign callable is reads off its body: an import is
  // the declaration the user's C defines, an export symbol the definition the
  // user's C calls. Where an export's subroutine belongs to a scope, the scope
  // publishes an entry per specialization and the symbol dispatches over those
  // entries -- a definition naming nothing this unit owns, which is why neither
  // the entry nor the symbol is one of these. A class's own callables live on
  // that class; these are the unit-level namespace's, one scope up.
  // Identity is minted separately from content, because a body here is named
  // before it exists: a subroutine of this namespace may call a sibling the
  // source declared after it, or itself, and what such a call names is the
  // position.
  base::Registry<CallableDecl, CallableId> callables;
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
  // The foreign names this unit declares on a scope (LRM 35.5.3), each named
  // once and in declaration order. A name is program-global and lives in its
  // own name space, and a scope's entry is compiled once per specialization of
  // that scope, so none of those entries is the symbol -- this is what the unit
  // states of the name itself: the prototype a foreign source compiles against
  // and the definition it links to. What a unit's own namespace owns is not
  // among these: its callable is the symbol and already says so.
  //
  // Being on this list rather than in the pool above is the whole statement of
  // which of the two a name is. A definition here names only the name and the
  // prototype, so every unit declaring such a scope writes the same one and the
  // party assembling the program keeps one; a definition up there calls that
  // unit's own subroutine and no other artifact can write it.
  std::vector<ForeignScopeEntry> foreign_scope_entries;
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
  // Every other compilation unit whose signature this one consumed, once each.
  // That set is the whole of what this unit depends on another for, so whoever
  // asks what an edit reaches asks this and nothing else -- an instantiated
  // child, a package symbol named by a body, a class reached into, a base
  // extended, all arrive here rather than each in a list of its own. Each names
  // the part it read rather than the unit whole, so a change to a class nobody
  // read reaches nobody.
  std::vector<ConsumedSignature> consumed_signatures;

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
            .unpacked_range = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kUnpackedRange}}),
            .enumeration = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kEnumeration}}),
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

  // Records what this unit read of another's signature. Called from HIR-to-MIR
  // wherever a promise is read; the same part reached twice is one entry.
  //
  // This unit is not a dependency of itself, and that is settled here because
  // one caller cannot settle it: the design root realizes a plan of unit names
  // it does not inspect, so it asks for every one of them and the set decides
  // which are outside. A site that does read what it reaches names the position
  // instead and never arrives here at all.
  void ConsumeNamespaceOf(std::string unit_name) {
    if (unit_name == name) {
      return;
    }
    Consume(ConsumedNamespace{.unit_name = std::move(unit_name)});
  }

  void ConsumeClassOf(std::string unit_name, std::string class_name) {
    if (unit_name == name) {
      return;
    }
    Consume(
        ConsumedClass{
            .unit_name = std::move(unit_name),
            .class_name = std::move(class_name)});
  }

  void Consume(ConsumedSignature consumed) {
    if (std::ranges::find(consumed_signatures, consumed) ==
        consumed_signatures.end()) {
      consumed_signatures.push_back(std::move(consumed));
    }
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

// Whether another unit can name this class, which is what a unit contributes to
// the program deciding. A design element publishes what it promised of its
// object and nothing else it declares inside (LRM 23.2.1); a namespace unit
// publishes the classes it declares (LRM 26.2), which are the ones the source
// named. Every consumer deciding which artifact a declaration belongs in asks
// here, so none of them answers it a second way.
[[nodiscard]] inline auto IsPromised(const CompilationUnit& unit, ClassId id)
    -> bool {
  if (const RootedTree* tree = RootedTreeOf(unit)) {
    return id == tree->promise;
  }
  return unit.GetClass(id).name.has_value();
}

// The classes of the program this one's declaration rests on: the class it
// extends (LRM 8.13) and each interface it commits to (LRM 8.26), counted only
// where that is a class some unit declares. A class the runtime library defines
// is not one, because the library is there before any unit is.
//
// This is the whole of what a declaration has to have behind it. Everything
// else a class names -- the type of a property, of an argument, of a result --
// is reached through a reference, so the named class may still be nothing but a
// name when this one is written. A consumer that has to put declarations in an
// order reads this and needs nothing else.
[[nodiscard]] inline auto RestsOnDeclaredClasses(const Class& cls)
    -> std::vector<ClassRef> {
  std::vector<ClassRef> resting;
  const auto take = [&resting](const ClassRef& ref) {
    if (!std::holds_alternative<RuntimeClassRef>(ref)) {
      resting.push_back(ref);
    }
  };
  if (cls.base.has_value()) {
    take(*cls.base);
  }
  for (const ClassRef& implemented : cls.implements) {
    take(implemented);
  }
  return resting;
}

// What spells one body of a unit's namespace: the linkage name the source wrote
// in the DPI-C name space, which is program-global and belongs to no unit (LRM
// 35.4); the identifier the unit's own namespace answers (LRM 26.3); which of
// the two bring-up entries it is; or, where nothing names it, the position it
// sits at.
//
// The position is the general case and the three before it are the special
// ones. A namespace holds what the source declared and what the compiler
// synthesized alike, in one pool for bodies and one for cells; the position is
// what every entry of either has, and an identifier is a relation over that
// position, stated only where the source wrote one. So an entry answering to
// nothing is an answer rather than a case to work around.
struct ReachedByLinkageName {
  std::string_view name;
};
struct ReachedByName {
  std::string_view name;
};
struct ReachedByMintedEntry {
  MintedEntry entry;
};
// A body nothing names, spelled from where it sits. A target mints the spelling
// into a range no source name reaches; nothing outside this unit can ask for
// it, which is why a position suffices.
struct ReachedByPosition {
  CallableId slot;
};

using NamespaceReach = std::variant<
    ReachedByLinkageName, ReachedByName, ReachedByMintedEntry,
    ReachedByPosition>;

// How `id` is spelled. Every target names it from this one answer, so no two
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
      return ReachedByMintedEntry{MintedEntry::kInstallStorage};
    }
    if (id == ns->initialize_storage) {
      return ReachedByMintedEntry{MintedEntry::kInitializeStorage};
    }
  }
  if (const RootedTree* tree = RootedTreeOf(unit)) {
    if (id == tree->object_entry) {
      return ReachedByMintedEntry{MintedEntry::kMakeObject};
    }
  }
  if (const std::optional<std::string_view> name =
          NameOf(unit.named_callables, id)) {
    return ReachedByName{*name};
  }
  return ReachedByPosition{id};
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
