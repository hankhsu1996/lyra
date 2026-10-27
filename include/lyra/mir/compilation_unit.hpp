#pragma once

#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/bring_up_entry.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/enum_table_pool.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

// A unit-level static variable: a mutable value the unit's namespace owns with
// static storage -- one program-global cell, shared across the whole
// simulation, not a member of any instance (LRM 26.2 package variables, LRM
// 6.21 static lifetime). A backend emits it as a namespace-scope observable
// cell, the storage dual of the unit's receiver-less callables. A body of a
// unit reaches the cell by its position, the unit owning it and one declaring
// it from what that unit published alike. `type` is the observable-cell type;
// the declared value type is its inner value. The initializer is not here: it
// runs in the unit's synthesized initializer at time zero (LRM 10.5), the way a
// class member's initializer runs in its Initialize phase, never as a field on
// the declaration.
// It carries no name, for the reason a field carries none: the arena also takes
// the static-lifetime cells of bodies the unit's namespace declares, and those
// the source never wrote. `defining_unit`, where present, names the unit whose
// namespace holds the cell, which this unit only declares (LRM 26.3).
struct StaticVariableDecl {
  TypeId type;
  std::optional<std::string> defining_unit;
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

// What one unit read of another's signature. A unit publishes its namespace --
// the cells and bodies it declares outside any class of it (LRM 26.2) -- each
// class it published, and for each body it is compiled to the entry that makes
// an object of that body. A referrer reads one of those rather than the whole,
// so what it depends on is that much and no more.
struct ConsumedNamespace {
  std::string unit_name;

  auto operator==(const ConsumedNamespace&) const -> bool = default;
};

struct ConsumedClass {
  std::string unit_name;
  support::DefPath class_path;

  auto operator==(const ConsumedClass&) const -> bool = default;
};

struct ConsumedObjectEntry {
  std::string unit_name;
  std::string body_name;

  auto operator==(const ConsumedObjectEntry&) const -> bool = default;
};

using ConsumedSignature =
    std::variant<ConsumedNamespace, ConsumedClass, ConsumedObjectEntry>;

// Whether each unit naming `type` states its operations itself: a fixed-size
// unpacked array, which the language identifies by its element and its range
// rather than by a declaration (LRM 6.22.1 f), so no declaration states them
// once for every unit, as a structure's and a union's does.
[[nodiscard]] inline auto OperationsAreStatedPerUnit(const Type& type) -> bool {
  return type.Is<UnpackedArrayType>();
}

// The body a unit states for the operation `answers` at `type`. A type whose
// operations are stated per unit has every operation on its whole value stated
// so (LRM 7.4.2), as rustc instantiates a generic function in each crate that
// uses it: an array's is each element's in turn. Every type an array of
// the unit has as its element, of whatever kind, has its default stated here
// too (LRM Table 6-7, Table 7-1): what a read through an index naming no
// element answers (LRM 7.4.5) and what an array that grows fills each new
// position with (LRM 7.5.1). A container's default depends on its element
// type, which the library's one type of each container does not know.
//
// A use in the program calls one by the type and the operation; code compiled
// before the type existed -- what holds a value of it, what an array grows
// with -- reaches it through the type's table instead, which is why a body
// nothing in the program calls is stated as well.
struct TypeOperation {
  TypeId type;
  support::ValueOperation answers;
  CallableCode code;
};

// Every type some array of `types` has as its element, whatever the array's
// kind, each once and in the order the pool first names it: the types a unit
// states a default for.
[[nodiscard]] inline auto ArrayElementTypes(const TypePool& types)
    -> std::vector<TypeId> {
  std::vector<bool> taken(types.size(), false);
  std::vector<TypeId> elements;
  for (const Type& type : types) {
    const std::optional<TypeId> element = type.ContainerElementType();
    if (element.has_value() && !taken[element->value]) {
      taken[element->value] = true;
      elements.push_back(*element);
    }
  }
  return elements;
}

// A unit whose instances are a tree of objects the runtime drives (LRM 23.3):
// the class at the root of the tree, the one body that brings one into
// existence, and, for each of its block bodies, the one that brings a block of
// it into existence (LRM 27), with where that block body stands among the
// unit's.
//
// That body exists because what a unit publishes of its object states what may
// be reached and never how much storage the object takes -- so an instantiator
// cannot make one and asks for one instead. The party that builds the design's
// tops asks the same way, having no more than any other referrer. A block's
// entry exists because which body stands at a place is the object's, so the
// scope building the block names none of them and calls the entry its table
// holds. None answers to a name, which is why the unit holds each as the body
// it is rather than among the bodies an identifier reaches.
struct RootedTree {
  ClassId root;
  CallableId object_entry;
  std::map<CallableId, BlockBodyOrdinal> block_entries;
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
  // An unsigned 64-bit machine word, for a quantity read as bits rather than
  // as a number: a set of positions is stated as a sequence of these, one bit
  // per position.
  TypeId machine_word;
  // The two machine floats a real-family value wraps: single precision for a
  // `shortreal`, double for a `real` or a `realtime` (LRM 6.12).
  TypeId machine_float32;
  TypeId machine_float64;
  // A borrowed C string, the text a runtime entry is handed as plain
  // characters.
  TypeId machine_c_string;
  TypeId string;
  // The run of bits a stream is where some part's width is the running
  // program's (LRM 6.24.3, 11.4.14).
  TypeId bit_vec;
  TypeId void_type;
  // A `real`, which a `realtime` is as well (LRM 6.12).
  TypeId real;
  TypeId time;
  TypeId effects;
  TypeId scope_ptr;
  // The object type an imported runtime-library class handle (LRM 9.7
  // `process`) references, one of the classes the runtime library defines, as
  // the scope class is.
  TypeId process_object;
  // What a source event is: a managed reference to the synchronization object
  // the runtime library defines (LRM 6.17), or to none.
  TypeId event;
  // The problem a call of `randomize()` fills (LRM 18.6): a managed reference
  // to the object the runtime library defines for it.
  TypeId constraint_problem;
  TypeId files;
  TypeId diagnostic;
  // An enumeration's member table: its members in declared order, which the
  // questions LRM 6.19.5 and 6.24.2 ask about a value are answered against.
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
  TypeId read_report;
  // What a function is handed where a call of it is to report what it reads
  // (LRM 9.4.2), and null where it only runs.
  TypeId read_report_ptr;
  TypeId wait;
  // What each stop at a wait is handed.
  TypeId wait_ptr;
  // The keepers a deferred closure is emplaced in: where a region keeps its
  // effects, and where a violation report is kept.
  TypeId deferred_effects_ptr;
  TypeId violation_report_keeper_ptr;
  TypeId coroutine_void;
  TypeId wildcard_index;
};

struct CompilationUnit {
  // The unit's own name -- its module, package, or interface name, with what
  // was fixed for it. Every declaration another unit may name is named under
  // it, so a unit whose root is a namespace rather than a class (a package)
  // still has a stable identity independent of any member.
  std::string name;
  // Which body of the unit this is: the unit's name with the digest of the
  // values its instances' code uses, or the unit's name alone where that code
  // uses none. Its bodies each state the declarations the unit names, alike,
  // and what only one body holds -- the classes realizing its scopes, their
  // entries, the artifact it is emitted as -- is named under this.
  std::string body_name;
  // The identifier the source declares the unit under, which several units
  // share where one definition is compiled once per specialization, and which
  // a unit the source gave no identifier lacks. It is what a person is told
  // the compiler is working on, and the design element name `$printtimescale`
  // prints (LRM 20.4.2); nothing compiled is named from it.
  std::string source_name;
  TypePool types;
  // The values this unit settles before the program runs, each held once and
  // named by every occurrence: the constant integral values its bodies were
  // built with, and the member tables of the enumerations they ask about. Both
  // fill as bodies are lowered.
  IntegralConstantPool integral_constants;
  EnumTablePool enum_tables;
  BuiltinMirTypes builtins;
  // Every class this unit names, owned here exactly once and reached by its
  // identity, with a declare-then-define lifecycle so a class can be named
  // before its body is built: the classes it defines, and those of another
  // unit it reaches -- including the class another unit's instances are --
  // each declared with what that unit published (`Class::defining_unit`).
  base::Registry<Class, ClassId> classes;
  UnitContent content;
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
  // The name each body a subroutine the source declared is reached by (LRM
  // 26.3): one of this unit's namespace, or one of another unit's this unit
  // declares, under the name that unit published it. A body the compiler
  // synthesized is not here.
  std::vector<NamedCallable> named_callables;
  // The operations this unit states for each type it names whose operations
  // are stated per unit, and the default of each type it holds an array of.
  // One per type and operation.
  std::vector<TypeOperation> type_operations;
  // Static variables the unit's namespace owns directly rather than through one
  // of its classes -- a package's variables (LRM 26.2), one program-global cell
  // each, shared and reached by name. A class's own static storage lives on
  // that class; these are the unit-level namespace's, one scope up. Their
  // initializers run in the unit's synthesized initializer at time zero.
  base::Arena<StaticVariableDecl, StaticVariableId> static_variables;
  // The identifiers the source declared for the cells among
  // `static_variables`: a package variable of this unit's namespace, or of
  // another unit's this unit declares, under the name that unit published it.
  // The cell a subroutine's static-lifetime local keeps takes no part.
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
  // Every structure and union this unit names, settled where its type is
  // translated: the ones its source declares, and those of another unit it
  // holds a value of, declared with what that unit published. A record type
  // names one of these by where it sits.
  base::Registry<RecordDecl, RecordId> records;
  // The methods of each of those, by the same identity, stated once its type
  // is interned, since a method reaches the record's own members; none for a
  // record another unit declares, which states its own.
  base::Registry<RecordMethods, RecordId> record_methods;
  // Every closure of this unit, one per closure site -- an anonymous concrete
  // callable value (capture fields plus one invoke). Its `ClosureId` is the
  // closure's type identity, in a separate registry from `records`: a
  // closure is its own callable-value category, not a record.
  base::Registry<ClosureDecl, ClosureId> closures;
  // How large a value of each type asked about is, worked out once per type and
  // kept, as clang keeps a record's layout beside the declarations it follows.
  // A structure's layout follows every member and is asked at every value of
  // it, so working it out each time costs a design's member count at each
  // value. A layout follows only what the type's declaration states, a
  // capture counting as it is held, so no rewrite changes one; a unit is
  // lowered by one thread, so the answers are kept wherever the unit is read.
  mutable std::unordered_map<TypeId, support::Layout> layouts;
  // Which phase the unit is in, which only the conversions between them move.
  Phase phase = Phase::kBuilt;

  CompilationUnit()
      : builtins{
            .int_type = types.Intern(
                Type{IntegralType{
                    .bit_width = 32,
                    .signedness = Signedness::kSigned,
                    .state_kind = IntegralStateKind::kTwoState}}),
            .int_unsigned = types.Intern(
                Type{IntegralType{
                    .bit_width = 32,
                    .signedness = Signedness::kUnsigned,
                    .state_kind = IntegralStateKind::kTwoState}}),
            .integer = types.Intern(
                Type{IntegralType{
                    .bit_width = 32,
                    .signedness = Signedness::kSigned,
                    .state_kind = IntegralStateKind::kFourState}}),
            .bit1 = types.Intern(
                Type{IntegralType{
                    .bit_width = 1,
                    .signedness = Signedness::kUnsigned,
                    .state_kind = IntegralStateKind::kTwoState}}),
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
            .machine_c_string = types.Intern(Type{MachineCStringType{}}),
            .string = types.Intern(Type{StringType{}}),
            .bit_vec = types.Intern(
                Type{RuntimeLibraryType{.kind = RuntimeLibraryKind::kBitVec}}),
            .void_type = types.Intern(Type{VoidType{}}),
            .real = types.Intern(Type{RealType{}}),
            .time = types.Intern(
                Type{IntegralType{
                    .bit_width = 64,
                    .signedness = Signedness::kUnsigned,
                    .state_kind = IntegralStateKind::kFourState}}),
            .effects = types.Intern(Type{RuntimeEffectsType{}}),
            .scope_ptr = types.Intern(
                Type{PointerType{
                    .pointee = types.Intern(
                        Type{RuntimeClassType{
                            .which = support::RuntimeClass::kScope}}),
                    .ownership = PointerOwnership::kBorrowed}}),
            .process_object = types.Intern(
                Type{RuntimeClassType{
                    .which = support::RuntimeClass::kProcess}}),
            .event = types.Intern(
                Type{ManagedRefType{
                    .pointee = types.Intern(
                        Type{RuntimeClassType{
                            .which = support::RuntimeClass::
                                kSynchronizationObject}})}}),
            .constraint_problem = types.Intern(
                Type{ManagedRefType{
                    .pointee = types.Intern(
                        Type{RuntimeClassType{
                            .which =
                                support::RuntimeClass::kConstraintProblem}})}}),
            .files = types.Intern(Type{FilesType{}}),
            .diagnostic = types.Intern(Type{DiagnosticType{}}),
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
            .read_report = types.Intern(
                Type{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kReadReport}}),
            .read_report_ptr = TypeId{},
            .wait = types.Intern(
                Type{RuntimeLibraryType{.kind = RuntimeLibraryKind::kWait}}),
            .wait_ptr = TypeId{},
            .deferred_effects_ptr = types.Intern(
                Type{PointerType{
                    .pointee = types.Intern(
                        Type{RuntimeLibraryType{
                            .kind = RuntimeLibraryKind::kDeferredEffects}}),
                    .ownership = PointerOwnership::kBorrowed}}),
            .violation_report_keeper_ptr = types.Intern(
                Type{PointerType{
                    .pointee = types.Intern(
                        Type{RuntimeLibraryType{
                            .kind =
                                RuntimeLibraryKind::kViolationReportKeeper}}),
                    .ownership = PointerOwnership::kBorrowed}}),
            .coroutine_void = TypeId{},
            .wildcard_index = types.Intern(Type{WildcardIndexType{}}),
        } {
    // `Coroutine<void>` is the completion type of a process or void task, a
    // pointer to a read report is what a function is handed, and a pointer to a
    // wait is what a stop is handed. Each is built in the constructor body
    // because each reads back an already-interned type; their member-list
    // entries above are unused placeholders overwritten here.
    // The field is a convenience alias for the canonical instance, not a
    // deduplication mechanism -- interning `Coroutine<void>` anywhere returns
    // this same id.
    builtins.coroutine_void = types.Intern(
        mir::Type{mir::CoroutineType{.payload = builtins.void_type}});
    builtins.read_report_ptr = types.Intern(
        mir::Type{mir::PointerType{
            .pointee = builtins.read_report,
            .ownership = mir::PointerOwnership::kBorrowed}});
    builtins.wait_ptr = types.Intern(
        mir::Type{mir::PointerType{
            .pointee = builtins.wait,
            .ownership = mir::PointerOwnership::kBorrowed}});
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

  [[nodiscard]] auto GetRecordMethods(RecordId id) const
      -> const RecordMethods& {
    return record_methods.Get(id);
  }
  [[nodiscard]] auto GetRecord(RecordId id) const -> const RecordDecl& {
    return records.Get(id);
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
// the program deciding. Every class another unit names carries the path it is
// named by: what a design element published of its scopes, the classes it and
// a namespace unit declare (LRM 23.6, 26.2). A class the lowering builds for
// its own use answers to none. Every consumer deciding which artifact a
// declaration belongs in asks here, so none of them answers it a second way.
[[nodiscard]] inline auto IsPublished(const CompilationUnit& unit, ClassId id)
    -> bool {
  return support::PathOf(unit.GetClass(id).identity) != nullptr;
}

// Whether another unit defines this class, which this unit only declares. A
// declaration of another unit's class is complete from the moment it is taken,
// before any body lowers, while a class this unit defines is filled in as its
// bodies lower, so this can be asked of either at any point.
[[nodiscard]] inline auto IsDefinedElsewhere(
    const CompilationUnit& unit, ClassId id) -> bool {
  return unit.classes.IsDefined(id) &&
         unit.GetClass(id).defining_unit.has_value();
}

// Whether every body the unit is compiled to states this class alike, so that
// a program holds one of them whichever of the bodies it links: what the
// unit's declarations alone build, rather than what one body's code adds.
//
// That is what a scope of the design hierarchy published -- an instance's
// class, or a generate block's. A class the source declared reaches the scope
// declaring it as that scope's body compiled it, and a scope declaring one has
// one body.
[[nodiscard]] inline auto IsStatedByEveryBody(
    const CompilationUnit& unit, ClassId id) -> bool {
  const Class& cls = unit.GetClass(id);
  return support::PathOf(cls.identity) != nullptr && cls.base.has_value() &&
         std::holds_alternative<ObjectTreeRootRef>(*cls.base);
}

// The class some unit declares that `cls` extends, where it extends one -- the
// class an object of it is also an object of (LRM 8.13). Neither root is
// declared by any unit -- the design hierarchy's, nor the one every object the
// program builds extends: both are there before any unit is, declare no member
// of the source language, and hold no definition a class could step to.
[[nodiscard]] inline auto DeclaredBase(const Class& cls)
    -> std::optional<ClassId> {
  if (!cls.base.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](const ClassId& declared) -> std::optional<ClassId> {
            return declared;
          },
          [](const ObjectTreeRootRef&) -> std::optional<ClassId> {
            return std::nullopt;
          },
          [](const ManagedObjectRootRef&) -> std::optional<ClassId> {
            return std::nullopt;
          }},
      *cls.base);
}

// The classes of the program this one's declaration rests on: the class it
// extends (LRM 8.13), where some unit declares it, and each interface it
// commits to (LRM 8.26).
//
// This is the whole of what a declaration has to have behind it. Everything
// else a class names -- the type of a property, of an argument, of a result --
// is reached through a reference, so the named class may still be nothing but a
// name when this one is written. A consumer that has to put declarations in an
// order reads this and needs nothing else.
[[nodiscard]] inline auto RestsOnDeclaredClasses(const Class& cls)
    -> std::vector<ClassId> {
  std::vector<ClassId> resting;
  if (const std::optional<ClassId> base = DeclaredBase(cls)) {
    resting.push_back(*base);
  }
  resting.insert(resting.end(), cls.implements.begin(), cls.implements.end());
  return resting;
}

// The unit an entry is defined by: the one its `defining_unit` names, or `unit`
// itself where it names none. A reference, so a name built over it outlives
// the call.
[[nodiscard]] inline auto DefiningUnitOf(
    const CompilationUnit& unit,
    const std::optional<std::string>& defining_unit) -> const std::string& {
  return defining_unit.has_value() ? *defining_unit : unit.name;
}

// What spells one body of a unit's namespace: the linkage name the source wrote
// in the DPI-C name space, which is program-global and belongs to no unit (LRM
// 35.4); the identifier the unit's own namespace answers (LRM 26.3); which of
// the two bring-up entries it is; being the entry that makes an object of the
// body this unit is compiled to, or a block of one of its block bodies; the
// unit and identifier another unit's namespace publishes it under, for one
// this unit only declares; or, where nothing names it, the position it sits
// at.
//
// The position is the general case and the five before it are the special
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
struct ReachedByBringUpEntry {
  BringUpEntry entry;
};
// Each body a unit is compiled to has its own, and one for each of its block
// bodies, so the entry is spelled from the unit, the body and, for a block
// body, where it stands among the body's.
struct ReachedAsObjectEntry {
  std::optional<BlockBodyOrdinal> block;
};
// A subroutine another unit's namespace defines, spelled as that unit spells
// it (LRM 26.3).
struct ReachedInAnotherUnit {
  std::string_view unit_name;
  std::string_view name;
};
// A body nothing names, spelled from where it sits. A target mints the spelling
// into a range no source name reaches; nothing outside this unit can ask for
// it, which is why a position suffices.
struct ReachedByPosition {
  CallableId slot;
};

using NamespaceReach = std::variant<
    ReachedByLinkageName, ReachedByName, ReachedByBringUpEntry,
    ReachedAsObjectEntry, ReachedInAnotherUnit, ReachedByPosition>;

// How `id` is spelled. Every target names it from this one answer, so no two
// arrive at different names for one body and none works out for itself which
// kind of body it is looking at.
[[nodiscard]] inline auto NamespaceReachOf(
    const CompilationUnit& unit, CallableId id) -> NamespaceReach {
  const CallableDecl& callable = unit.callables.Get(id);
  if (callable.defining_unit.has_value()) {
    const std::optional<std::string_view> name =
        NameOf(unit.named_callables, id);
    if (!name.has_value()) {
      throw InternalError(
          "mir: a subroutine of another unit's namespace is declared with no "
          "name to reach it by -- please report this as a bug");
    }
    return ReachedInAnotherUnit{
        .unit_name = *callable.defining_unit, .name = *name};
  }
  if (callable.foreign.has_value()) {
    return ReachedByLinkageName{callable.foreign->foreign_name};
  }
  if (const BroughtUpNamespace* ns = BroughtUpNamespaceOf(unit)) {
    if (id == ns->install_storage) {
      return ReachedByBringUpEntry{BringUpEntry::kInstallStorage};
    }
    if (id == ns->initialize_storage) {
      return ReachedByBringUpEntry{BringUpEntry::kInitializeStorage};
    }
  }
  if (const RootedTree* tree = RootedTreeOf(unit)) {
    if (id == tree->object_entry) {
      return ReachedAsObjectEntry{.block = std::nullopt};
    }
    if (const auto block = tree->block_entries.find(id);
        block != tree->block_entries.end()) {
      return ReachedAsObjectEntry{.block = block->second};
    }
  }
  if (const std::optional<std::string_view> name =
          NameOf(unit.named_callables, id)) {
    return ReachedByName{*name};
  }
  return ReachedByPosition{id};
}

// The body this unit states for `answers` at `type`, or null where it states
// none.
[[nodiscard]] inline auto FindTypeOperation(
    const CompilationUnit& unit, TypeId type, support::ValueOperation answers)
    -> const TypeOperation* {
  for (const TypeOperation& stated : unit.type_operations) {
    if (stated.type == type && stated.answers == answers) {
      return &stated;
    }
  }
  return nullptr;
}

// The members' types of the record `record`, wherever it was declared, in
// declaration order.
[[nodiscard]] inline auto RecordElements(
    const CompilationUnit& unit, RecordId record) -> std::span<const TypeId> {
  return unit.GetRecord(record).elements;
}

// Which aggregate the record `record` is, wherever it was declared.
[[nodiscard]] inline auto RecordKindOf(
    const CompilationUnit& unit, RecordId record) -> RecordKind {
  return unit.GetRecord(record).kind;
}

// The record declaration `type` is, and nothing for any other type.
[[nodiscard]] inline auto RecordOf(const Type& type) -> const RecordId* {
  if (const auto* record = type.As<RecordType>()) {
    return &record->declaration;
  }
  return nullptr;
}

// Which aggregate `type` is, where it is a record, and nothing for any other
// type.
[[nodiscard]] inline auto RecordKindOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<RecordKind> {
  const RecordId* record = RecordOf(unit.types.Get(type));
  if (record == nullptr) {
    return std::nullopt;
  }
  return RecordKindOf(unit, *record);
}

// The members of the union or the tagged union `type` is, which overlay one
// storage (LRM 7.3), and nothing for any other type.
[[nodiscard]] inline auto OverlaidMembers(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const RecordId* record = RecordOf(unit.types.Get(type));
  if (record == nullptr) {
    return std::nullopt;
  }
  switch (RecordKindOf(unit, *record)) {
    case RecordKind::kStruct:
      return std::nullopt;
    case RecordKind::kUnion:
    case RecordKind::kTaggedUnion:
      return RecordElements(unit, *record);
  }
  throw InternalError("mir: unknown record kind");
}

// Whether `type` is a tagged union, whose member is chosen by its tag (LRM
// 7.3.2).
[[nodiscard]] inline auto IsTaggedUnion(
    const CompilationUnit& unit, TypeId type) -> bool {
  const std::optional<RecordKind> kind = RecordKindOf(unit, type);
  if (!kind.has_value()) {
    return false;
  }
  switch (*kind) {
    case RecordKind::kStruct:
    case RecordKind::kUnion:
      return false;
    case RecordKind::kTaggedUnion:
      return true;
  }
  throw InternalError("mir: unknown record kind");
}

// The declaration any unit names the record `record` by.
[[nodiscard]] inline auto RecordDeclarationOf(
    const CompilationUnit& unit, RecordId record) -> TypeDeclarationRef {
  const RecordDecl& declared = unit.GetRecord(record);
  return TypeDeclarationRef{
      .unit_name = declared.defining_unit.value_or(unit.name),
      .path = declared.path};
}

// The components of a product -- a tuple or a structure, which are reached the
// same way -- or nothing for a type that is no product.
[[nodiscard]] inline auto ProductElements(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const Type& t = unit.types.Get(type);
  if (const auto* tuple = t.As<TupleType>()) {
    return std::span<const TypeId>{tuple->elements};
  }
  const RecordId* record = RecordOf(t);
  if (record == nullptr) {
    return std::nullopt;
  }
  switch (RecordKindOf(unit, *record)) {
    case RecordKind::kStruct:
      return RecordElements(unit, *record);
    case RecordKind::kUnion:
    case RecordKind::kTaggedUnion:
      return std::nullopt;
  }
  throw InternalError("mir: unknown record kind");
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
  if (!IsTaggedUnion(unit, tagged_union)) {
    throw InternalError("TaggedComponentType: type is not a tagged union");
  }
  const std::span<const TypeId> members = *OverlaidMembers(unit, tagged_union);
  if (tag_index.value >= members.size()) {
    throw InternalError("TaggedComponentType: tag index out of range");
  }
  return members[tag_index.value];
}

}  // namespace lyra::mir
