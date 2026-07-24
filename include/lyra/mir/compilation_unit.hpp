#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/deferred_check_site.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interner.hpp"

namespace lyra::mir {

struct DeferredCheckSite {};

// A unit-level static variable: a named mutable value the unit's namespace owns
// with static storage -- one program-global cell, shared across the whole
// simulation, not a member of any instance (LRM 26.2 package variables, LRM
// 6.21 static lifetime). The mutable, observable counterpart of a
// `StaticConstantDecl`: a backend emits it as a namespace-scope observable cell
// and every reference reaches it by name (`unit::name`), the storage dual of
// the unit's receiver-less callables. `type` is the observable-cell type; the
// declared value type is its inner value. The initializer is not here: it runs
// in the unit's synthesized initializer at time zero (LRM 10.5), the way a
// class member's initializer runs in its Initialize phase, never as a field on
// the declaration.
struct StaticVariableDecl {
  std::string name;
  TypeId type;
};

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
  TypeId integer;
  TypeId bit1;
  // The machine integer a runtime entry hands back as a plain value. It is the
  // widest one, so a narrower machine integer is reached from it by an
  // `IntCastExpr` rather than by an entry of its own.
  TypeId machine_int64;
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
  TypeId channel_cancellation;
  TypeId print_item;
  TypeId print_literal_item;
  TypeId print_value_item;
  TypeId format_spec;
  TypeId format_arg;
  TypeId time_format;
  TypeId hierarchy_segment;
  TypeId trigger;
  TypeId coroutine_void;
  TypeId wildcard_index;
};

struct CompilationUnit {
  // The unit's own name -- its module, package, or interface name. A backend
  // names the unit's emitted artifact from this, so a unit whose root is a
  // namespace rather than a class (a package) still has a stable identity
  // independent of any member.
  std::string name;
  TypeInterner types;
  BuiltinMirTypes builtins;
  // Every class declaration of this unit, owned here exactly once and reached
  // by its identity, with a declare-then-define lifecycle so a class can be
  // named before its body is built. `root` identifies the unit's top class, the
  // module or interface declaration; it is absent for a package, whose root is
  // a namespace that owns no instance.
  base::Registry<Class, ClassId> classes;
  std::optional<ClassId> root;
  // Callables the unit's namespace owns directly rather than through one of its
  // classes -- a package's functions and tasks (LRM 26.3), receiver-less and
  // bodied. A class's own callables live on that class; these are the
  // unit-level namespace's, one scope up.
  base::Arena<CallableDecl, CallableId> callables;
  // Every DPI-C export foreign-linkage wrapper (LRM 35.5) this unit
  // contributes. An export is a program-global symbol in its own name space,
  // never a class member (LRM 35.4, 35.7), so a module-scoped export (a method)
  // and a package-scoped export (a receiver-less function) are owned here at
  // the unit alike; each wrapper's body recovers whatever context it needs, so
  // they are one uniform list.
  std::vector<ForeignExportWrapper> foreign_export_wrappers;
  // Static variables the unit's namespace owns directly rather than through one
  // of its classes -- a package's variables (LRM 26.2), one program-global cell
  // each, shared and reached by name. A class's own static storage lives on
  // that class; these are the unit-level namespace's, one scope up. Their
  // initializers run in the unit's synthesized initializer at time zero.
  base::Arena<StaticVariableDecl, StaticVariableId> static_variables;
  // The source name of each nominal type that carries none of its own, keyed by
  // that type's id. An enum is the case: it has no intrinsic name (an anonymous
  // enum has none, a multi-typedef enum has several), so a `typedef` names it
  // and that name lives here, first typedef winning. An anonymous enum has no
  // entry and renders a synthesized name. A struct is not here -- it carries
  // its own name in its declaration. This is the unit-level fact a backend
  // resolves an enum's emitted name against, the peer of the struct registry's
  // name.
  std::unordered_map<TypeId, std::string> nominal_type_names;
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
  std::vector<DeferredCheckSite> deferred_check_sites;
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
  // The packages whose variables this package's variable initializers read
  // directly (LRM 26.2 / 10.5) -- the by-name dependency the design root uses
  // to pick a stable value-initialization order. It records only reads written
  // directly in an initializer expression; a read reached through a called
  // function does not contribute yet. This is a preference, not a correctness
  // input: every cell is installed with its default before any initializer
  // runs, so a missed or cyclic dependency only means a read observes a
  // default, never an uninstalled cell. Empty for a non-package unit and for a
  // package with no initializer that reads another package's variable.
  std::vector<std::string> direct_initializer_package_reads;

  CompilationUnit()
      : builtins{
            .int_type = types.Intern(
                PackedArrayType{
                    .atom = BitAtom::kBit,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}},
                    .form = PackedArrayForm::kInt}),
            .integer = types.Intern(
                PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}},
                    .form = PackedArrayForm::kInteger}),
            .bit1 = types.Intern(
                PackedArrayType{
                    .atom = BitAtom::kBit,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 0, .right = 0}},
                    .form = PackedArrayForm::kExplicit}),
            .machine_int64 = types.Intern(
                MachineIntType{
                    .bit_width = 64, .signedness = Signedness::kSigned}),
            .string = types.Intern(StringType{}),
            .void_type = types.Intern(VoidType{}),
            .realtime = types.Intern(RealTimeType{}),
            .time = types.Intern(
                PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 63, .right = 0}},
                    .form = PackedArrayForm::kTime}),
            .effects = types.Intern(RuntimeEffectsType{}),
            .scope_ptr = types.PointerTo(
                types.Intern(
                    ExternalClassType{
                        .qualified_name = "lyra::runtime::Scope"}),
                PointerOwnership::kBorrowed),
            .process_object = types.Intern(
                ExternalClassType{
                    .qualified_name = "lyra::runtime::RuntimeProcess"}),
            .files = types.Intern(FilesType{}),
            .diagnostic = types.Intern(DiagnosticType{}),
            .channel_cancellation = types.Intern(
                RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kChannelCancellation}),
            .print_item = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kPrintItem}),
            .print_literal_item = types.Intern(
                RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintLiteralItem}),
            .print_value_item = types.Intern(
                RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintValueItem}),
            .format_spec = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kFormatSpec}),
            .format_arg = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kFormatArg}),
            .time_format = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kTimeFormat}),
            .hierarchy_segment = types.Intern(
                RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kHierarchySegment}),
            .trigger = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kTrigger}),
            .coroutine_void = TypeId{},
            .wildcard_index = types.Intern(WildcardIndexType{}),
        } {
    // `Coroutine<void>` is the completion type of a process or void task. It is
    // built in the constructor body because it reads back the already-interned
    // `void_type`; the member-list entry above is an unused placeholder
    // overwritten here. The field is a convenience alias for the canonical
    // instance, not a deduplication mechanism -- interning `Coroutine<void>`
    // anywhere returns this same id.
    builtins.coroutine_void = types.CoroutineOf(builtins.void_type);
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
  void AddExternalReferencedUnit(std::string unit_name) {
    for (const std::string& existing : external_referenced_units) {
      if (existing == unit_name) {
        return;
      }
    }
    external_referenced_units.push_back(std::move(unit_name));
  }

  // Records a cross-unit class-reference dependency, deduplicated. Called
  // from HIR-to-MIR when a class handle type, a `new`, a field / method /
  // static access, or a base extension names a class of another unit.
  void AddExternalClassUnit(std::string unit_name) {
    for (const std::string& existing : external_class_units) {
      if (existing == unit_name) {
        return;
      }
    }
    external_class_units.push_back(std::move(unit_name));
  }

  // Backing-vector position is the id, matching TypeId / LocalId.
  auto AllocateDeferredCheckSiteId() -> DeferredCheckSiteId {
    const auto id = static_cast<std::uint32_t>(deferred_check_sites.size());
    deferred_check_sites.push_back({});
    return DeferredCheckSiteId{id};
  }
};

// 2-state signed 32-bit literal, typed `int` (LRM 6.11.1).
[[nodiscard]] inline auto MakeIntLiteral(TypeId int_type, std::int64_t value)
    -> Expr {
  return Expr{
      .data =
          IntegerLiteral{
              .value =
                  IntegralConstant{
                      .value_words = {static_cast<std::uint64_t>(value)},
                      .state_words = {},
                      .width = 32,
                      .signedness = Signedness::kSigned,
                      .state_kind = IntegralStateKind::kTwoState}},
      .type = int_type};
}

// 4-state signed 32-bit literal, typed `integer` (LRM 6.11.1). Used by sites
// that compare against the matched-count return of `$sscanf` / `$fscanf` --
// those return `integer`, so the operand on the other side must match
// state-kind.
[[nodiscard]] inline auto MakeIntegerLiteral(
    TypeId integer_type, std::int64_t value) -> Expr {
  return Expr{
      .data =
          IntegerLiteral{
              .value =
                  IntegralConstant{
                      .value_words = {static_cast<std::uint64_t>(value)},
                      .state_words = {},
                      .width = 32,
                      .signedness = Signedness::kSigned,
                      .state_kind = IntegralStateKind::kFourState}},
      .type = integer_type};
}

}  // namespace lyra::mir
