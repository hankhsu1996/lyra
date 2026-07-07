#pragma once

#include <cstdint>
#include <vector>

#include "lyra/base/registry.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/deferred_check_site.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_interner.hpp"

namespace lyra::mir {

struct DeferredCheckSite {};

// Named TypeIds the lowering and rendering reuse. Most are language or runtime
// atomic types (the literal `int` type, the 1-bit selector type, the `void`
// result of system tasks). `scope_ptr` and `coroutine_void` are not atomic:
// they are convenience aliases for canonical instances of composite types
// (`Pointer<Scope>`, `Coroutine<void>`) that nearly every scope and process
// materializes -- the interner gives them their identity, these fields only
// name them. Populated by `CompilationUnit`'s constructor; consumers read them
// off the unit.
struct BuiltinMirTypes {
  TypeId int32;
  TypeId integer;
  TypeId bit1;
  TypeId string;
  TypeId void_type;
  TypeId realtime;
  TypeId time;
  TypeId services;
  TypeId scope_ptr;
  TypeId files;
  TypeId diagnostic;
  TypeId channel_cancellation;
  TypeId print_item;
  TypeId print_literal_item;
  TypeId print_value_item;
  TypeId format_spec;
  TypeId time_format;
  TypeId hierarchy_segment;
  TypeId coroutine_void;
  TypeId wildcard_index;
};

struct CompilationUnit {
  TypeInterner types;
  BuiltinMirTypes builtins;
  // Every class declaration of this unit, owned here exactly once and reached
  // by its identity, with a declare-then-define lifecycle so a class can be
  // named before its body is built. `root` identifies the unit's top class, the
  // module declaration.
  base::Registry<Class, ClassId> classes;
  ClassId root{};
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

  CompilationUnit()
      : builtins{
            .int32 = types.Intern(
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
            .string = types.Intern(StringType{}),
            .void_type = types.Intern(VoidType{}),
            .realtime = types.Intern(RealTimeType{}),
            .time = types.Intern(
                PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 63, .right = 0}},
                    .form = PackedArrayForm::kTime}),
            .services = types.Intern(ServicesType{}),
            .scope_ptr = types.PointerTo(
                types.Intern(ScopeType{}), PointerOwnership::kBorrowed),
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
            .time_format = types.Intern(
                RuntimeLibraryType{.kind = RuntimeLibraryKind::kTimeFormat}),
            .hierarchy_segment = types.Intern(
                RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kHierarchySegment}),
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

  // Backing-vector position is the id, matching TypeId / LocalId.
  auto AllocateDeferredCheckSiteId() -> DeferredCheckSiteId {
    const auto id = static_cast<std::uint32_t>(deferred_check_sites.size());
    deferred_check_sites.push_back({});
    return DeferredCheckSiteId{id};
  }
};

[[nodiscard]] inline auto MakeInt32Literal(
    TypeId int32_type, std::int64_t value) -> Expr {
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
      .type = int32_type};
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
