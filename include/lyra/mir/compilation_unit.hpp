#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct DeferredCheckSite {};

// Canonical TypeIds for primitives the lowering and rendering frequently
// materialize (literal `int` type, 1-bit selector type, void result of system
// tasks, etc.). Populated by `CompilationUnit`'s constructor; consumers read
// them off the unit.
struct BuiltinMirTypes {
  TypeId int32;
  TypeId integer;
  TypeId bit1;
  TypeId string;
  TypeId void_type;
  TypeId realtime;
  TypeId time;
  TypeId services;
  TypeId print_item;
  TypeId print_literal_item;
  TypeId print_value_item;
  TypeId format_spec;
  TypeId coroutine;
};

struct CompilationUnit {
  std::vector<Type> types;
  BuiltinMirTypes builtins;
  Class top_class;
  std::vector<DeferredCheckSite> deferred_check_sites;

  CompilationUnit()
      : builtins{
            .int32 = AddType(
                TypeData{PackedArrayType{
                    .atom = BitAtom::kBit,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}},
                    .form = PackedArrayForm::kInt}}),
            .integer = AddType(
                TypeData{PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kSigned,
                    .dims = {PackedRange{.left = 31, .right = 0}},
                    .form = PackedArrayForm::kInteger}}),
            .bit1 = AddType(
                TypeData{PackedArrayType{
                    .atom = BitAtom::kBit,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 0, .right = 0}},
                    .form = PackedArrayForm::kExplicit}}),
            .string = AddType(TypeData{StringType{}}),
            .void_type = AddType(TypeData{VoidType{}}),
            .realtime = AddType(TypeData{RealTimeType{}}),
            .time = AddType(
                TypeData{PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 63, .right = 0}},
                    .form = PackedArrayForm::kTime}}),
            .services = AddType(TypeData{ServicesType{}}),
            .print_item = AddType(
                TypeData{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintItem}}),
            .print_literal_item = AddType(
                TypeData{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintLiteralItem}}),
            .print_value_item = AddType(
                TypeData{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kPrintValueItem}}),
            .format_spec = AddType(
                TypeData{RuntimeLibraryType{
                    .kind = RuntimeLibraryKind::kFormatSpec}}),
            .coroutine = AddType(TypeData{CoroutineType{}}),
        } {
  }

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types.at(id.value);
  }

  auto AddType(TypeData data) -> TypeId {
    const TypeId id{static_cast<std::uint32_t>(types.size())};
    types.push_back(Type{.data = std::move(data)});
    return id;
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
