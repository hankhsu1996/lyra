#pragma once

#include <string>
#include <utility>

#include "lyra/base/arena.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Canonical TypeIds for primitives the lowering frequently materializes
// (literal int type, void result of system tasks, etc.). Populated by
// `ModuleUnit`'s constructor; consumers read them off the unit.
struct BuiltinHirTypes {
  TypeId void_type;
  TypeId int32;
  TypeId integer;
  TypeId string;
  TypeId time;
  TypeId realtime;
  TypeId wildcard_index;
};

struct ModuleUnit {
  std::string name;
  base::Arena<Type, TypeId> types;
  BuiltinHirTypes builtins;
  StructuralScope root_scope;

  explicit ModuleUnit(std::string name)
      : name(std::move(name)),
        builtins{
            .void_type = types.Add(Type{.data = VoidType{}}),
            .int32 = types.Add(
                Type{
                    .data =
                        PackedArrayType{
                            .atom = BitAtom::kBit,
                            .signedness = Signedness::kSigned,
                            .dims = {PackedRange{.left = 31, .right = 0}},
                            .form = PackedArrayForm::kInt}}),
            .integer = types.Add(
                Type{
                    .data =
                        PackedArrayType{
                            .atom = BitAtom::kLogic,
                            .signedness = Signedness::kSigned,
                            .dims = {PackedRange{.left = 31, .right = 0}},
                            .form = PackedArrayForm::kInteger}}),
            .string = types.Add(Type{.data = StringType{}}),
            .time = types.Add(
                Type{
                    .data =
                        PackedArrayType{
                            .atom = BitAtom::kLogic,
                            .signedness = Signedness::kUnsigned,
                            .dims = {PackedRange{.left = 63, .right = 0}},
                            .form = PackedArrayForm::kTime}}),
            .realtime = types.Add(Type{.data = RealTimeType{}}),
            .wildcard_index = types.Add(Type{.data = WildcardIndexType{}}),
        } {
  }
};

}  // namespace lyra::hir
