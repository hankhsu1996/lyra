#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

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
  std::vector<Type> types;
  BuiltinHirTypes builtins;
  StructuralScope root_scope;

  explicit ModuleUnit(std::string name)
      : name(std::move(name)),
        builtins{
            .void_type = AddType(TypeData{VoidType{}}),
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
            .string = AddType(TypeData{StringType{}}),
            .time = AddType(
                TypeData{PackedArrayType{
                    .atom = BitAtom::kLogic,
                    .signedness = Signedness::kUnsigned,
                    .dims = {PackedRange{.left = 63, .right = 0}},
                    .form = PackedArrayForm::kTime}}),
            .realtime = AddType(TypeData{RealTimeType{}}),
            .wildcard_index = AddType(TypeData{WildcardIndexType{}}),
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
};

}  // namespace lyra::hir
