#pragma once

#include <string>
#include <utility>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Canonical TypeIds for primitives the lowering frequently materializes
// (literal int type, void result of system tasks, etc.). Populated by
// `CompilationUnit`'s constructor; consumers read them off the unit.
struct BuiltinHirTypes {
  TypeId scalar_bit;
  TypeId scalar_logic;
  TypeId void_type;
  TypeId int_type;
  TypeId integer;
  TypeId string;
  TypeId time;
  TypeId realtime;
  TypeId wildcard_index;
};

struct CompilationUnit {
  std::string name;
  base::Arena<Type, TypeId> types;
  BuiltinHirTypes builtins;
  StructuralScope root_scope;
  // A class can be referenced -- as a handle type or a `new` target -- before
  // its body is built, so its identity must exist before its definition.
  base::Registry<ClassDecl, ClassId> classes;

  explicit CompilationUnit(std::string name)
      : name(std::move(name)), builtins(MakeBuiltins(types)) {
  }

 private:
  // The single-bit leaves and the predefined-width integers are the primitive
  // canonical types. The leaves are added first; the predefined integers are
  // single-dimension packed arrays over them (LRM 7.4.1: an integer type with a
  // predefined width matches a single-dimension packed array).
  static auto MakeBuiltins(base::Arena<Type, TypeId>& types)
      -> BuiltinHirTypes {
    const auto add = [&](TypeData data) {
      return types.Add(Type{.data = std::move(data)});
    };
    const TypeId scalar_bit = add(ScalarBitType{.atom = BitAtom::kBit});
    const TypeId scalar_logic = add(ScalarBitType{.atom = BitAtom::kLogic});
    return BuiltinHirTypes{
        .scalar_bit = scalar_bit,
        .scalar_logic = scalar_logic,
        .void_type = add(VoidType{}),
        .int_type =
            add(PackedArrayType{
                .dim = PackedRange{.left = 31, .right = 0},
                .element_type = scalar_bit,
                .signedness = Signedness::kSigned,
                .form = PackedArrayForm::kInt}),
        .integer =
            add(PackedArrayType{
                .dim = PackedRange{.left = 31, .right = 0},
                .element_type = scalar_logic,
                .signedness = Signedness::kSigned,
                .form = PackedArrayForm::kInteger}),
        .string = add(StringType{}),
        .time =
            add(PackedArrayType{
                .dim = PackedRange{.left = 63, .right = 0},
                .element_type = scalar_logic,
                .signedness = Signedness::kUnsigned,
                .form = PackedArrayForm::kTime}),
        .realtime = add(RealTimeType{}),
        .wildcard_index = add(WildcardIndexType{}),
    };
  }
};

}  // namespace lyra::hir
