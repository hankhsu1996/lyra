#pragma once

#include <cstdint>
#include <string>
#include <utility>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Whether a compilation unit's instances exist as objects. A module (LRM 23)
// and an interface (LRM 25) are instantiated into the hierarchy, so each roots
// an object composing a top class; a package (LRM 26) and the `$unit` file-set
// scope (LRM 3.12.1) hold declarations reached by name with no receiver, and
// root none. Which of the four forms a unit was written as is a different
// question, and not one anything below this asks. The answer is intrinsic to
// the source construct, so it travels on the unit rather than being re-inferred
// from its scope shape at each stage that must branch on it.
enum class UnitRole : std::uint8_t { kObjectRoot, kNamespace };

// Canonical TypeIds for primitives the lowering frequently materializes
// (literal int type, void result of system tasks, etc.). Populated by
// `CompilationUnit`'s constructor; consumers read them off the unit.
struct BuiltinHirTypes {
  TypeId scalar_bit;
  TypeId scalar_logic;
  TypeId void_type;
  TypeId int_type;
  TypeId int_unsigned;
  TypeId integer;
  TypeId string;
  TypeId time;
  TypeId realtime;
  TypeId wildcard_index;
};

struct CompilationUnit {
  std::string name;
  UnitRole role = UnitRole::kObjectRoot;
  TypePool types;
  BuiltinHirTypes builtins;
  StructuralScope root_scope;
  // A class can be referenced -- as a handle type or a `new` target -- before
  // its body is built, so its identity must exist before its definition.
  base::Registry<ClassDecl, ClassId> classes;
  // One entry per unit this one reaches an object of, recorded where that
  // unit's signature was consumed.
  base::Arena<ExternalUnitObject, ExternalUnitObjectId> external_unit_objects;
  // Every DPI-C import this unit takes part in (LRM 35.4), whether declared
  // inside it or declared elsewhere and called from it. The unit owns them
  // rather than any scope because an import's foreign symbol is program-global,
  // in a name space of its own that no compilation-unit scope contains -- so a
  // call reaches it without depending on whichever unit spelled the
  // declaration, and one declaration read by several units yields one identical
  // entry in each. One entry per declaration; a repeat is the same entry.
  base::Arena<ForeignImportDecl, ForeignImportId> foreign_imports;

  explicit CompilationUnit(std::string name)
      : name(std::move(name)), builtins(MakeBuiltins(types)) {
  }

 private:
  // The single-bit leaves and the predefined-width integers are the primitive
  // canonical types. The leaves are added first; the predefined integers are
  // single-dimension packed arrays over them (LRM 7.4.1: an integer type with a
  // predefined width matches a single-dimension packed array).
  static auto MakeBuiltins(TypePool& types) -> BuiltinHirTypes {
    const auto add = [&](auto arm) {
      return types.Intern(Type{std::move(arm)});
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
                .signedness = Signedness::kSigned}),
        .int_unsigned =
            add(PackedArrayType{
                .dim = PackedRange{.left = 31, .right = 0},
                .element_type = scalar_bit,
                .signedness = Signedness::kUnsigned}),
        .integer =
            add(PackedArrayType{
                .dim = PackedRange{.left = 31, .right = 0},
                .element_type = scalar_logic,
                .signedness = Signedness::kSigned}),
        .string = add(StringType{}),
        .time =
            add(PackedArrayType{
                .dim = PackedRange{.left = 63, .right = 0},
                .element_type = scalar_logic,
                .signedness = Signedness::kUnsigned}),
        .realtime = add(RealTimeType{}),
        .wildcard_index = add(WildcardIndexType{}),
    };
  }
};

}  // namespace lyra::hir
