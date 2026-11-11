#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/class_registry.hpp"
#include "lyra/hir/coverage_library.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/namespace.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

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

  auto operator==(const BuiltinHirTypes&) const -> bool = default;
};

// What one lowering of a unit came out as, which is one body of the unit.
struct CompilationUnit {
  // The unit, which is what it publishes and what every referrer names.
  std::string name;
  // Which body of the unit this is: the unit's name for the first distinct
  // body of the unit in the design's order, and for each later one the unit's
  // name with the body's ordinal after; lowerings whose code comes out alike
  // are one body.
  std::string body_name;
  // The identifier the source declares this unit's design element, package or
  // class under, which is what its author knows it by; `$unit` for a
  // compilation unit's scope (LRM 3.12.1); and for a text of Lyra's own the
  // unit it is compiled as. Units of one definition compiled to more than one
  // unit share it.
  std::string source_name;
  // The unit's own scope's name as far as its text fixes it (LRM 21.2.1.5):
  // the package's, and nothing for a design element or `$unit`.
  FixedName fixed_name;
  UnitDefinition definition;
  TypePool types;
  // What each enumeration, unpacked structure and unpacked union `types` names
  // declares, whichever unit declares it.
  TypeDefinitions type_definitions;
  BuiltinHirTypes builtins;
  StructuralScope root_scope;
  // Every namespace this unit names, the unit itself where it is one.
  NamespaceRegistry namespaces;
  // What this unit defines as a namespace, present exactly where it is one.
  std::optional<NamespaceDefinition> namespace_definition;
  // Every body a generate block of the unit compiles to (LRM 27), each once:
  // blocks stating the same thing are one body, whichever objects build them.
  base::Arena<BlockBody, BlockBodyId> block_bodies;
  ClassRegistry classes;
  CovergroupRegistry covergroups;
  // What every covergroup this unit declares or names is written over, present
  // where there is one.
  std::optional<CoverageLibraryMembers> coverage_library;
  // Every class of a scope of the design hierarchy this unit names: the ones it
  // publishes of its own scopes, and each one of another unit's scope it
  // reaches an object of, recorded where that unit's signature was consumed.
  ScopeClassRegistry scope_classes;
  // Every DPI-C import this unit takes part in (LRM 35.4), whether declared
  // inside it or declared elsewhere and called from it. The unit owns them
  // rather than any scope because an import's foreign symbol is program-global,
  // in a name space of its own that no compilation-unit scope contains -- so a
  // call reaches it without depending on whichever unit spelled the
  // declaration, and one declaration read by several units yields one identical
  // entry in each. One entry per declaration; a repeat is the same entry.
  base::Arena<ForeignImportDecl, ForeignImportId> foreign_imports;

  CompilationUnit(std::string name, std::string body_name)
      : name(std::move(name)),
        body_name(std::move(body_name)),
        builtins(MakeBuiltins(types)),
        namespaces(this->name),
        classes(this->name),
        covergroups(this->name),
        scope_classes(this->name) {
  }

  auto operator==(const CompilationUnit&) const -> bool = default;

  // Whether a reference to the unit named `unit_name` is one to this unit: a
  // hierarchical name reaching another instance of this same application names
  // it the way a reference to any other unit does, by its name.
  [[nodiscard]] auto IsItself(std::string_view unit_name) const -> bool {
    return unit_name == name;
  }

  // What the declaration an enumeration, an unpacked structure or an unpacked
  // union names declares.
  [[nodiscard]] auto DefinitionOf(const EnumType& type) const
      -> const EnumDefinition& {
    return type_definitions.Of<EnumDefinition>(type.declaration);
  }
  [[nodiscard]] auto DefinitionOf(const UnpackedStructType& type) const
      -> const UnpackedStructDefinition& {
    return type_definitions.Of<UnpackedStructDefinition>(type.declaration);
  }
  [[nodiscard]] auto DefinitionOf(const UnpackedUnionType& type) const
      -> const UnpackedUnionDefinition& {
    return type_definitions.Of<UnpackedUnionDefinition>(type.declaration);
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
