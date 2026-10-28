#pragma once

#include <cstddef>
#include <format>
#include <map>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/random_qualifier.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

struct EnumMember {
  std::string name;
  IntegralConstant value;

  auto operator==(const EnumMember&) const -> bool = default;
};

// What an enumeration declares (LRM 6.19): the type its values are of, and
// its members in the order written, each with its value.
struct EnumDefinition {
  TypeId base_type;
  std::vector<EnumMember> members;

  auto operator==(const EnumDefinition&) const -> bool = default;
};

// A named member of an unpacked aggregate (struct or union). Unlike a packed
// aggregate field, an unpacked member has its own independent storage of its
// declared type -- there is no shared bit vector, so no offset or width. The
// member is identified by its position in declaration order (LRM 7.2 / 7.3),
// the same index a member-access expression carries. `default_init` holds the
// member's own declaration initializer (LRM 7.2.2) as a folded constant value
// -- type metadata, like an enum member's value -- which takes precedence over
// the member type's Table 7-1 default when the enclosing struct is
// default-constructed; absent when the member has no initializer. `random` is
// how a member of a structure is declared random, which it is wherever the
// structure is (LRM 18.4).
struct UnpackedAggregateField {
  std::string name;
  TypeId type;
  std::optional<ConstantValue> default_init;
  RandomQualifier random = RandomQualifier::kNone;

  auto operator==(const UnpackedAggregateField&) const -> bool = default;
};

// What an unpacked structure declares (LRM 7.2): its members.
struct UnpackedStructDefinition {
  std::vector<UnpackedAggregateField> fields;

  auto operator==(const UnpackedStructDefinition&) const -> bool = default;
};

// What an unpacked union declares (LRM 7.3): the members its storage overlays,
// and whether it is tagged (LRM 7.3.2) -- carrying a tag naming the member it
// holds, which may then be `void` -- or the untagged form read through any
// member.
struct UnpackedUnionDefinition {
  std::vector<UnpackedAggregateField> fields;
  bool tagged = false;

  auto operator==(const UnpackedUnionDefinition&) const -> bool = default;
};

using TypeDefinition = std::variant<
    EnumDefinition, UnpackedStructDefinition, UnpackedUnionDefinition>;

// Hands `take` the declaration of every declared type `types` names, again
// until a pass names none: what taking one reads may join the pool, and is
// handed in turn.
template <typename Take>
void ForEachNamedDeclaration(const TypePool& types, const Take& take) {
  for (std::size_t named = 0; named != types.size();) {
    named = types.size();
    for (const TypeId id : types.Ids()) {
      if (const TypeDeclarationRef* declaration =
              types.Get(id).NamedDeclaration()) {
        take(*declaration);
      }
    }
  }
}

// The definitions of the declared types a pool names, each under the
// declaration it is, whose types are that pool's. A unit holds the definition
// of every such type it names: its own, worked out from its declarations, and
// another unit's, as that unit published it.
class TypeDefinitions {
 public:
  // Records what `declaration` declares. A declaration is defined once.
  void Define(
      const TypeDeclarationRef& declaration, TypeDefinition definition) {
    const auto [_, entered] =
        definitions_.try_emplace(declaration, std::move(definition));
    if (!entered) {
      throw InternalError(
          std::format(
              "hir::TypeDefinitions::Define: '{}' of unit '{}' is defined "
              "already -- please report this as a bug",
              support::DisplayOf(declaration.path), declaration.unit_name));
    }
  }

  // What `declaration` declares, or nothing where it is not defined here.
  [[nodiscard]] auto Find(const TypeDeclarationRef& declaration) const
      -> const TypeDefinition* {
    const auto it = definitions_.find(declaration);
    return it == definitions_.end() ? nullptr : &it->second;
  }

  // What `declaration` declares, which a unit holds for every declared type
  // it names.
  template <typename Definition>
  [[nodiscard]] auto Of(const TypeDeclarationRef& declaration) const
      -> const Definition& {
    const TypeDefinition* definition = Find(declaration);
    const Definition* of_kind =
        definition == nullptr ? nullptr : std::get_if<Definition>(definition);
    if (of_kind == nullptr) {
      throw InternalError(
          std::format(
              "hir::TypeDefinitions::Of: '{}' of unit '{}' is not defined here "
              "as the kind of type it was named as -- please report this as a "
              "bug",
              support::DisplayOf(declaration.path), declaration.unit_name));
    }
    return *of_kind;
  }

  [[nodiscard]] auto begin() const {
    return definitions_.begin();
  }
  [[nodiscard]] auto end() const {
    return definitions_.end();
  }

  auto operator==(const TypeDefinitions&) const -> bool = default;

 private:
  std::map<TypeDeclarationRef, TypeDefinition> definitions_;
};

}  // namespace lyra::hir
