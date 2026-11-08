#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/arena.hpp"
#include "lyra/base/id_range.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/keyed_identities.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// A namespace -- a package (LRM 26.2) or the compilation-unit scope (LRM
// 3.12.1) -- as one unit names it.
struct NamespaceId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const NamespaceId&) const -> std::strong_ordering = default;
};

// A variable a namespace keeps, at its position in the namespace's declaration.
struct NamespaceVariableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const NamespaceVariableId&) const
      -> std::strong_ordering = default;
};

// A subroutine a namespace declares, at its position in the namespace's
// declaration.
struct NamespaceSubroutineId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const NamespaceSubroutineId&) const
      -> std::strong_ordering = default;
};

// The name every unit gives a namespace: the unit that is it. A namespace is
// not instantiated, so the unit and the namespace are one.
struct NamespaceKey {
  std::string unit_name;

  auto operator<=>(const NamespaceKey&) const -> std::strong_ordering = default;
  struct Hash {
    auto operator()(const NamespaceKey& key) const -> std::size_t {
      return std::hash<std::string>{}(key.unit_name);
    }
  };
};

// A variable a namespace keeps (LRM 26.2): one it declares, or a static
// variable one of its subroutines declares (LRM 6.21), a formal of a static one
// included (LRM 13.3.2), named as the namespace knows it.
struct NamespaceVariable {
  support::QualifiedName name;
  TypeId type;

  auto operator==(const NamespaceVariable&) const -> bool = default;
};

using NamespaceSubroutine = DeclaredSubroutine<NamespaceSubroutineId>;

// What a namespace declares, as every unit naming it reads it, the namespace's
// own unit included: the subroutines another unit calls on no object (LRM
// 26.3), and the variables it keeps, each at the position the namespace places
// it. It is also what the namespace publishes.
struct NamespaceDeclaration {
  base::Arena<NamespaceSubroutine, NamespaceSubroutineId> subroutines;
  base::Arena<NamespaceVariable, NamespaceVariableId> variables;

  auto operator==(const NamespaceDeclaration&) const -> bool = default;

  // The variable the namespace knows as `name`, where it keeps one.
  [[nodiscard]] auto FindVariable(const support::QualifiedName& name) const
      -> std::optional<NamespaceVariableId> {
    for (const NamespaceVariableId id : variables.Ids()) {
      if (variables.Get(id).name == name) return id;
    }
    return std::nullopt;
  }

  // The subroutine the namespace declares as `name`, where it declares one.
  [[nodiscard]] auto FindSubroutine(std::string_view name) const
      -> std::optional<NamespaceSubroutineId> {
    for (const NamespaceSubroutineId id : subroutines.Ids()) {
      if (subroutines.Get(id).prototype.name == name) return id;
    }
    return std::nullopt;
  }
};

// Every namespace one unit names, its own among them where the unit is one,
// each under one identity and with what it declares. The identity arrives
// where a name first reaches the namespace, and what it declares is read then,
// from the namespace's signature, which is what its own unit reads too.
class NamespaceRegistry {
 public:
  explicit NamespaceRegistry(std::string unit_name)
      : identities_(std::move(unit_name)) {
  }

  // The namespace `key` names, and whether it entered just now, in which case
  // what it declares is still to be set.
  auto Enter(NamespaceKey key) -> std::pair<NamespaceId, bool> {
    const auto [id, entered] = identities_.Enter(std::move(key));
    if (entered) declarations_.Declare();
    return {id, entered};
  }

  void SetDeclaration(NamespaceId id, NamespaceDeclaration declaration) {
    declarations_.Define(id, std::move(declaration));
  }

  [[nodiscard]] auto HasDeclaration(NamespaceId id) const -> bool {
    return declarations_.IsDefined(id);
  }

  [[nodiscard]] auto Declaration(NamespaceId id) const
      -> const NamespaceDeclaration& {
    return declarations_.Get(id);
  }

  // The variable at `variable` of what the namespace `owner` declares.
  [[nodiscard]] auto VariableOf(NamespaceId owner, NamespaceVariableId variable)
      const -> const NamespaceVariable& {
    return Declaration(owner).variables.Get(variable);
  }

  // The subroutine at `subroutine` of what the namespace `owner` declares.
  [[nodiscard]] auto SubroutineOf(
      NamespaceId owner, NamespaceSubroutineId subroutine) const
      -> const NamespaceSubroutine& {
    return Declaration(owner).subroutines.Get(subroutine);
  }

  [[nodiscard]] auto KeyOf(NamespaceId id) const -> const NamespaceKey& {
    return identities_.KeyOf(id);
  }

  // Whether this unit is the namespace, rather than naming another.
  [[nodiscard]] auto IsOwn(NamespaceId id) const -> bool {
    return identities_.IsOwn(id);
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<NamespaceId> {
    return identities_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return identities_.size();
  }

  auto operator==(const NamespaceRegistry&) const -> bool = default;

 private:
  KeyedIdentities<NamespaceKey, NamespaceId> identities_;
  base::Registry<NamespaceDeclaration, NamespaceId> declarations_;
};

// A static variable a subroutine of the namespace declares (LRM 6.21): the
// initializer the source wrote, an expression of the unit's own scope written
// in that scope's procedural scope `written_in`.
struct NamespaceStatic {
  std::optional<ExprId> initializer;
  ProceduralScopeId written_in;

  auto operator==(const NamespaceStatic&) const -> bool = default;
};

// Where the namespace defines a variable it keeps: as a declaration of its own
// scope, or as a static of one of its subroutines.
using NamespaceVariableDefinition =
    std::variant<StructuralDataObjectId, NamespaceStatic>;

// What a namespace unit defines of what it declares: the entry it has for
// itself, and at each position of that entry's declaration, where each
// variable is declared and which subroutine of its scope each subroutine it
// declares is.
struct NamespaceDefinition {
  NamespaceId entry;
  base::Registry<NamespaceVariableDefinition, NamespaceVariableId> variables;
  base::Translation<NamespaceSubroutineId, StructuralSubroutineId> subroutines;

  auto operator==(const NamespaceDefinition&) const -> bool = default;
};

}  // namespace lyra::hir
