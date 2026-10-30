#pragma once

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/keyed_identities.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// Every class one compilation unit names: the ones it declares and the ones it
// reaches in other units, each under one identity and with what it declares.
// What a class's operations do is not here: it is the code of the scope
// replicating the class, held by that scope.
//
// A class variable may be declared before the class itself is, which is how two
// classes each hold a handle to the other and how one holds a handle to itself
// (LRM 8.27). So a class is nameable from the moment the source names it and
// complete only once its declaration is settled, and the two are answered
// separately: the identity arrives with the name, what the class declares
// arrives later.
//
// A pair naming this unit is one of its own classes -- a hierarchical name
// reaching another instance of this unit, or a signature naming one of this
// unit's classes, writes it that way -- so it lands on that class's identity,
// and one class is never two identities here.
class ClassRegistry {
 public:
  explicit ClassRegistry(std::string unit_name)
      : identities_(std::move(unit_name)) {
  }

  // A class this unit declares, at `path`. A class is one declaration of its
  // unit wherever its text was elaborated, so a path declared again is the
  // class already there.
  auto DeclareOwn(support::DefPath path) -> ClassId {
    return Enter(
        ClassKey{
            .unit_name = identities_.UnitName(),
            .class_path = std::move(path)});
  }

  // The class `key` names, given an identity the first time it enters.
  auto Import(const ClassKey& key) -> ClassId {
    if (const std::optional<ClassId> known = Find(key)) {
      return *known;
    }
    if (key.unit_name == identities_.UnitName()) {
      throw InternalError(
          std::format(
              "hir::ClassRegistry::Import: unit '{}' declares no class '{}' "
              "and a name reached one -- please report this as a bug",
              key.unit_name, support::DisplayOf(key.class_path)));
    }
    return Enter(key);
  }

  // The class `key` names, where it has an identity here already.
  [[nodiscard]] auto Find(const ClassKey& key) const -> std::optional<ClassId> {
    return identities_.Find(key);
  }

  // What the class declares: for a class of this unit, computed where it is
  // interned; for one of another unit, what that unit published, read back.
  void SetDeclaration(ClassId id, ClassDeclaration declaration) {
    declarations_.Define(id, std::move(declaration));
  }

  [[nodiscard]] auto HasDeclaration(ClassId id) const -> bool {
    return declarations_.IsDefined(id);
  }

  [[nodiscard]] auto KeyOf(ClassId id) const -> const ClassKey& {
    return identities_.KeyOf(id);
  }

  [[nodiscard]] auto PathOf(ClassId id) const -> const support::DefPath& {
    return KeyOf(id).class_path;
  }

  // Whether this unit declares the class, rather than reaching it in another.
  [[nodiscard]] auto IsOwn(ClassId id) const -> bool {
    return identities_.IsOwn(id);
  }

  // What the class declares, whichever unit declares it: the one answer to
  // every question about a class.
  [[nodiscard]] auto Declaration(ClassId id) const -> const ClassDeclaration& {
    return declarations_.Get(id);
  }

  // The class `id` extends, or nothing where it extends none.
  [[nodiscard]] auto BaseOf(ClassId id) const -> std::optional<ClassId> {
    return Declaration(id).base;
  }

  // Whether `id` is an interface class (LRM 8.26).
  [[nodiscard]] auto IsInterfaceClass(ClassId id) const -> bool {
    return Declaration(id).is_interface_class;
  }

  // The behavior (LRM 8.20) the method `method` answers, named by the method
  // that introduced it, or nothing where the method is in no dispatch.
  [[nodiscard]] auto BehaviorOf(const ClassMethodTarget& method) const
      -> std::optional<ClassMethodTarget> {
    return std::visit(
        Overloaded{
            [](const TypeAssociated&) -> std::optional<ClassMethodTarget> {
              return std::nullopt;
            },
            [](const NotVirtual&) -> std::optional<ClassMethodTarget> {
              return std::nullopt;
            },
            [&](const IntroducesVirtual&) -> std::optional<ClassMethodTarget> {
              return method;
            },
            [&](const OverridesVirtual& overriding)
                -> std::optional<ClassMethodTarget> {
              return IntroducedBy(overriding.overridden);
            }},
        Declaration(method.owner).methods.Get(method.method).dispatch);
  }

  // The method declaring the behavior `behavior` names (LRM 8.20), in the
  // class that introduced it. A class declares at most one member under a
  // name, so the name finds it.
  [[nodiscard]] auto IntroducedBy(const NamedBehavior& behavior) const
      -> ClassMethodTarget {
    const ClassId introducer = behavior.introducer;
    const std::string_view name = behavior.name;
    const std::optional<MethodId> method =
        FindMethod(Declaration(introducer).methods, name);
    if (!method.has_value()) {
      throw InternalError(
          std::format(
              "hir::ClassRegistry::IntroducedBy: class '{}' of unit '{}' "
              "introduces no '{}' -- please report this as a bug",
              support::DisplayOf(PathOf(introducer)),
              KeyOf(introducer).unit_name, name));
    }
    return ClassMethodTarget{.owner = introducer, .method = *method};
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<ClassId> {
    return identities_.Ids();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return identities_.size();
  }

  // The classes this unit declares, and those of other units it reaches, each
  // in the order it named them.
  [[nodiscard]] auto OwnIds() const -> std::vector<ClassId> {
    return identities_.OwnIds();
  }
  [[nodiscard]] auto ImportedIds() const -> std::vector<ClassId> {
    return identities_.ImportedIds();
  }

  auto operator==(const ClassRegistry&) const -> bool = default;

 private:
  auto Enter(ClassKey key) -> ClassId {
    const auto [id, entered] = identities_.Enter(std::move(key));
    if (entered) {
      declarations_.Declare();
    }
    return id;
  }

  KeyedIdentities<ClassKey, ClassId> identities_;
  base::Registry<ClassDeclaration, ClassId> declarations_;
};

}  // namespace lyra::hir
