#pragma once

#include <cstddef>
#include <format>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::support {

// A type the runtime library declares as an IR names it: its declaration and
// the arguments the language gives it, each a type of that IR or a queue's
// bound, as rustc's `Adt(def, args)` names a definition of a library crate.
// Every IR naming one names it the same way, so what is read of its arguments
// is read the same way in each.
template <typename TypeId>
struct LibraryType {
  // One argument, of the kind its declaration's parameter at that position
  // takes.
  using Arg = std::variant<TypeId, QueueBound>;

  LibraryDecl decl;
  std::vector<Arg> args;

  // The type the argument taking `role` names, and nothing where the
  // declaration has no parameter taking it.
  [[nodiscard]] auto FindTypeArg(LibraryParam role) const
      -> std::optional<TypeId> {
    const Arg* arg = FindArg(role);
    if (arg == nullptr) {
      return std::nullopt;
    }
    const auto* type = std::get_if<TypeId>(arg);
    if (type == nullptr) {
      throw InternalError(
          "support: an argument of a library type that is no type was read "
          "as one -- please report this as a bug");
    }
    return *type;
  }

  // The same where the declaration has to take it, which asking of one that
  // does not is a caller's error.
  [[nodiscard]] auto TypeArg(LibraryParam role) const -> TypeId {
    const std::optional<TypeId> type = FindTypeArg(role);
    if (!type.has_value()) {
      RefuseMissing();
    }
    return *type;
  }

  // The type of the values a history keeps, which its declaration names by
  // the parameter its member states, and nothing for every other declaration.
  [[nodiscard]] auto FindValuesHeld() const -> std::optional<TypeId> {
    using Held = std::optional<TypeId>;
    return std::visit(
        Overloaded{
            [&](const HistoryMember& history) -> Held {
              return TypeArg(history.values);
            },
            [](const NoMember&) -> Held { return std::nullopt; },
            [](const ValueMember&) -> Held { return std::nullopt; },
            [](const OwnObjectMember&) -> Held { return std::nullopt; },
            [](const BorrowedMember&) -> Held { return std::nullopt; }},
        DeclarationOf(decl).member);
  }

  // How many elements a queue may hold (LRM 7.10), and nothing where the
  // declaration takes no bound.
  [[nodiscard]] auto FindBound() const -> std::optional<QueueBound> {
    const Arg* arg = FindArg(LibraryParam::kBound);
    if (arg == nullptr) {
      return std::nullopt;
    }
    const auto* bound = std::get_if<QueueBound>(arg);
    if (bound == nullptr) {
      throw InternalError(
          "support: a queue's bound was read from an argument that is no "
          "bound -- please report this as a bug");
    }
    return *bound;
  }

  // The same where the declaration has to take one.
  [[nodiscard]] auto Bound() const -> QueueBound {
    const std::optional<QueueBound> bound = FindBound();
    if (!bound.has_value()) {
      RefuseMissing();
    }
    return *bound;
  }

  auto operator==(const LibraryType&) const -> bool = default;

 private:
  // The argument at the position of the declaration's parameter taking
  // `role`, and nothing where none takes it. One the declaration takes and
  // the type was not named with is a type the verification refuses.
  [[nodiscard]] auto FindArg(LibraryParam role) const -> const Arg* {
    const std::optional<std::size_t> position = ParamOf(decl, role);
    if (!position.has_value()) {
      return nullptr;
    }
    if (*position >= args.size()) {
      throw InternalError(
          std::format(
              "support: a {} is named with fewer arguments than its "
              "declaration takes -- please report this as a bug",
              DeclarationOf(decl).name));
    }
    return &args[*position];
  }

  // Asking for an argument the declaration takes no parameter for, which is
  // a caller's error.
  [[noreturn]] void RefuseMissing() const {
    throw InternalError(
        std::format(
            "support: an argument of a {} its declaration takes no parameter "
            "for was asked for -- please report this as a bug",
            DeclarationOf(decl).name));
  }
};

}  // namespace lyra::support
