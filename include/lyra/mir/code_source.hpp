#pragma once

#include <compare>
#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

struct CompilationUnit;

// Where a callable's code sits, as rustc's `MirSource` names the item a body
// belongs to. Every code a unit holds is at exactly one of these: a callable of
// the unit's table at the target a call names it by, whoever declares it, the
// definition a foreign name's scope entries share, and a body built below MIR,
// which the unit does not hold, named by its type and the operation.
struct TypeOperationSource {
  TypeId type;
  support::ValueOperation answers;
  std::vector<TypeId> arguments = {};

  auto operator<=>(const TypeOperationSource&) const
      -> std::strong_ordering = default;
};

// A body built for an operation of a type no declaration states it for, at
// the type it answers it at: what a target resolving types where a call is
// written renders apart from the unit, where the other target lowers it with
// the unit.
struct TypeOperationBody {
  TypeOperationSource source;
  CallableCode code;
};

struct ForeignScopeEntrySource {
  std::string foreign_name;

  auto operator<=>(const ForeignScopeEntrySource&) const
      -> std::strong_ordering = default;
};
using CodeSource =
    std::variant<CallableTarget, TypeOperationSource, ForeignScopeEntrySource>;

// The source by what a reader of a message knows it as: the name the source
// gave it where it gave one, and where it sits otherwise.
[[nodiscard]] auto Describe(const CompilationUnit& unit, const CodeSource& at)
    -> std::string;

// The class `owner` by what a reader of a message knows it as, the same way.
[[nodiscard]] auto DescribeClass(const CompilationUnit& unit, ClassId owner)
    -> std::string;

// Whether the code at `at` is a subroutine the foreign program defines (LRM
// 35.4), entered as its declaration states rather than as this program enters
// its own bodies. The body a foreign name's scope entries share is this
// program's own, and takes by value only what is small (LRM H.8.1).
[[nodiscard]] auto IsEnteredForeign(
    const CompilationUnit& unit, const CodeSource& at) -> bool;

using CodeVisit = std::function<void(const CodeSource&, const CallableCode&)>;

// Calls `visit` with every code `unit` holds, a definition or a declaration
// alike, with its source.
void ForEachCode(const CompilationUnit& unit, const CodeVisit& visit);

using CodeRewrite = std::function<std::optional<CallableCode>(
    const CompilationUnit&, const CodeSource&, const CallableCode&)>;

// `unit` with the code at every source `rewrite` answers for replaced by its
// answer, and the code it answers nothing for kept as it is. Every code is
// asked of the unit, which `rewrite` is handed, as it was before any was
// replaced, so a rewrite reads the whole unit, other code included, the same
// way whichever it is asked of first.
[[nodiscard]] auto RewriteEachCode(
    CompilationUnit unit, const CodeRewrite& rewrite) -> CompilationUnit;

}  // namespace lyra::mir
