#pragma once

#include <compare>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

struct CompilationUnit;

// Where a unit holds a callable's code, as rustc's `MirSource` names the item
// a body belongs to. Every code a unit holds is at exactly one of these, and a
// callable is at the target a call names it by.
struct ConstructorSource {
  ClassId owner;

  auto operator<=>(const ConstructorSource&) const
      -> std::strong_ordering = default;
};
struct RecordMethodSource {
  RecordId record;
  support::ValueOperation answers;

  auto operator<=>(const RecordMethodSource&) const
      -> std::strong_ordering = default;
};
struct TypeOperationSource {
  TypeId type;
  support::ValueOperation answers;

  auto operator<=>(const TypeOperationSource&) const
      -> std::strong_ordering = default;
};
struct ClosureSource {
  ClosureId closure;

  auto operator<=>(const ClosureSource&) const
      -> std::strong_ordering = default;
};
struct ForeignScopeEntrySource {
  std::string foreign_name;

  auto operator<=>(const ForeignScopeEntrySource&) const
      -> std::strong_ordering = default;
};
using CodeSource = std::variant<
    ConstructorSource, CallableTarget, UnitCallableTarget, RecordMethodSource,
    TypeOperationSource, ClosureSource, ForeignScopeEntrySource>;

// The source by what a reader of a message knows it as: the name the source
// gave it where it gave one, and where it sits otherwise.
[[nodiscard]] auto Describe(const CompilationUnit& unit, const CodeSource& at)
    -> std::string;

// Whether the code at `at` is a subroutine the foreign program defines (LRM
// 35.4), entered as its declaration states rather than as this program enters
// its own bodies. The body a foreign name's scope entries share is this
// program's own, and takes by value only what is small (LRM H.8.1).
[[nodiscard]] auto IsEnteredForeign(
    const CompilationUnit& unit, const CodeSource& at) -> bool;

// The code at a source, and the nodes of its body that no statement reaches
// but the source still names: what a constructor enters its base's
// constructor with (LRM 8.7), and nothing anywhere else.
struct SourcedCode {
  CallableCode code;
  std::vector<ExprId> outside_roots;
};

using CodeVisit = std::function<void(
    const CodeSource&, const CallableCode&, std::span<const ExprId>)>;

// Calls `visit` with every code `unit` holds, a definition or a declaration
// alike, with its source and its outside roots.
void ForEachCode(const CompilationUnit& unit, const CodeVisit& visit);

using CodeRewrite = std::function<std::optional<SourcedCode>(
    const CompilationUnit&, const CodeSource&, const CallableCode&,
    std::span<const ExprId>)>;

// `unit` with the code at every source `rewrite` answers for replaced by its
// answer, and the code it answers nothing for kept as it is. Every code is
// asked of the unit, which `rewrite` is handed, as it was before any was
// replaced, so a rewrite reads the whole unit, other code included, the same
// way whichever it is asked of first.
[[nodiscard]] auto RewriteEachCode(
    CompilationUnit unit, const CodeRewrite& rewrite) -> CompilationUnit;

}  // namespace lyra::mir
