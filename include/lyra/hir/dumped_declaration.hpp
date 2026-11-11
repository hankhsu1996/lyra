#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/support/dump_kind.hpp"

namespace lyra::hir {

// One data object a scope declares that a value dump may record (LRM
// 21.7.1.2): a variable, a net, a port or a named event, as the dump records
// it -- its name, what it is declared as, how many bits it holds, the range of
// a vector, which the dump writes beside the name (LRM 21.7.2.3 `$var`), and
// where it is what a port of the scope connects, which an extended dump
// records alone (LRM 21.7.3.1) -- the expression a body of the scope names it
// with, and, for a static variable of a block, a task or a function, the
// procedural scope enclosing it, which the dump writes a `$scope` around it
// for where the source named it.
struct DumpedDeclaration {
  std::string identifier;
  support::DumpVarKind kind = support::DumpVarKind::kWire;
  std::uint32_t width = 0;
  struct Range {
    std::int64_t left;
    std::int64_t right;

    auto operator==(const Range&) const -> bool = default;
  };
  std::optional<Range> range;
  // The port's direction, and its position among the ports in the order the
  // declaration gives them (LRM 21.7.4.2).
  struct Port {
    support::DumpPortDirection direction;
    std::uint32_t position;

    auto operator==(const Port&) const -> bool = default;
  };
  std::optional<Port> port;
  ExprId named;
  std::optional<ProceduralScopeId> enclosing;

  auto operator==(const DumpedDeclaration&) const -> bool = default;
};

}  // namespace lyra::hir
