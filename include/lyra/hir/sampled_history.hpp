#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

struct SampledHistoryId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const SampledHistoryId&) const
      -> std::strong_ordering = default;
};

// What a scope keeps so a sampled value function can answer across the ticks of
// a clocking event (LRM 16.9.3). One serves every read of one expression under
// one event, and the event is what separates two of them: reads that name
// differently gated events count different ticks, so they are different
// histories even where the expression is the same.
//
// `subject` is the expression a tick settles, evaluated over its variables'
// sampled values (LRM 16.5.1). What is kept is that value and not the variables
// it read, because a function call in the expression is called on the sampled
// values of its arguments at that tick, and calling it again where the answer
// is read is a different call.
//
// `clock` is the event already carrying the LRM 9.4.2.3 qualifier where the
// source gated it, which is what `$past`'s third argument states: it gates the
// clock rather than the expression, so a tick the gate does not admit is not a
// prior tick.
//
// `depth` is how far back any read of this history reaches -- 1 for a value
// change function, which compares against the most recent prior tick, and the
// largest count any `$past` names. That count is an elaboration-time constant
// (LRM 16.9.3), so the depth is settled where the storage is declared.
struct SampledHistoryDecl {
  ExprId subject;
  EventControl clock;
  std::uint32_t depth = 0;
};

}  // namespace lyra::hir
