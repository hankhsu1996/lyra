#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/translation.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/clocking_block_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

// A clocking skew (LRM 14.3, 14.4) as written, or as LRM 14.3 defaults it for
// its direction: `#1step` for an input, a skew of none for an output. A delay
// may be a parameter (LRM 14.4), so it is the expression the source wrote and
// not a value, and `#0` is the delay of none.
struct OneStepSkew {
  auto operator==(const OneStepSkew&) const -> bool = default;
};
struct NoSkew {
  auto operator==(const NoSkew&) const -> bool = default;
};
struct DelaySkew {
  DelayControl delay;

  auto operator==(const DelaySkew&) const -> bool = default;
};
// An edge of the clocking event's signal, and the delay after it where the
// skew writes one (LRM 14.3).
struct EdgeSkew {
  support::EventEdge edge{};
  std::optional<DelayControl> delay;

  auto operator==(const EdgeSkew&) const -> bool = default;
};

using ClockingInputSkew = std::variant<OneStepSkew, DelaySkew>;
using ClockingOutputSkew = std::variant<NoSkew, DelaySkew, EdgeSkew>;

// The input half of a clockvar (LRM 14.3): `sample` is the variable of the
// block holding what the block last sampled of the signal, which every read of
// the clockvar reads (LRM 14.13).
struct ClockingInput {
  StructuralDataObjectId sample;
  ClockingInputSkew skew;

  auto operator==(const ClockingInput&) const -> bool = default;
};

// An output whose signal is nets drives them as if by a continuous assignment
// from its driving variable at (strong1, strong0), which is 'z until the first
// drive (LRM 14.16). `driving` reads the variable, and `reads` is what that
// read watches.
struct NetSignal {
  ExprId driving;
  std::vector<SensitivityEntry> reads;

  auto operator==(const NetSignal&) const -> bool = default;
};

// An output whose signal is one variable of the scope declaring the block puts
// onto it, once each Re-NBA pass that drives it has run, the value its driving
// variable was left holding, so only the last value a pass's drives give a part
// is driven onto the signal (LRM 14.16.2).
struct VariableSignal {
  auto operator==(const VariableSignal&) const -> bool = default;
};

using ClockingOutputSignal = std::variant<NetSignal, VariableSignal>;

// The output half of a clockvar (LRM 14.3, 14.16). A drive writes `driving`,
// the variable inside the clocking block holding the value the block drives
// onto the signal, which LRM 14.16 names for a net and LRM 14.16.2 describes
// for both; `signal` says how that value reaches the signal.
struct ClockingOutput {
  ClockingOutputSkew skew;
  StructuralDataObjectId driving;
  ClockingOutputSignal signal;

  auto operator==(const ClockingOutput&) const -> bool = default;
};

// A clockvar (LRM 14.3): `signal` is the expression of the declaring scope it
// stands for, and `signal_reads` what it reads, which every change of the
// signal is a change of; an inout has both halves.
struct Clockvar {
  std::string name;
  diag::SourceSpan span;
  ExprId signal;
  std::vector<SensitivityEntry> signal_reads;
  std::optional<ClockingInput> input;
  std::optional<ClockingOutput> output;

  auto operator==(const Clockvar&) const -> bool = default;
};

// A clocking block (LRM 14.3), under the name a hierarchical name writes for
// it (LRM 23.6), one copy of which each instance of its scope holds (LRM
// 14.7). At each occurrence of `event` it samples its inputs and then
// triggers `block_event`, the event its name stands for (LRM 14.10, 14.13); a
// drive of an output lands where the output's skew says (LRM 14.16).
struct ClockingBlockDecl {
  std::string name;
  diag::SourceSpan span;
  EventControl event;
  StructuralDataObjectId block_event;
  base::Translation<ClockvarId, Clockvar> clockvars;

  auto operator==(const ClockingBlockDecl&) const -> bool = default;
};

}  // namespace lyra::hir
