#pragma once

#include <optional>
#include <string>
#include <variant>

#include "lyra/base/diag_message.hpp"
#include "slang/ast/SemanticFacts.h"

namespace slang::ast {
class ClockVarSymbol;
class TimingControl;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A clockvar's skew in force for one direction (LRM 14.3, 14.4): its own, or
// its block's default for that direction, or where neither was written the
// standard's: `#1step` for an input and a skew of none for an output. A delay
// may be a parameter (LRM 14.4), so it is a value of the instance and never
// read here.
struct OneStepSkewed {};
struct Unskewed {};
struct DelaySkewed {
  const slang::ast::TimingControl* delay = nullptr;
};
// An edge, and the delay after it the skew may write (LRM 14.3).
struct EdgeSkewed {
  slang::ast::EdgeKind edge = slang::ast::EdgeKind::None;
  const slang::ast::TimingControl* delay = nullptr;
};
using InputSkewKind = std::variant<OneStepSkewed, DelaySkewed, EdgeSkewed>;
using OutputSkewKind =
    std::variant<Unskewed, OneStepSkewed, DelaySkewed, EdgeSkewed>;

[[nodiscard]] auto InputSkewOf(const slang::ast::ClockVarSymbol& clockvar)
    -> InputSkewKind;
[[nodiscard]] auto OutputSkewOf(const slang::ast::ClockVarSymbol& clockvar)
    -> OutputSkewKind;

// What an output clockvar's signal is (LRM 14.16): nets only, each given a
// driver the block's own variable sets; one whole variable of the scope
// declaring the block, `variable`, which every output onto it sets once a
// Re-NBA pass driving it has run; or anything else, which no output is
// carried for.
struct NetSignal {};
struct VariableSignal {
  const slang::ast::VariableSymbol* variable = nullptr;
};
struct UncarriedSignal {};
using OutputSignal = std::variant<NetSignal, VariableSignal, UncarriedSignal>;

[[nodiscard]] auto OutputSignalOf(const slang::ast::ClockVarSymbol& clockvar)
    -> OutputSignal;

// The variable inside a clocking block that a drive of the output `clockvar`
// writes, as its scope publishes it: a net output's own, named after the
// clockvar and held by its block (LRM 14.16); and for a variable output the one
// every output onto that variable shares, from whichever block of the scope,
// named after the variable and held by the scope declaring the blocks, so the
// drives of one pass reach the variable in the order they land (LRM 14.16.2).
struct DrivingVariableNaming {
  std::string name;
  bool held_by_scope = false;
};
[[nodiscard]] auto DrivingVariableNamingOf(
    const slang::ast::ClockVarSymbol& clockvar) -> DrivingVariableNaming;

// Why the input half of `clockvar`, or its output half, is not carried, or
// nothing where it is. A half not carried is no part of the block, and a use
// reaching it is refused with this.
[[nodiscard]] auto InputRefusal(const slang::ast::ClockVarSymbol& clockvar)
    -> std::optional<base::DiagMessage>;
[[nodiscard]] auto OutputRefusal(const slang::ast::ClockVarSymbol& clockvar)
    -> std::optional<base::DiagMessage>;

// Whether a clockvar has an input half (LRM 14.3: input and inout).
[[nodiscard]] auto IsClockingInput(const slang::ast::ClockVarSymbol& clockvar)
    -> bool;

// Whether a clockvar has an output half (LRM 14.3: output and inout).
[[nodiscard]] auto IsClockingOutput(const slang::ast::ClockVarSymbol& clockvar)
    -> bool;

}  // namespace lyra::lowering::ast_to_hir
