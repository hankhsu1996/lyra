#pragma once

#include <cstdint>
#include <optional>
#include <string_view>

#include "lyra/diag/kind.hpp"

namespace lyra::diag {

// Stable identity for primary diagnostics. Notes have no code. Nothing branches
// per code -- each is looked up by identity for the name and kind it registers,
// and by name to parse one back -- so it is not a dispatch set. What every code
// does need is an entry in that registry: a code without one is a lookup that
// fails while reporting something else, in front of a user.
enum class DiagCode : std::uint32_t {
  kUnsupportedTypeKind,

  kUnsupportedNonStaticVariableLifetime,
  kUnsupportedStructuralMember,
  kUnsupportedStatementForm,
  kUnsupportedExpressionForm,
  kUnsupportedStructuralExpressionForm,
  kUnsupportedNonVariableNamedReference,
  kUnsupportedTimingControlKind,
  kUnsupportedEventTriggerForm,
  kUnsupportedContinuousAssignForm,
  kUnsupportedPortConnectionForm,
  kUnsupportedAssignmentPatternKind,
  kUnsupportedSubroutineArgument,
  kUnsupportedClassFeature,
  kUnsupportedDpi,
  kUnsupportedConversionForm,
  kUnsupportedAssignmentPatternText,

  kErrorTopLevelPortMustBeConnected,
  // The front end accepted the text, a diagnostic it gives having been
  // relaxed, and built nothing there to carry out. Which deviations are
  // tolerated and what can be simulated are separate questions, so this is an
  // error and never a gap the compiler has left.
  kErrorInvalidFrontendSemantics,
  // The front end accepted a cross bin selecting `binsof` a coverpoint the
  // cross does not cross, which the standard gives no meaning (LRM 19.6.1).
  kErrorBinsofOutsideItsCross,
  // The front end accepted an alias between a net of an unpacked aggregate and
  // a net of another type, whose bits the standard overlays only as a packed
  // union's members, which no unpacked aggregate can be (LRM 10.11).
  kErrorAliasAcrossTypes,
  // A name reaches what an assertion declares that the build leaves out of the
  // design, so the design reads something that exists only while it is
  // checked, which leaving assertions out promised it does not.
  kErrorNamesWhatAnAssertionLeftOutDeclares,

  kHostInvalidCliArgs,
  kHostInvalidManifest,
  kHostNoInputFiles,
  kHostIoError,
  kHostBuildFailed,

  kInternalFailure,

  kWarningPedantic,
};

struct DiagCodeInfo {
  DiagKind kind;
  std::string_view name;
};

auto Info(DiagCode code) -> const DiagCodeInfo&;
auto DiagCodeName(DiagCode code) -> std::string_view;
auto DiagCodeKind(DiagCode code) -> DiagKind;
auto ParseDiagCode(std::string_view text) -> std::optional<DiagCode>;

}  // namespace lyra::diag
