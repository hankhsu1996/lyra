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
  kUnsupportedAssociativeArrayType,
  kUnsupportedTypeKind,

  kUnsupportedNonStaticVariableLifetime,
  kUnsupportedStructuralMember,
  kUnsupportedStatementForm,
  kUnsupportedExpressionForm,
  kUnsupportedStructuralExpressionForm,
  kUnsupportedNonVariableNamedReference,
  kUnsupportedAssignmentTarget,
  kUnsupportedTimingControlKind,
  kUnsupportedDelayExpressionForm,
  kUnsupportedEventTriggerForm,
  kUnsupportedContinuousAssignForm,
  kUnsupportedPortConnectionForm,
  kUnsupportedAssignmentPatternKind,
  kUnsupportedSubroutineArgument,
  kUnsupportedClassFeature,
  kUnsupportedDpi,
  kUnsupportedConversionForm,

  kErrorDelayValueOutOfRange,
  kErrorCaseEqualityOnRealOperand,
  kErrorFormatStringTrailingPercent,
  kErrorFormatStringMissingPrecision,
  kErrorFormatStringWidthOverflow,
  kErrorFormatStringUnknownSpecifier,
  kErrorFormatStringModifierNotPermitted,
  kErrorDisplayMissingArg,

  kHostInvalidCliArgs,
  kHostInvalidManifest,
  kHostNoInputFiles,
  kHostIoError,
  kHostBuildFailed,
  kHostBackendUnimplemented,

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
