#pragma once

#include <cstdint>
#include <optional>
#include <string_view>

#include "lyra/diag/kind.hpp"

namespace lyra::diag {

// Stable identity for primary diagnostics. Notes have no code.
enum class DiagCode : std::uint32_t {
  kUnsupportedPackedArrayElementType,
  kUnsupportedTaggedPackedUnion,
  kUnsupportedFixedSizeUnpackedArrayType,
  kUnsupportedDynamicArrayType,
  kUnsupportedAssociativeArrayType,
  kUnsupportedUnpackedStructType,
  kUnsupportedUnpackedUnionType,
  kUnsupportedTypeKind,

  kUnsupportedNonStaticVariableLifetime,
  kUnsupportedStatementForm,
  kUnsupportedExpressionForm,
  kUnsupportedStructuralExpressionForm,
  kUnsupportedNonVariableNamedReference,
  kUnsupportedAssignmentTarget,
  kUnsupportedBinaryOperator,
  kUnsupportedTimingControlKind,
  kUnsupportedDelayExpressionForm,
  kUnsupportedEventTriggerForm,
  kUnsupportedContinuousAssignForm,
  kUnsupportedPortConnectionForm,
  kUnsupportedAssignmentPatternKind,
  kUnsupportedSubroutineArgument,
  kUnsupportedForkJoinForm,

  kDelayValueOutOfRange,
  kCaseEqualityOnRealOperand,
  kFormatStringTrailingPercent,
  kFormatStringMissingSpecifier,
  kFormatStringWidthOverflow,
  kFormatStringUnknownSpecifier,
  kDisplayMissingArg,
  kFormatModulePathNotImplemented,
  kSystemSubroutineExecutionNotImplemented,

  kCppEmitExpressionFormNotImplemented,

  kHostInvalidCliArgs,
  kHostProjectModeUnimplemented,
  kHostNoInputFiles,
  kHostIoError,
  kHostBuildFailed,

  kWarningPedantic,
};

struct DiagCodeInfo {
  DiagKind kind;
  std::optional<UnsupportedCategory> category;
  std::string_view name;
};

auto Info(DiagCode code) -> const DiagCodeInfo&;
auto DiagCodeName(DiagCode code) -> std::string_view;
auto DiagCodeKind(DiagCode code) -> DiagKind;
auto DiagCodeCategory(DiagCode code) -> std::optional<UnsupportedCategory>;
auto ParseDiagCode(std::string_view text) -> std::optional<DiagCode>;

}  // namespace lyra::diag
