#pragma once

#include <cstdint>
#include <optional>
#include <string_view>

#include "lyra/diag/kind.hpp"

namespace lyra::diag {

// Stable identity for primary diagnostics. Notes have no code.
enum class DiagCode : std::uint32_t {
  kUnsupportedPackedArrayElementType,
  kUnsupportedPackedStructType,
  kUnsupportedPackedUnionType,
  kUnsupportedEnumType,
  kUnsupportedFixedSizeUnpackedArrayType,
  kUnsupportedDynamicArrayType,
  kUnsupportedQueueType,
  kUnsupportedAssociativeArrayType,
  kUnsupportedUnpackedStructType,
  kUnsupportedUnpackedUnionType,
  kUnsupportedTypeKind,

  kUnsupportedNonStaticVariableLifetime,
  kUnsupportedNonInitialProcedure,
  kUnsupportedForGenerate,
  kUnsupportedStatementForm,
  kUnsupportedExpressionForm,
  kUnsupportedStructuralExpressionForm,
  kUnsupportedIntegerLiteralWidth,
  kUnsupportedNonVariableNamedReference,
  kUnsupportedNonBlockingAssignment,
  kUnsupportedCompoundAssignment,
  kUnsupportedAssignmentTarget,
  kUnsupportedBinaryOperator,

  kHostInvalidCliArgs,
  kHostProjectModeUnimplemented,
  kHostNoInputFiles,
  kHostIoError,
  kHostExpectedSingleTopModule,

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
