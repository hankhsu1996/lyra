#include "lyra/diag/diag_code.hpp"

#include <array>
#include <optional>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"

namespace lyra::diag {
namespace {

constexpr std::array kEntries{
    std::pair{
        DiagCode::kUnsupportedAssociativeArrayType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_associative_array_type"}},
    std::pair{
        DiagCode::kUnsupportedTypeKind,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported, .name = "unsupported_type_kind"}},

    std::pair{
        DiagCode::kUnsupportedNonStaticVariableLifetime,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_non_static_variable_lifetime"}},
    std::pair{
        DiagCode::kUnsupportedStructuralMember,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_structural_member"}},
    std::pair{
        DiagCode::kUnsupportedStatementForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_statement_form"}},
    std::pair{
        DiagCode::kUnsupportedExpressionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_expression_form"}},
    std::pair{
        DiagCode::kUnsupportedStructuralExpressionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_structural_expression_form"}},
    std::pair{
        DiagCode::kUnsupportedNonVariableNamedReference,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_non_variable_named_reference"}},
    std::pair{
        DiagCode::kUnsupportedAssignmentTarget,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_assignment_target"}},
    std::pair{
        DiagCode::kUnsupportedTimingControlKind,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_timing_control_kind"}},
    std::pair{
        DiagCode::kUnsupportedEventTriggerForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_event_trigger_form"}},
    std::pair{
        DiagCode::kUnsupportedContinuousAssignForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_continuous_assign_form"}},
    std::pair{
        DiagCode::kUnsupportedPortConnectionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_port_connection_form"}},
    std::pair{
        DiagCode::kUnsupportedAssignmentPatternKind,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_assignment_pattern_kind"}},
    std::pair{
        DiagCode::kUnsupportedSubroutineArgument,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_subroutine_argument"}},
    std::pair{
        DiagCode::kUnsupportedClassFeature,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_class_feature"}},
    std::pair{
        DiagCode::kUnsupportedDpi,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported, .name = "unsupported_dpi"}},
    std::pair{
        DiagCode::kUnsupportedConversionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .name = "unsupported_conversion_form"}},

    std::pair{
        DiagCode::kErrorCaseEqualityOnRealOperand,
        DiagCodeInfo{
            .kind = DiagKind::kError, .name = "case_equality_on_real_operand"}},
    std::pair{
        DiagCode::kErrorFormatStringTrailingPercent,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .name = "format_string_trailing_percent"}},
    std::pair{
        DiagCode::kErrorFormatStringMissingPrecision,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .name = "format_string_missing_precision"}},
    std::pair{
        DiagCode::kErrorFormatStringWidthOverflow,
        DiagCodeInfo{
            .kind = DiagKind::kError, .name = "format_string_width_overflow"}},
    std::pair{
        DiagCode::kErrorFormatStringUnknownSpecifier,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .name = "format_string_unknown_specifier"}},
    std::pair{
        DiagCode::kErrorFormatStringModifierNotPermitted,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .name = "format_string_modifier_not_permitted"}},
    std::pair{
        DiagCode::kErrorDisplayMissingArg,
        DiagCodeInfo{.kind = DiagKind::kError, .name = "display_missing_arg"}},

    std::pair{
        DiagCode::kHostInvalidCliArgs,
        DiagCodeInfo{
            .kind = DiagKind::kHostError, .name = "host_invalid_cli_args"}},
    std::pair{
        DiagCode::kHostInvalidManifest,
        DiagCodeInfo{
            .kind = DiagKind::kHostError, .name = "host_invalid_manifest"}},
    std::pair{
        DiagCode::kHostNoInputFiles,
        DiagCodeInfo{
            .kind = DiagKind::kHostError, .name = "host_no_input_files"}},
    std::pair{
        DiagCode::kHostIoError,
        DiagCodeInfo{.kind = DiagKind::kHostError, .name = "host_io_error"}},
    std::pair{
        DiagCode::kHostBuildFailed,
        DiagCodeInfo{
            .kind = DiagKind::kHostError, .name = "host_build_failed"}},
    std::pair{
        DiagCode::kHostBackendUnimplemented,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .name = "host_backend_unimplemented"}},

    std::pair{
        DiagCode::kWarningPedantic,
        DiagCodeInfo{.kind = DiagKind::kWarning, .name = "warning_pedantic"}},
};

}  // namespace

auto Info(DiagCode code) -> const DiagCodeInfo& {
  for (const auto& [c, info] : kEntries) {
    if (c == code) return info;
  }
  throw InternalError("diag::Info: unknown DiagCode value");
}

auto DiagCodeName(DiagCode code) -> std::string_view {
  return Info(code).name;
}

auto DiagCodeKind(DiagCode code) -> DiagKind {
  return Info(code).kind;
}

auto ParseDiagCode(std::string_view text) -> std::optional<DiagCode> {
  for (const auto& [c, info] : kEntries) {
    if (info.name == text) return c;
  }
  return std::nullopt;
}

}  // namespace lyra::diag
