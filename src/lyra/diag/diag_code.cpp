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
        DiagCode::kUnsupportedPackedArrayElementType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_packed_array_element_type"}},
    std::pair{
        DiagCode::kUnsupportedPackedStructType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_packed_struct_type"}},
    std::pair{
        DiagCode::kUnsupportedPackedUnionType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_packed_union_type"}},
    std::pair{
        DiagCode::kUnsupportedEnumType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_enum_type"}},
    std::pair{
        DiagCode::kUnsupportedFixedSizeUnpackedArrayType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_fixed_size_unpacked_array_type"}},
    std::pair{
        DiagCode::kUnsupportedDynamicArrayType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_dynamic_array_type"}},
    std::pair{
        DiagCode::kUnsupportedQueueType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_queue_type"}},
    std::pair{
        DiagCode::kUnsupportedAssociativeArrayType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_associative_array_type"}},
    std::pair{
        DiagCode::kUnsupportedUnpackedStructType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_unpacked_struct_type"}},
    std::pair{
        DiagCode::kUnsupportedUnpackedUnionType,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_unpacked_union_type"}},
    std::pair{
        DiagCode::kUnsupportedTypeKind,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kType,
            .name = "unsupported_type_kind"}},

    std::pair{
        DiagCode::kUnsupportedNonStaticVariableLifetime,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_non_static_variable_lifetime"}},
    std::pair{
        DiagCode::kUnsupportedNonInitialProcedure,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_non_initial_procedure"}},
    std::pair{
        DiagCode::kUnsupportedStatementForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_statement_form"}},
    std::pair{
        DiagCode::kUnsupportedExpressionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kOperation,
            .name = "unsupported_expression_form"}},
    std::pair{
        DiagCode::kUnsupportedStructuralExpressionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_structural_expression_form"}},
    std::pair{
        DiagCode::kUnsupportedNonVariableNamedReference,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_non_variable_named_reference"}},
    std::pair{
        DiagCode::kUnsupportedNonBlockingAssignment,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_non_blocking_assignment"}},
    std::pair{
        DiagCode::kUnsupportedCompoundAssignment,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_compound_assignment"}},
    std::pair{
        DiagCode::kUnsupportedAssignmentTarget,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_assignment_target"}},
    std::pair{
        DiagCode::kUnsupportedBinaryOperator,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kOperation,
            .name = "unsupported_binary_operator"}},
    std::pair{
        DiagCode::kUnsupportedTimingControlKind,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_timing_control_kind"}},
    std::pair{
        DiagCode::kUnsupportedDelayExpressionForm,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_delay_expression_form"}},

    std::pair{
        DiagCode::kDelayValueOutOfRange,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "delay_value_out_of_range"}},
    std::pair{
        DiagCode::kFormatStringTrailingPercent,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "format_string_trailing_percent"}},
    std::pair{
        DiagCode::kFormatStringMissingSpecifier,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "format_string_missing_specifier"}},
    std::pair{
        DiagCode::kFormatStringWidthOverflow,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "format_string_width_overflow"}},
    std::pair{
        DiagCode::kFormatStringUnknownSpecifier,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "format_string_unknown_specifier"}},
    std::pair{
        DiagCode::kDisplayMissingArg,
        DiagCodeInfo{
            .kind = DiagKind::kError,
            .category = std::nullopt,
            .name = "display_missing_arg"}},
    std::pair{
        DiagCode::kFileDisplayNotImplemented,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "file_display_not_implemented"}},
    std::pair{
        DiagCode::kFormatModulePathNotImplemented,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "format_module_path_not_implemented"}},
    std::pair{
        DiagCode::kFormatSpecifierNotImplemented,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "format_specifier_not_implemented"}},
    std::pair{
        DiagCode::kSystemSubroutineExecutionNotImplemented,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "system_subroutine_execution_not_implemented"}},

    std::pair{
        DiagCode::kHostInvalidCliArgs,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .category = std::nullopt,
            .name = "host_invalid_cli_args"}},
    std::pair{
        DiagCode::kHostProjectModeUnimplemented,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .category = std::nullopt,
            .name = "host_project_mode_unimplemented"}},
    std::pair{
        DiagCode::kHostNoInputFiles,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .category = std::nullopt,
            .name = "host_no_input_files"}},
    std::pair{
        DiagCode::kHostIoError,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .category = std::nullopt,
            .name = "host_io_error"}},
    std::pair{
        DiagCode::kHostExpectedSingleTopModule,
        DiagCodeInfo{
            .kind = DiagKind::kHostError,
            .category = std::nullopt,
            .name = "host_expected_single_top_module"}},

    std::pair{
        DiagCode::kWarningPedantic,
        DiagCodeInfo{
            .kind = DiagKind::kWarning,
            .category = std::nullopt,
            .name = "warning_pedantic"}},
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

auto DiagCodeCategory(DiagCode code) -> std::optional<UnsupportedCategory> {
  return Info(code).category;
}

auto ParseDiagCode(std::string_view text) -> std::optional<DiagCode> {
  for (const auto& [c, info] : kEntries) {
    if (info.name == text) return c;
  }
  return std::nullopt;
}

}  // namespace lyra::diag
