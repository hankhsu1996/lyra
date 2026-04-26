#include "lyra/diag/diag_code.hpp"

#include <array>
#include <optional>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/support/internal_error.hpp"

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
        DiagCode::kUnsupportedForGenerate,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_for_generate"}},
    std::pair{
        DiagCode::kUnsupportedDeclInGenerate,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_decl_in_generate"}},
    std::pair{
        DiagCode::kUnsupportedProcessInGenerate,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_process_in_generate"}},
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
        DiagCode::kUnsupportedIntegerLiteralWidth,
        DiagCodeInfo{
            .kind = DiagKind::kUnsupported,
            .category = UnsupportedCategory::kFeature,
            .name = "unsupported_integer_literal_width"}},
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
  throw lyra::support::InternalError("diag::Info: unknown DiagCode value");
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
