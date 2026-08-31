#include "lyra/backend/cpp/value_form.hpp"

#include <string>
#include <string_view>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto ConcatValueForm(const mir::Type& result) -> ValueForm {
  if (!result.IsIntegralPacked()) {
    throw InternalError(
        "ConcatValueForm: only an integral concatenation is a build "
        "primitive");
  }
  return {.member = "Concat", .grouping = OperandGrouping::kSequenceArgument};
}

auto ReplicationValueForm(const mir::Type& result) -> ValueForm {
  if (!result.IsIntegralPacked()) {
    throw InternalError(
        "ReplicationValueForm: only an integral replication is a build "
        "primitive");
  }
  return {.member = "Replicate", .grouping = OperandGrouping::kArgumentList};
}

auto RenderValueForm(
    const ValueForm& form, std::string_view result_type,
    const std::vector<std::string>& operands) -> std::string {
  const std::string callee =
      std::string{result_type} + "::" + std::string{form.member};
  switch (form.grouping) {
    case OperandGrouping::kArgumentList:
      return CallOf(callee, operands);
    case OperandGrouping::kSequenceArgument:
      return CallOf(callee, {BracedListOf(operands)});
  }
  throw InternalError("RenderValueForm: unknown operand grouping");
}

}  // namespace lyra::backend::cpp
