#include "lyra/lowering/hir_to_mir/type_operation_body.hpp"

#include <expected>
#include <optional>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/bound_free_conversion.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The default of a data type that is no array: each component's for a tuple,
// and for any other type what its declaration or the library states.
auto BuildDefaultBody(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<mir::CallableCode> {
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = type;
  mir::Builder block(code.Body(), diag::SourceSpan{});
  std::expected<mir::Expr, std::string_view> built =
      DefaultValueOf(unit, block, type);
  if (!built) {
    return std::nullopt;
  }
  block.AppendStmt(mir::ReturnStmt{.value = block.Add(*std::move(built))});
  return code;
}

}  // namespace

auto MakeTypeOperationBody(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::CallableCode> {
  const std::optional<mir::TypeOperationKind> kind =
      mir::TypeOperationKindOf(unit.types, type, answers);
  if (!kind.has_value()) {
    return std::nullopt;
  }
  switch (*kind) {
    case mir::TypeOperationKind::kToBoundFree:
      return BuildToBoundFreeBody(unit, type);
    case mir::TypeOperationKind::kFromBoundFree:
      return BuildFromBoundFreeBody(unit, type);
    case mir::TypeOperationKind::kStructural:
      return BuildStructuralOperationBody(unit, type, answers);
    case mir::TypeOperationKind::kDefault:
      return BuildDefaultBody(unit, type);
  }
  throw InternalError("hir_to_mir: unknown kind of type operation");
}

}  // namespace lyra::lowering::hir_to_mir
