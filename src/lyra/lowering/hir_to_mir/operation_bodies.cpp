#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::DirectTarget target, support::ValueOperation answers,
    std::optional<mir::ExprId> receiver, std::vector<mir::ExprId> operands,
    mir::TypeId result) -> mir::ExprId {
  const std::optional<mir::CallableCode> declared = mir::OperationDeclarationOf(
      unit, mir::TypeOperationSource{.type = type, .answers = answers});
  if (!declared.has_value()) {
    throw InternalError(
        "hir_to_mir: a call names an operation of a type that declares none "
        "such -- please report this as a bug");
  }
  mir::PassedOperands passed = mir::PassedAsDeclared(
      unit, block, *declared, receiver, std::move(operands));
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = std::move(target),
                          .receiver = passed.receiver},
                  .arguments = std::move(passed.arguments)},
          .type = result});
}

}  // namespace lyra::lowering::hir_to_mir
