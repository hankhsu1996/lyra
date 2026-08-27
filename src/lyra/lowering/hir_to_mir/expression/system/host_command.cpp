#include "lyra/lowering/hir_to_mir/expression/system/host_command.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHostCommandSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  auto& block = *frame.current_block;

  std::vector<mir::ExprId> args;
  if (const std::optional<hir::ExprId> command = OptionalOperand(call, 0)) {
    auto lowered =
        process.LowerExpr(process.HirBody().exprs.Get(*command), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    // Only a form that actually runs a command reaches the engine, and only to
    // publish the design's own pending output before a child process can write
    // over it; the null command runs nothing and observes the host alone.
    args.push_back(
        block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner())));
    // A command line written as an unsized literal arrives packed; the runtime
    // signature is on SV `string`, so the operand takes the value-layer
    // conversion the store boundary would give it.
    args.push_back(ConvertToType(
        unit, block, block.exprs.Add(*std::move(lowered)),
        unit.builtins.string));
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kRunHostCommand},
              .arguments = std::move(args)},
      .type = unit.builtins.int_type};
}

}  // namespace lyra::lowering::hir_to_mir
