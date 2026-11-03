#include "lyra/lowering/hir_to_mir/assertion_control.hpp"

#include <cstdint>

#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto ReadAllowed(
    ProcessLowerer& process, const WalkFrame& frame,
    hir::ControlledAssertionId assertion) -> mir::LocalId {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  const mir::TypeId word = unit.builtins.machine_word;
  const mir::ExprId allowed = block.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kAssertionControlAllowed,
              .receiver = process.ControlPlace(frame, assertion)},
          {}, word));
  return DeclareLocal(frame, allowed);
}

auto Allows(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId allowed,
    std::uint64_t what) -> mir::ExprId {
  const mir::TypeId word = unit.builtins.machine_word;
  const auto literal = [&](std::uint64_t value) {
    return block.Add(
        mir::Expr{
            .data =
                mir::MachineIntLiteral{
                    .value = static_cast<std::int64_t>(value)},
            .type = word});
  };
  const mir::ExprId masked = block.Add(
      mir::MakeBinary(
          unit, block.Body(), mir::BinaryOp::kBitwiseAnd,
          block.ReadLocal(allowed, word), literal(what), word));
  return block.Add(
      mir::MakeBinary(
          unit, block.Body(), mir::BinaryOp::kInequality, masked, literal(0),
          unit.builtins.machine_bool));
}

auto ControlAddress(
    mir::CompilationUnit& unit, mir::Builder block, const WalkFrame& frame,
    const StaticCellBinding& control) -> mir::Expr {
  const mir::ExprId cell = block.Add(BuildStaticStorageAccess(
      unit, frame, control.home, control.cell_type, mir::EnclosingHops{}));
  return mir::MakeAddressOfExpr(
      cell, unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = control.cell_type,
                    .ownership = mir::PointerOwnership::kBorrowed}}));
}

}  // namespace lyra::lowering::hir_to_mir
