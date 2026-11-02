#include "lyra/backend/llvm/llvm_fn_abi.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/target_lowering.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The machine type an operand this target states on its own account crosses
// at.
auto MachineTypeOf(CodeGenTypes& types, const StatedOperand& operand)
    -> llvm::Type* {
  return std::visit(
      Overloaded{
          [&](const ValueOperand& value) -> llvm::Type* {
            return types.Map(value.type);
          },
          [&](const MachineOperand& machine) -> llvm::Type* {
            switch (machine.kind) {
              case MachineOperandKind::kAddress:
                return types.Ptr();
              case MachineOperandKind::kCount:
                return llvm::Type::getInt64Ty(types.Ptr()->getContext());
              case MachineOperandKind::kSpan:
                return types.Span();
            }
            throw InternalError("llvm codegen: unknown machine operand");
          }},
      operand);
}

}  // namespace

auto AtMachineTypes(CodeGenModule& module, FnAbi abi, ReturnInfo ret)
    -> LlvmFnAbi {
  std::vector<LlvmArgAbi> args;
  args.reserve(abi.args.size());
  for (const ArgAbi& arg : abi.args) {
    args.push_back(
        LlvmArgAbi{
            .type = MachineTypeOf(module.Types(), arg.operand),
            .mode = arg.mode});
  }
  return LlvmFnAbi{
      .implicit_arg = abi.implicit_arg,
      .named_part = abi.named_part,
      .args = std::move(args),
      .type_args = std::move(abi.type_args),
      .ret = ret};
}

CallArranger::CallArranger(
    CodeGenModule& module, const TargetLowering& lowering)
    : module_(&module), lowering_(&lowering) {
}

auto CallArranger::Direct(llvm::Type* type) -> LlvmArgAbi {
  return LlvmArgAbi{.type = type, .mode = PassDirect{}};
}

auto CallArranger::ReturnOfValue(CodeGenTypes& types, lir::TypeId type)
    -> ReturnInfo {
  if (types.HasAggregateEvaluationKind(type)) {
    return ReturnIndirect{.returned = types.Ptr()};
  }
  return ReturnDirect{.type = types.Map(type)};
}

auto CallArranger::Arrange(std::vector<LlvmArgAbi> args, ReturnInfo ret)
    -> LlvmFnAbi {
  return LlvmFnAbi{
      .implicit_arg = NoImplicitArg{},
      .named_part = std::nullopt,
      .args = std::move(args),
      .type_args = {},
      .ret = ret};
}

auto CallArranger::ReturnOf(
    const lir::CallInstr& call, lir::TypeId result_type) const -> ReturnInfo {
  // What builds its answer where its caller says answers with where that is.
  if (lowering_->AnswersInStorage(call, result_type)) {
    return ReturnIndirect{.returned = module_->Types().Ptr()};
  }
  return ReturnDirect{.type = module_->Types().Map(result_type)};
}

auto CallArranger::LlvmFnAbiOf(
    const lir::CallInstr& call, lir::TypeId result_type) const
    -> diag::Result<LlvmFnAbi> {
  auto abi = lowering_->FnAbiOf(call, result_type);
  if (!abi) {
    return std::unexpected(std::move(abi.error()));
  }
  return AtMachineTypes(*module_, *std::move(abi), ReturnOf(call, result_type));
}

}  // namespace lyra::backend::llvm_backend
