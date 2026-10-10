#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/integral_one_word.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/runtime/integral_abi.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::IntegralOp;
using support::IntegralOperandKind;
using support::IntegralOperation;
using support::IntegralOperationOf;

[[noreturn]] void Misapplied(
    const IntegralOperation& operation, std::string_view what) {
  throw InternalError(
      std::format(
          "llvm codegen: the integral operation {} {} -- please report this "
          "as a bug",
          operation.name, what));
}

}  // namespace

class CodeGenFunction::IntegralEmitter {
 public:
  explicit IntegralEmitter(CodeGenFunction& function)
      : function_(&function),
        module_(function.module_),
        builder_(&function.builder_) {
  }

  auto Lower(
      IntegralOp op, std::span<const OperandRef> operands,
      lir::TypeId answer_type, llvm::Value* out) -> llvm::Value*;

 private:
  // The types `op` applied to `operands` and answering at `answer_type` is
  // at, held to what the operation declares: an operand it takes as an
  // integral value is one, operands it takes at one type are of one type, and
  // an answer whose type the operation fixes is stated at that type. This is
  // the one place an application's types are read, so performing the operation
  // here and handing it to the library start from the same answer.
  [[nodiscard]] auto ShapesOf(
      IntegralOp op, std::span<const OperandRef> operands,
      lir::TypeId answer_type) const -> IntegralShapes;
  // The planes of the one-word value of type `shape` lying at `at`, each
  // loaded at its storage unit and cut to the value's width; and such planes
  // laid out there, each widened to its storage unit, which is what keeps
  // every position above the width clear.
  auto LoadPlanes(llvm::Value* at, const value::IntegralShape& shape)
      -> OneWord;
  void StorePlanes(
      OneWord planes, llvm::Value* at, const value::IntegralShape& shape);
  // The library's entry carrying out `op`, read out of the table the library
  // publishes its entries in.
  auto EntryOf(IntegralOp op) -> llvm::Value*;
  // The call on that entry: the arguments its declaration gives it, each taken
  // from the operands, from `out`, or from the types.
  auto ResolveEntryCall(
      IntegralOp op, std::span<const OperandRef> operands,
      const IntegralShapes& shapes, lir::TypeId answer_type, llvm::Value* out)
      -> ResolvedCall;

  CodeGenFunction* function_;
  CodeGenModule* module_;
  llvm::IRBuilder<>* builder_;
};

auto CodeGenFunction::IntegralEmitter::ShapesOf(
    IntegralOp op, std::span<const OperandRef> operands,
    lir::TypeId answer_type) const -> IntegralShapes {
  const IntegralOperation& operation = IntegralOperationOf(op);
  const std::span<const IntegralOperandKind> kinds = operation.operands.Kinds();
  if (operands.size() != kinds.size()) {
    Misapplied(operation, "is handed a number of operands it does not take");
  }
  IntegralShapes shapes;
  std::vector<value::IntegralShape> integral;
  std::vector<value::IntegralExtent> extents;
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    std::optional<value::IntegralShape> shape;
    if (support::IsIntegralOperand(kinds[i])) {
      shape = module_->Types().IntegralShapeOf(operands[i].type);
      if (!shape.has_value()) {
        Misapplied(operation, "is handed no integral value where it takes one");
      }
      integral.push_back(*shape);
      extents.push_back(value::ExtentOf(*shape));
    }
    shapes.operands.push_back(shape);
  }
  value::RequireOperandTypes(op, integral);

  shapes.answer = value::AnswerShapeOf(
      op, extents, module_->Types().IntegralShapeOf(answer_type));
  return shapes;
}

auto CodeGenFunction::IntegralEmitter::LoadPlanes(
    llvm::Value* at, const value::IntegralShape& shape) -> OneWord {
  const std::size_t plane_bytes = value::PlaneBytesFor(shape.width);
  llvm::Type* const unit =
      builder_->getIntNTy(static_cast<unsigned>(8 * plane_bytes));
  const llvm::Align align(value::IntegralAlignFor(shape.width));
  llvm::Type* const exact =
      builder_->getIntNTy(static_cast<unsigned>(shape.width));
  const auto plane = [&](llvm::Value* plane_at) {
    return builder_->CreateTrunc(
        builder_->CreateAlignedLoad(unit, plane_at, align), exact);
  };
  llvm::Value* const value = plane(at);
  return OneWord{
      .value = value,
      .unknown = shape.IsFourState()
                     ? plane(builder_->CreateConstInBoundsGEP1_64(
                           builder_->getInt8Ty(), at, plane_bytes))
                     : llvm::Constant::getNullValue(exact)};
}

void CodeGenFunction::IntegralEmitter::StorePlanes(
    OneWord planes, llvm::Value* at, const value::IntegralShape& shape) {
  if (planes.value->getType()->getIntegerBitWidth() != shape.width) {
    throw InternalError(
        std::format(
            "llvm codegen: an operation over one word answers {} bits where "
            "its answer's type has {} -- please report this as a bug",
            planes.value->getType()->getIntegerBitWidth(), shape.width));
  }
  const std::size_t plane_bytes = value::PlaneBytesFor(shape.width);
  llvm::Type* const unit =
      builder_->getIntNTy(static_cast<unsigned>(8 * plane_bytes));
  const llvm::Align align(value::IntegralAlignFor(shape.width));
  const auto plane = [&](llvm::Value* bits, llvm::Value* plane_at) {
    builder_->CreateAlignedStore(
        builder_->CreateZExt(bits, unit), plane_at, align);
  };
  plane(planes.value, at);
  if (shape.IsFourState()) {
    plane(
        planes.unknown, builder_->CreateConstInBoundsGEP1_64(
                            builder_->getInt8Ty(), at, plane_bytes));
  }
}

auto CodeGenFunction::IntegralEmitter::EntryOf(IntegralOp op) -> llvm::Value* {
  llvm::Type* const table_type = llvm::ArrayType::get(
      module_->Types().Ptr(), support::IntegralOperations().size());
  llvm::Constant* const table = module_->Module().getOrInsertGlobal(
      runtime::kIntegralEntriesSymbol, table_type);
  llvm::cast<llvm::GlobalVariable>(table)->setConstant(true);
  return builder_->CreateLoad(
      module_->Types().Ptr(),
      builder_->CreateConstInBoundsGEP2_64(
          table_type, table, 0, std::to_underlying(op)),
      std::format("integral.{}", IntegralOperationOf(op).name));
}

auto CodeGenFunction::IntegralEmitter::ResolveEntryCall(
    IntegralOp op, std::span<const OperandRef> operands,
    const IntegralShapes& shapes, lir::TypeId answer_type, llvm::Value* out)
    -> ResolvedCall {
  const auto fact = [&](const value::IntegralShape& shape,
                        runtime::IntegralTypeFact which) -> llvm::Value* {
    switch (which) {
      case runtime::IntegralTypeFact::kWidth:
        return llvm::ConstantInt::get(builder_->getInt64Ty(), shape.width);
      case runtime::IntegralTypeFact::kIsSigned:
        return builder_->getInt1(
            shape.signedness == value::Signedness::kSigned);
      case runtime::IntegralTypeFact::kIsFourState:
        return builder_->getInt1(shape.IsFourState());
    }
    throw InternalError("llvm codegen: unknown integral type fact");
  };
  const runtime::IntegralEntryArguments declared =
      runtime::IntegralEntryArgumentsOf(op);
  // The machine type each argument crosses as is the entry's own statement,
  // which the library's side of the same entry is compiled from.
  const auto crossing =
      [&](const runtime::IntegralEntryArgument& argument) -> llvm::Type* {
    switch (runtime::MachineTypeOf(op, argument)) {
      case runtime::IntegralEntryMachineType::kAddress:
      case runtime::IntegralEntryMachineType::kStorage:
        return builder_->getPtrTy();
      case runtime::IntegralEntryMachineType::kInt64:
        return builder_->getInt64Ty();
      case runtime::IntegralEntryMachineType::kBool:
        return builder_->getInt1Ty();
      case runtime::IntegralEntryMachineType::kByte:
        return builder_->getInt8Ty();
    }
    throw InternalError("llvm codegen: unknown integral entry machine type");
  };
  std::vector<llvm::Value*> args;
  std::vector<llvm::Type*> parameters;
  args.reserve(declared.All().size());
  parameters.reserve(declared.All().size());
  for (const runtime::IntegralEntryArgument& argument : declared.All()) {
    llvm::Value* const handed = std::visit(
        Overloaded{
            [&](const runtime::OperandArgument& operand) {
              return operands[operand.operand].value;
            },
            [&](const runtime::AnswerStorageArgument&) { return out; },
            [&](const runtime::OperandTypeArgument& told) {
              return fact(*shapes.operands.at(told.operand), told.fact);
            },
            [&](const runtime::AnswerTypeArgument& told) {
              return fact(*shapes.answer, told.fact);
            }},
        argument);
    llvm::Type* const crosses_as = crossing(argument);
    if (handed->getType() != crosses_as) {
      Misapplied(
          IntegralOperationOf(op),
          "is handed an argument of another machine type than its entry takes");
    }
    args.push_back(handed);
    parameters.push_back(crosses_as);
  }
  // An answer laid out in storage the caller gives is all the entry answers
  // with; any other is a machine value of the type the call states.
  llvm::Type* const answered = shapes.answer.has_value()
                                   ? module_->Types().Void()
                                   : module_->Types().Map(answer_type);
  return ResolvedCall{
      .callee = llvm::FunctionCallee(
          llvm::FunctionType::get(answered, parameters, false), EntryOf(op)),
      .args = std::move(args)};
}

auto CodeGenFunction::IntegralEmitter::Lower(
    IntegralOp op, std::span<const OperandRef> operands,
    lir::TypeId answer_type, llvm::Value* out) -> llvm::Value* {
  const IntegralShapes shapes = ShapesOf(op, operands, answer_type);
  const auto planes = [&](std::size_t index) {
    return LoadPlanes(operands[index].value, *shapes.operands.at(index));
  };
  const auto machine = [&](std::size_t index) { return operands[index].value; };
  if (const std::optional<OneWordAnswer> on_one_word = LowerOnOneWord(
          *builder_, op, shapes,
          OneWordOperands{.planes = planes, .machine = machine})) {
    return std::visit(
        Overloaded{
            [&](const OneWord& answer) {
              StorePlanes(answer, out, *shapes.answer);
              return out;
            },
            [](llvm::Value* answer) { return answer; }},
        *on_one_word);
  }
  const ResolvedCall entry =
      ResolveEntryCall(op, operands, shapes, answer_type, out);
  llvm::Value* const answered =
      function_->EmitCallOrInvoke(entry.callee, entry.args);
  return shapes.answer.has_value() ? out : answered;
}

auto CodeGenFunction::LowerIntegral(
    IntegralOp op, std::span<const OperandRef> operands,
    lir::TypeId answer_type, llvm::Value* out) -> llvm::Value* {
  return IntegralEmitter(*this).Lower(op, operands, answer_type, out);
}

}  // namespace lyra::backend::llvm_backend
