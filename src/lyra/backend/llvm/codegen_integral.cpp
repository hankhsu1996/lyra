#include <algorithm>
#include <array>
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
#include "lyra/backend/llvm/integral_planes.hpp"
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
  auto StoreBitsOfPlace(
      llvm::Value* at, lir::TypeId whole, std::span<const BitsPartRef> parts,
      OperandRef bits) -> bool;

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
  // `target`, a value of `lying`, with `bits` written over the bits the parts
  // from `at` on name in it. A part between the value and the bits is taken
  // out as its bits lie -- at its own width and holding what `lying` holds, so
  // nothing is converted on the way -- changed, and put back.
  auto WithBitsWritten(
      Planes target, const value::IntegralShape& lying,
      std::span<const BitsPartRef> parts, std::size_t at, OperandRef bits)
      -> Planes;
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

auto CodeGenFunction::IntegralEmitter::EntryOf(IntegralOp op) -> llvm::Value* {
  llvm::Type* const table_type = llvm::ArrayType::get(
      module_->Types().Ptr(), support::IntegralOperations().size());
  llvm::Constant* const table = module_->Module().getOrInsertGlobal(
      runtime::kIntegralEntriesSymbol, table_type);
  llvm::cast<llvm::GlobalVariable>(table)->setConstant(true);
  llvm::PointerType* const entry = module_->Types().Ptr();
  llvm::Value* const slot = builder_->CreateConstInBoundsGEP2_64(
      table_type, table, 0, std::to_underlying(op));
  return builder_->CreateLoad(
      entry, slot, std::format("integral.{}", IntegralOperationOf(op).name));
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
      .args = std::move(args),
      .return_slot = shapes.answer.has_value() ? out : nullptr};
}

auto CodeGenFunction::IntegralEmitter::Lower(
    IntegralOp op, std::span<const OperandRef> operands,
    lir::TypeId answer_type, llvm::Value* out) -> llvm::Value* {
  const IntegralShapes shapes = ShapesOf(op, operands, answer_type);
  const auto planes = [&](std::size_t index) {
    return LoadPlanes(
        *builder_, operands[index].value, *shapes.operands.at(index));
  };
  const auto machine = [&](std::size_t index) { return operands[index].value; };
  if (const std::optional<PlaneAnswer> on_planes = LowerOnPlanes(
          *builder_, op, shapes,
          PlaneOperands{.planes = planes, .machine = machine})) {
    return std::visit(
        Overloaded{
            [&](const Planes& answer) {
              StorePlanes(*builder_, answer, out, *shapes.answer);
              return out;
            },
            [](llvm::Value* answer) { return answer; }},
        *on_planes);
  }
  const ResolvedCall entry =
      ResolveEntryCall(op, operands, shapes, answer_type, out);
  llvm::Value* const answered =
      function_->EmitCallOrInvoke(entry.callee, entry.args);
  return entry.return_slot != nullptr ? entry.return_slot : answered;
}

auto CodeGenFunction::IntegralEmitter::WithBitsWritten(
    Planes target, const value::IntegralShape& lying,
    std::span<const BitsPartRef> parts, std::size_t at, OperandRef bits)
    -> Planes {
  const BitsPartRef& part = parts[at];
  const value::IntegralShape start_shape =
      module_->Types().RequiredIntegralShapeOf(part.start.type);
  const Planes start = LoadPlanes(*builder_, part.start.value, start_shape);
  const auto no_machine_operand = [](std::size_t) -> llvm::Value* {
    throw InternalError(
        "llvm codegen: a part of an integral value is read or written over "
        "integral values alone -- please report this as a bug");
  };
  // `op` over `target`, where the part starts, and whatever else it takes,
  // answering a value of `answer`.
  const auto applied =
      [&](IntegralOp op, const value::IntegralShape& answer,
          std::span<const Planes> further,
          std::span<const value::IntegralShape> further_shapes) {
        IntegralShapes shapes{
            .operands = {lying, start_shape}, .answer = answer};
        shapes.operands.insert(
            shapes.operands.end(), further_shapes.begin(),
            further_shapes.end());
        const std::array<Planes, 2> leading{target, start};
        const auto planes = [&](std::size_t index) -> Planes {
          return index < leading.size() ? leading.at(index)
                                        : further[index - leading.size()];
        };
        const std::optional<PlaneAnswer> answered = LowerOnPlanes(
            *builder_, op, shapes,
            PlaneOperands{.planes = planes, .machine = no_machine_operand});
        if (!answered.has_value()) {
          throw InternalError(
              "llvm codegen: a part of a value stated on its planes is not "
              "stated on planes -- please report this as a bug");
        }
        return std::get<Planes>(*answered);
      };
  const auto with = [&](Planes written, const value::IntegralShape& shape) {
    return applied(
        IntegralOp::kWithSlice, lying, std::span(&written, 1),
        std::span(&shape, 1));
  };
  if (at + 1 == parts.size()) {
    const value::IntegralShape bits_shape =
        module_->Types().RequiredIntegralShapeOf(bits.type);
    return with(LoadPlanes(*builder_, bits.value, bits_shape), bits_shape);
  }
  const value::IntegralShape part_lying{
      .width = module_->Types().RequiredIntegralShapeOf(part.type).width,
      .signedness = value::Signedness::kUnsigned,
      .domain = lying.domain};
  const Planes taken = applied(IntegralOp::kSlice, part_lying, {}, {});
  return with(
      WithBitsWritten(taken, part_lying, parts, at + 1, bits), part_lying);
}

auto CodeGenFunction::IntegralEmitter::StoreBitsOfPlace(
    llvm::Value* at, lir::TypeId whole, std::span<const BitsPartRef> parts,
    OperandRef bits) -> bool {
  const value::IntegralShape lying =
      module_->Types().RequiredIntegralShapeOf(whole);
  // A part is reached and written back by operations on one word only, so the
  // whole write is the library's past one machine word.
  const auto on_planes = [&](lir::TypeId type) {
    return IsSingleWord(module_->Types().RequiredIntegralShapeOf(type));
  };
  if (!on_planes(whole) || !on_planes(bits.type) ||
      !std::ranges::all_of(parts, [&](const BitsPartRef& part) {
        return on_planes(part.type) && on_planes(part.start.type);
      })) {
    return false;
  }
  StorePlanes(
      *builder_,
      WithBitsWritten(LoadPlanes(*builder_, at, lying), lying, parts, 0, bits),
      at, lying);
  return true;
}

auto CodeGenFunction::LoadPosition(const OperandRef& position) -> Planes {
  const std::optional<value::IntegralShape> stated =
      module_->Types().IntegralShapeOf(position.type);
  if (!stated.has_value() || *stated != value::kShapeOf<value::Position>) {
    throw InternalError(
        "llvm codegen: an element is named by something that is not stated "
        "as a position -- please report this as a bug");
  }
  return LoadPlanes(builder_, position.value, *stated);
}

auto CodeGenFunction::StoreBitsOnPlanes(
    llvm::Value* at, lir::TypeId whole, std::span<const BitsPartRef> parts,
    OperandRef bits) -> bool {
  return IntegralEmitter(*this).StoreBitsOfPlace(at, whole, parts, bits);
}

auto CodeGenFunction::LowerIntegral(
    IntegralOp op, std::span<const OperandRef> operands,
    lir::TypeId answer_type, llvm::Value* out) -> llvm::Value* {
  return IntegralEmitter(*this).Lower(op, operands, answer_type, out);
}

}  // namespace lyra::backend::llvm_backend
