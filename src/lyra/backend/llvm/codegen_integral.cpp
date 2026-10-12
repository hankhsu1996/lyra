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
  // The planes of the value of type `shape` lying at `at`, each loaded as the
  // integer its bytes are and cut to the value's width.
  auto LoadPlanes(llvm::Value* at, const value::IntegralShape& shape) -> Planes;

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
  // Planes laid out at `at` as a value of type `shape`, each widened to its
  // storage unit, which is what keeps every position above the width clear.
  void StorePlanes(
      Planes planes, llvm::Value* at, const value::IntegralShape& shape);
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

auto CodeGenFunction::IntegralEmitter::LoadPlanes(
    llvm::Value* at, const value::IntegralShape& shape) -> Planes {
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
  return Planes{
      .value = value,
      .unknown = shape.IsFourState()
                     ? plane(builder_->CreateConstInBoundsGEP1_64(
                           builder_->getInt8Ty(), at, plane_bytes))
                     : llvm::Constant::getNullValue(exact)};
}

void CodeGenFunction::IntegralEmitter::StorePlanes(
    Planes planes, llvm::Value* at, const value::IntegralShape& shape) {
  if (planes.value->getType()->getIntegerBitWidth() != shape.width) {
    throw InternalError(
        std::format(
            "llvm codegen: an operation on planes answers {} bits where "
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
  if (const std::optional<PlaneAnswer> on_planes = LowerOnPlanes(
          *builder_, op, shapes,
          PlaneOperands{.planes = planes, .machine = machine})) {
    return std::visit(
        Overloaded{
            [&](const Planes& answer) {
              StorePlanes(answer, out, *shapes.answer);
              return out;
            },
            [](llvm::Value* answer) { return answer; }},
        *on_planes);
  }
  const ResolvedCall entry =
      ResolveEntryCall(op, operands, shapes, answer_type, out);
  llvm::Value* const answered =
      function_->EmitCallOrInvoke(entry.callee, entry.args);
  return shapes.answer.has_value() ? out : answered;
}

auto CodeGenFunction::IntegralEmitter::WithBitsWritten(
    Planes target, const value::IntegralShape& lying,
    std::span<const BitsPartRef> parts, std::size_t at, OperandRef bits)
    -> Planes {
  const BitsPartRef& part = parts[at];
  const value::IntegralShape start_shape =
      module_->Types().RequiredIntegralShapeOf(part.start.type);
  const Planes start = LoadPlanes(part.start.value, start_shape);
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
          return index < leading.size() ? leading[index]
                                        : further[index - leading.size()];
        };
        const std::optional<PlaneAnswer> answered = LowerOnPlanes(
            *builder_, op, shapes,
            PlaneOperands{.planes = planes, .machine = no_machine_operand});
        if (!answered.has_value()) {
          throw InternalError(
              "llvm codegen: a part of a value lying in place does not lie in "
              "place -- "
              "please report this as a bug");
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
    return with(LoadPlanes(bits.value, bits_shape), bits_shape);
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
  const auto lies_in_place = [&](lir::TypeId type) {
    return LiesInPlace(module_->Types().RequiredIntegralShapeOf(type));
  };
  if (!lies_in_place(whole) || !lies_in_place(bits.type) ||
      !std::ranges::all_of(parts, [&](const BitsPartRef& part) {
        return lies_in_place(part.type) && lies_in_place(part.start.type);
      })) {
    return false;
  }
  StorePlanes(
      WithBitsWritten(LoadPlanes(at, lying), lying, parts, 0, bits), at, lying);
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
  return IntegralEmitter(*this).LoadPlanes(position.value, *stated);
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
