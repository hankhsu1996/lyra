#include "lyra/backend/llvm/codegen_types.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/LLVMContext.h>

#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/runtime_tuple.hpp"

namespace lyra::backend::llvm_backend {

CodeGenTypes::CodeGenTypes(
    llvm::LLVMContext& ctx, const lir::CompilationUnit& unit)
    : ctx_(&ctx),
      unit_(&unit),
      ptr_ty_(llvm::PointerType::getUnqual(ctx)),
      span_ty_(
          llvm::StructType::get(ctx, {ptr_ty_, llvm::Type::getInt64Ty(ctx)})) {
}

auto CodeGenTypes::Void() const -> llvm::Type* {
  return llvm::Type::getVoidTy(*ctx_);
}

auto CodeGenTypes::Map(lir::TypeId id) -> llvm::Type* {
  if (auto it = cache_.find(id); it != cache_.end()) {
    return it->second;
  }
  // An address, which is the whole of what a machine register holds for such a
  // type. The arms below are grouped by why a type answers this way: a value
  // held where its bytes lie, whether this backend lays them out or the
  // runtime realizes the value as an object of its own; storage whose every
  // operation consumes where it lives; and a value that already is an address.
  const auto address = [&](const auto&) -> llvm::Type* { return ptr_ty_; };
  llvm::Type* mapped = unit_->types.Get(id).Visit(
      Overloaded{
          // The machine types, which name themselves: what a target's own type
          // system spells is what the target computes with.
          [&](const lir::VoidType&) -> llvm::Type* { return Void(); },
          [&](const lir::MachineBoolType&) -> llvm::Type* {
            return llvm::Type::getInt1Ty(*ctx_);
          },
          [&](const lir::MachineIntType& m) -> llvm::Type* {
            return llvm::IntegerType::get(*ctx_, lir::BitsOf(m.width));
          },
          [&](const lir::MachineFloatType& m) -> llvm::Type* {
            switch (m.width) {
              case lir::MachineFloatWidth::k32:
                return llvm::Type::getFloatTy(*ctx_);
              case lir::MachineFloatWidth::k64:
                return llvm::Type::getDoubleTy(*ctx_);
            }
            throw InternalError("llvm codegen: unknown MachineFloatWidth");
          },
          // A sequence of machine values is held as a view over where they
          // lie.
          [&](const lir::MachineArrayType&) -> llvm::Type* { return span_ty_; },
          [&](const lir::MachineCStringType& t) { return address(t); },
          // The machine family's callable axis. The signature it carries is
          // what a call made through it is compiled against, not part of the
          // address itself, which is why the address is all this maps to.
          [&](const lir::MachineFunctionType& t) { return address(t); },

          // An integral value is the bytes its type lays it out in, built in
          // storage this backend sizes from the type and held where they lie.
          [&](const lir::IntegralType& t) { return address(t); },
          // A simulation value the runtime realizes as an object it owns. Every
          // operation on one is a library call handed where the object lives,
          // so generated code never holds the object's shape and would have
          // nothing to do with it if it did.
          [&](const lir::UnpackedArrayType& t) { return address(t); },
          [&](const lir::DynamicArrayType& t) { return address(t); },
          [&](const lir::QueueType& t) { return address(t); },
          [&](const lir::AssociativeArrayType& t) { return address(t); },
          [&](const lir::StringType& t) { return address(t); },
          [&](const lir::RealType& t) { return address(t); },
          [&](const lir::ShortRealType& t) { return address(t); },
          [&](const lir::UnionType& t) { return address(t); },
          [&](const lir::TaggedUnionType& t) { return address(t); },
          [&](const lir::EmptyType& t) { return address(t); },
          // A product, which this backend lays out itself from its
          // components; it is held where it lies, like every value above.
          [&](const lir::TupleType& t) { return address(t); },
          [&](const lir::StructType& t) { return address(t); },
          [&](const lir::EventType& t) { return address(t); },
          // A wildcard index (LRM 7.8.1) names where an index of such an array
          // goes. What stands there is an object of the library's keeping the
          // index with the type it was written in, held where it lies.
          [&](const lir::WildcardIndexType& t) { return address(t); },

          // Storage, whose operations consume where it lives rather than what
          // it holds: a node of the object tree, a closure's captures, and the
          // cells a declaration installs over a value.
          [&](const lir::ObjectType& t) { return address(t); },
          [&](const lir::CrossUnitClassType& t) { return address(t); },
          [&](const lir::RuntimeClassType& t) { return address(t); },
          [&](const lir::ClosureType& t) { return address(t); },
          [&](const lir::ObservableType& t) { return address(t); },
          [&](const lir::ResolvedType& t) { return address(t); },
          [&](const lir::SampledHistoryType& t) { return address(t); },
          [&](const lir::EvaluationAttemptsType& t) { return address(t); },
          [&](const lir::RuntimeEffectsType& t) { return address(t); },
          [&](const lir::FilesType& t) { return address(t); },
          [&](const lir::DiagnosticType& t) { return address(t); },
          [&](const lir::RuntimeLibraryType& t) { return address(t); },
          [&](const lir::OpenWriteType& t) { return address(t); },
          [&](const lir::DesignationType& t) { return address(t); },
          [&](const lir::RefType& t) { return address(t); },

          // A value that already is an address: a handle onto storage of some
          // other lifetime, an execution's own frame, or -- for a chandle (LRM
          // 6.14) and a class handle (LRM 8.3) -- a value whose whole content
          // is what it refers to.
          [&](const lir::PointerType& t) { return address(t); },
          [&](const lir::ManagedRefType& t) { return address(t); },
          [&](const lir::ChandleType& t) { return address(t); },
          [&](const lir::DriverType& t) { return address(t); },
          [&](const lir::VectorType& t) { return address(t); },
          [&](const lir::CoroutineType& t) { return address(t); },
      });
  cache_.emplace(id, mapped);
  return mapped;
}

auto CodeGenTypes::GetFunctionType(const FnAbi& abi) -> llvm::FunctionType* {
  // What an entry is told of a type crosses as machine values of its own: a
  // width as a count, a signedness and a state domain as a flag each, and the
  // constant of a type as its address.
  llvm::Type* const count = llvm::Type::getInt64Ty(*ctx_);
  llvm::Type* const flag = llvm::Type::getInt1Ty(*ctx_);
  std::vector<llvm::Type*> params;
  std::visit(
      Overloaded{
          [](const NoImplicitArg&) {},
          [&](const CompleteObjectSizeArg&) { params.push_back(count); },
          [&](const HeldWidthArg&) { params.push_back(count); }},
      abi.implicit_arg);
  for (std::size_t i = 0; i <= abi.args.size(); ++i) {
    if (abi.named_part.has_value() && abi.named_part->before == i) {
      params.push_back(count);
    }
    if (i == abi.args.size()) {
      break;
    }
    params.push_back(abi.args[i].type);
    std::visit(
        Overloaded{
            [](const PassDirect&) {},
            [&](const PassWithExtent&) {
              params.push_back(count);
              params.push_back(flag);
            },
            [&](const PassWithShape&) {
              params.push_back(count);
              params.push_back(flag);
              params.push_back(flag);
            },
            [&](const PassWithType&) { params.push_back(ptr_ty_); },
            [&](const PassWithWidth&) { params.push_back(count); }},
        abi.args[i].mode);
  }
  for (const TypeArg& told : abi.type_args) {
    std::visit(
        Overloaded{
            [&](const TypeConstantArg&) { params.push_back(ptr_ty_); },
            [&](const TypeExtentArg&) {
              params.push_back(count);
              params.push_back(flag);
            },
            [&](const TypeWidthArg&) { params.push_back(count); }},
        told);
  }
  return std::visit(
      Overloaded{
          [&](const ReturnDirect& direct) {
            return llvm::FunctionType::get(direct.type, params, false);
          },
          [&](const ReturnIndirect& indirect) {
            params.push_back(ptr_ty_);
            return llvm::FunctionType::get(indirect.returned, params, false);
          }},
      abi.ret);
}

auto CodeGenTypes::RequiredIntegralShapeOf(lir::TypeId type) const
    -> value::IntegralShape {
  const std::optional<value::IntegralShape> shape = IntegralShapeOf(type);
  if (!shape.has_value()) {
    throw InternalError(
        "llvm codegen: an entry is told the width, the signedness or the "
        "states of a type that is not integral -- please report this as a "
        "bug");
  }
  return *shape;
}

auto CodeGenTypes::IntegralShapeOf(lir::TypeId type) const
    -> std::optional<value::IntegralShape> {
  const auto* integral = unit_->types.Get(type).As<lir::IntegralType>();
  if (integral == nullptr) {
    return std::nullopt;
  }
  value::IntegralShape shape{.width = integral->bit_width};
  switch (integral->signedness) {
    case lir::Signedness::kSigned:
      shape.signedness = value::Signedness::kSigned;
      break;
    case lir::Signedness::kUnsigned:
      shape.signedness = value::Signedness::kUnsigned;
      break;
  }
  switch (integral->state_kind) {
    case lir::IntegralStateKind::kTwoState:
      shape.domain = value::StateDomain::kTwoState;
      break;
    case lir::IntegralStateKind::kFourState:
      shape.domain = value::StateDomain::kFourState;
      break;
  }
  return shape;
}

auto CodeGenTypes::StorageOf(lir::TypeId type) -> support::ObjectLayout {
  const lir::Type& ty = unit_->types.Get(type);
  if (ty.IsProduct()) {
    return LayoutOfTuple(type).storage;
  }
  if (const std::optional<value::IntegralShape> shape = IntegralShapeOf(type)) {
    return support::ObjectLayout{
        .size = static_cast<std::uint32_t>(shape->Bytes()),
        .align =
            static_cast<std::uint32_t>(value::IntegralAlignFor(shape->width)),
        .ends_with_nothing_to_do = true};
  }
  if (const auto* machine = ty.As<lir::MachineIntType>()) {
    const std::uint32_t bytes = lir::BitsOf(machine->width) / 8U;
    return support::ObjectLayout{
        .size = bytes, .align = bytes, .ends_with_nothing_to_do = true};
  }
  const std::optional<support::RuntimeObject> object = HeldObjectOf(ty);
  if (!object.has_value()) {
    throw InternalError(
        "llvm codegen: storage asked of a type whose values are not owned -- "
        "please report this as a bug");
  }
  return runtime::LayoutOf(*object);
}

auto CodeGenTypes::LayoutOfTuple(lir::TypeId tuple) -> const TupleLayout& {
  if (const auto found = tuples_.find(tuple); found != tuples_.end()) {
    return found->second;
  }
  const auto aligned_up = [](std::uint32_t size, std::uint32_t align) {
    return (size + align - 1) / align * align;
  };
  const std::optional<std::span<const lir::TypeId>> components =
      lir::ProductElements(*unit_, tuple);
  if (!components.has_value()) {
    throw InternalError(
        "llvm codegen: a tuple layout asked of a type that is no product");
  }
  TupleLayout layout{
      .components = {components->begin(), components->end()},
      .offsets = {},
      .storage = {
          .size = sizeof(const value::TupleType*),
          .align = alignof(const value::TupleType*),
          .ends_with_nothing_to_do = true}};
  layout.offsets.reserve(layout.components.size());
  for (const lir::TypeId component : layout.components) {
    const support::ObjectLayout held = StorageOf(component);
    const std::uint32_t offset = aligned_up(layout.storage.size, held.align);
    layout.offsets.push_back(offset);
    layout.storage.size = offset + held.size;
    layout.storage.align = std::max(layout.storage.align, held.align);
    layout.storage.ends_with_nothing_to_do =
        layout.storage.ends_with_nothing_to_do && held.ends_with_nothing_to_do;
  }
  layout.storage.size = aligned_up(layout.storage.size, layout.storage.align);
  return tuples_.emplace(tuple, std::move(layout)).first->second;
}

}  // namespace lyra::backend::llvm_backend
