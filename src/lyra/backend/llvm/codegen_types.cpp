#include "lyra/backend/llvm/codegen_types.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <ranges>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/LLVMContext.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/llvm_fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/scalar.hpp"

namespace lyra::backend::llvm_backend {

CodeGenTypes::CodeGenTypes(CodeGenModule& module)
    : ctx_(&module.Context()),
      unit_(&module.Unit()),
      ptr_ty_(llvm::PointerType::getUnqual(*ctx_)),
      span_ty_(
          llvm::StructType::get(
              *ctx_, {ptr_ty_, llvm::Type::getInt64Ty(*ctx_)})),
      bits_part_ty_(
          llvm::StructType::get(
              *ctx_, {ptr_ty_, llvm::Type::getInt64Ty(*ctx_)})),
      module_(&module) {
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
              case support::FloatWidth::k32:
                return llvm::Type::getFloatTy(*ctx_);
              case support::FloatWidth::k64:
                return llvm::Type::getDoubleTy(*ctx_);
            }
            throw InternalError("llvm codegen: unknown FloatWidth");
          },
          // A sequence of machine values is held as a view over where they
          // lie.
          [&](const lir::MachineArrayType&) -> llvm::Type* { return span_ty_; },
          [&](const lir::MachineSpanType&) -> llvm::Type* { return span_ty_; },
          [&](const lir::MachineCStringType& t) { return address(t); },
          // The machine family's callable axis. The signature it carries is
          // what a call made through it is compiled against, not part of the
          // address itself, which is why the address is all this maps to.
          [&](const lir::MachineFunctionType& t) { return address(t); },

          // An integral value is the bytes its type lays it out in, built in
          // storage this backend sizes from the type and held where they lie.
          [&](const lir::IntegralType& t) { return address(t); },
          // A real is the IEEE value of its width, laid out here as that
          // float and held where it lies, as an integral value is.
          [&](const lir::RealType& t) { return address(t); },
          // A fixed-size unpacked array is its elements one after another,
          // laid out here from the type and held where they lie.
          [&](const lir::UnpackedArrayType& t) { return address(t); },
          // A reference to a value of any type, which the runtime realizes as
          // an object it owns. Every operation on one is a library call handed
          // where the object lives, so generated code never holds the object's
          // shape and would have nothing to do with it if it did.
          [&](const lir::AnyReferenceType& t) { return address(t); },
          // A reference owning nothing is held where the address it is lies,
          // as every value of the design is, which is what lets a variable's
          // cell and a container hold one as they hold any other.
          [&](const lir::UnmanagedRefType& t) { return address(t); },
          // A tuple and a record, which this backend lays out itself from
          // their components and members; each is held where it lies, like
          // every value above.
          [&](const lir::TupleType& t) { return address(t); },
          [&](const lir::RecordType& t) { return address(t); },
          // A type the library declares, whose shape is the library's: a
          // value or an object of its own held where it lies, or storage and
          // facilities it keeps, consumed where they live.
          [&](const lir::LibraryType& t) { return address(t); },

          // Storage, whose operations consume where it lives rather than what
          // it holds: a node of the object tree, a closure's captures, and the
          // cells a declaration installs over a value.
          [&](const lir::ObjectType& t) { return address(t); },
          [&](const lir::ClosureType& t) { return address(t); },
          [&](const lir::ErasedCallableType& t) { return address(t); },
          [&](const lir::ObservableType& t) { return address(t); },
          [&](const lir::ResolvedType& t) { return address(t); },
          [&](const lir::OpenWriteType& t) { return address(t); },
          [&](const lir::DesignationType& t) { return address(t); },
          [&](const lir::ObjectWriteType& t) { return address(t); },
          [&](const lir::VariableRefType& t) { return address(t); },

          // A value that already is an address: a handle onto storage of some
          // other lifetime, an execution's own frame, or -- for a class handle
          // (LRM 8.3) -- a value whose whole content is what it refers to.
          [&](const lir::PointerType& t) { return address(t); },
          [&](const lir::ManagedRefType& t) { return address(t); },
          [&](const lir::DriverType& t) { return address(t); },
          [&](const lir::VectorType& t) { return address(t); },
          [&](const lir::CoroutineType& t) { return address(t); },
      });
  cache_.emplace(id, mapped);
  return mapped;
}

auto CodeGenTypes::GetFunctionType(const LlvmFnAbi& abi)
    -> llvm::FunctionType* {
  // What an entry is told of a type crosses as machine values of its own: a
  // width as a count, a signedness and a state domain as a flag each, and the
  // constant of a type, of its operations or of how a net folds values of it,
  // and the function taking some of a value's positions, as its address.
  llvm::Type* const count = llvm::Type::getInt64Ty(*ctx_);
  llvm::Type* const flag = llvm::Type::getInt1Ty(*ctx_);
  std::vector<llvm::Type*> params;
  std::visit(
      Overloaded{
          [](const NoImplicitArg&) {},
          [&](const WidthMetadataArg&) { params.push_back(count); },
          [&](const TypeMetadataArg&) { params.push_back(ptr_ty_); },
          [&](const TypeAndNetFoldsArg&) {
            params.push_back(ptr_ty_);
            params.push_back(ptr_ty_);
          }},
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
            [&](const ElementTypeArg&) { params.push_back(ptr_ty_); },
            [&](const QueueTypeArg&) { params.push_back(ptr_ty_); },
            [&](const TakePositionsArg&) { params.push_back(ptr_ty_); },
            [&](const IndexOrderArg&) { params.push_back(ptr_ty_); },
            [&](const OperationsArg&) { params.push_back(ptr_ty_); }},
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
        "llvm codegen: the width, the signedness or the states of a type "
        "that is not integral are asked for -- please report this as a bug");
  }
  return *shape;
}

auto CodeGenTypes::IntegralShapeOf(lir::TypeId type) const
    -> std::optional<value::IntegralShape> {
  const auto* integral = unit_->types.Get(type).As<lir::IntegralType>();
  if (integral == nullptr) {
    return std::nullopt;
  }
  return ShapeOf(*integral);
}

auto CodeGenTypes::HasAggregateEvaluationKind(lir::TypeId type) const -> bool {
  return unit_->types.Get(type).IsHeldInStorage();
}

auto CodeGenTypes::IsReadAsView(lir::TypeId type) const -> bool {
  return unit_->types.Get(type).Is<lir::MachineArrayType>();
}

auto CodeGenTypes::MachineValueLayout(lir::TypeId type)
    -> support::ObjectLayout {
  llvm::Type* const mapped = Map(type);
  if (mapped->isPointerTy()) {
    return support::ObjectLayout{
        .layout = {.size = sizeof(void*), .align = alignof(void*)},
        .needs_end = false};
  }
  if (!mapped->isIntegerTy() && !mapped->isFloatingPointTy()) {
    throw InternalError(
        "llvm codegen: an element of a sequence of machine values is an "
        "address, an integer or a floating-point number -- please report this "
        "as a bug");
  }
  const std::size_t bytes =
      (mapped->getPrimitiveSizeInBits().getFixedValue() + 7U) / 8U;
  return support::ObjectLayout{
      .layout = {.size = bytes, .align = bytes}, .needs_end = false};
}

auto CodeGenTypes::LayoutOf(lir::TypeId type) -> support::ObjectLayout {
  const std::optional<lir::Holding> held = unit_->types.Get(type).HeldAs();
  if (!held.has_value()) {
    return MachineDataLayout(type);
  }
  return std::visit(
      Overloaded{
          [](const value::Scalar& scalar) -> support::ObjectLayout {
            return {
                .layout = value::FactsOf(scalar).layout, .needs_end = false};
          },
          // A pointer owning what it points at is that address.
          [&](const lir::OwnedStorageAddress&) -> support::ObjectLayout {
            return {
                .layout = {.size = sizeof(void*), .align = alignof(void*)},
                .needs_end = lir::NeedsEnd(*unit_, type)};
          },
          // A closure is as large as its captures are placed, which is the
          // module's to say.
          [&](const lir::ClosureStorage&) -> support::ObjectLayout {
            return {
                .layout = module_->ClosureLayout(type),
                .needs_end = lir::NeedsEnd(*unit_, type)};
          },
          // A value laid out by its type is as large as its parts are placed.
          [&](const lir::LaidOutByType&) -> support::ObjectLayout {
            return std::visit(
                Overloaded{
                    [](const ProductLayout* product) {
                      return product->storage;
                    },
                    [](const ArrayLayout* array) { return array->storage; },
                    [](const UnionLayout* overlay) {
                      return overlay->storage;
                    }},
                FieldsOf(type));
          },
          // An object of the library's takes the storage stated for it.
          [](const support::RuntimeObject& object) -> support::ObjectLayout {
            return HolderStorageOf(object);
          }},
      *held);
}

auto CodeGenTypes::FieldsOf(lir::TypeId laid_out) -> FieldsShape {
  const lir::Type& stated = unit_->types.Get(laid_out);
  if (stated.Is<lir::UnpackedArrayType>()) {
    return &LayoutOfArray(laid_out);
  }
  if (stated.Is<lir::TupleType>()) {
    return &LayoutOfProduct(laid_out);
  }
  const lir::RecordId* record = lir::RecordOf(stated);
  if (record == nullptr) {
    throw InternalError(
        "llvm codegen: how parts are placed asked of a type not laid out by "
        "its type -- please report this as a bug");
  }
  switch (lir::RecordKindOf(*unit_, *record)) {
    case lir::RecordKind::kStruct:
      return &LayoutOfProduct(laid_out);
    case lir::RecordKind::kUnion:
    case lir::RecordKind::kTaggedUnion:
      return &LayoutOfUnion(laid_out);
  }
  throw InternalError("llvm codegen: unknown record kind");
}

auto CodeGenTypes::MachineDataLayout(lir::TypeId type)
    -> support::ObjectLayout {
  const lir::Type& ty = unit_->types.Get(type);
  if (const auto* machine = ty.As<lir::MachineIntType>()) {
    const std::size_t bytes = lir::BitsOf(machine->width) / 8U;
    return support::ObjectLayout{
        .layout = {.size = bytes, .align = bytes}, .needs_end = false};
  }
  // A sequence of machine values is the values one after another, and ends
  // none of them.
  if (const auto* sequence = ty.As<lir::MachineArrayType>()) {
    const support::ObjectLayout element = MachineValueLayout(sequence->element);
    return support::ObjectLayout{
        .layout =
            {.size = element.layout.size * sequence->size,
             .align = element.layout.align},
        .needs_end = false};
  }
  throw InternalError(
      "llvm codegen: storage asked of a type whose values are not owned -- "
      "please report this as a bug");
}

// The library walks every whole array by the layout it states, so that
// statement is the one read here. An array ends as its elements do.
auto CodeGenTypes::LayoutOfArray(lir::TypeId array) -> const ArrayLayout& {
  if (const auto found = arrays_.find(array); found != arrays_.end()) {
    return found->second;
  }
  const auto* stated = unit_->types.Get(array).As<lir::UnpackedArrayType>();
  if (stated == nullptr) {
    throw InternalError(
        "llvm codegen: an array layout asked of a type that is no fixed-size "
        "unpacked array");
  }
  const lir::TypeId element_type = stated->element_type;
  const std::uint64_t count = stated->size;
  const support::ObjectLayout element = LayoutOf(element_type);
  const value::SequenceLayout laid =
      value::SequenceLayoutOf(element.layout, count);
  return arrays_
      .emplace(
          array,
          ArrayLayout{
              .element = element_type,
              .count = count,
              .stride = laid.stride,
              .storage =
                  {.layout = laid.whole,
                   .needs_end = lir::NeedsEnd(*unit_, array)}})
      .first->second;
}

auto CodeGenTypes::LayoutOfUnion(lir::TypeId union_type) -> const UnionLayout& {
  if (const auto found = unions_.find(union_type); found != unions_.end()) {
    return found->second;
  }
  const std::optional<std::span<const lir::TypeId>> members =
      lir::OverlaidMembers(*unit_, union_type);
  if (!members.has_value()) {
    throw InternalError(
        "llvm codegen: a union layout asked of a type that is no union");
  }
  const support::Layout overlay = OverlayOf(*members);
  std::optional<value::TagPlacement> tag;
  support::Layout whole = overlay;
  if (lir::IsTaggedUnion(*unit_, union_type)) {
    const value::TaggedUnionLayout tagged =
        value::TaggedUnionLayoutOf(overlay, members->size());
    tag = tagged.tag;
    whole = tagged.whole;
  }
  return unions_
      .emplace(
          union_type,
          UnionLayout{
              .members = {members->begin(), members->end()},
              .tag = tag,
              .storage =
                  {.layout = whole,
                   .needs_end = lir::NeedsEnd(*unit_, union_type)}})
      .first->second;
}

auto CodeGenTypes::TagOf(lir::TypeId tagged) -> value::TagPlacement {
  const std::optional<value::TagPlacement> tag = LayoutOfUnion(tagged).tag;
  if (!tag.has_value()) {
    throw InternalError(
        "llvm codegen: a tag's placement asked of a type that is no tagged "
        "union");
  }
  return *tag;
}

auto CodeGenTypes::OverlayOf(std::span<const lir::TypeId> members)
    -> support::Layout {
  return value::OverlayLayoutOf(
      members | std::views::transform([&](lir::TypeId member) {
        return LayoutOf(member).layout;
      }));
}

auto CodeGenTypes::KeepsAboveWidthsClear(lir::TypeId type) const -> bool {
  if (const std::optional<value::IntegralShape> shape = IntegralShapeOf(type)) {
    return shape->width != value::PlaneBytesFor(shape->width) * 8;
  }
  const lir::Type& stated = unit_->types.Get(type);
  if (const auto* array = stated.As<lir::UnpackedArrayType>()) {
    return KeepsAboveWidthsClear(array->element_type);
  }
  const std::optional<lir::RecordKind> kind = lir::RecordKindOf(*unit_, type);
  if (kind.has_value()) {
    switch (*kind) {
      case lir::RecordKind::kStruct:
      case lir::RecordKind::kTaggedUnion:
        break;
      case lir::RecordKind::kUnion:
        return false;
    }
  }
  const std::optional<std::span<const lir::TypeId>> parts =
      lir::ComponentTypes(*unit_, type);
  return parts.has_value() &&
         std::ranges::any_of(*parts, [&](lir::TypeId part) {
           return KeepsAboveWidthsClear(part);
         });
}

auto CodeGenTypes::LayoutOfProduct(lir::TypeId product)
    -> const ProductLayout& {
  if (const auto found = products_.find(product); found != products_.end()) {
    return found->second;
  }
  const std::optional<std::span<const lir::TypeId>> components =
      lir::ProductElements(*unit_, product);
  if (!components.has_value()) {
    throw InternalError(
        "llvm codegen: a product layout asked of a type that is no product");
  }
  std::vector<support::ObjectLayout> held;
  held.reserve(components->size());
  for (const lir::TypeId component : *components) {
    held.push_back(LayoutOf(component));
  }
  value::ProductLayout laid = value::ProductLayoutOf(
      held | std::views::transform(&support::ObjectLayout::layout));
  ProductLayout layout{
      .components = {components->begin(), components->end()},
      .offsets = std::move(laid.offsets),
      .storage = {
          .layout = laid.whole, .needs_end = lir::NeedsEnd(*unit_, product)}};
  return products_.emplace(product, std::move(layout)).first->second;
}

auto HolderStorageOf(const support::RuntimeObject& object)
    -> support::ObjectLayout {
  const std::optional<support::ObjectLayout> stated =
      support::StatedStorageOf(object);
  if (!stated.has_value()) {
    throw InternalError(
        "llvm codegen: a holder gives storage to an object no storage is "
        "stated for -- please report this as a bug");
  }
  return *stated;
}

}  // namespace lyra::backend::llvm_backend
