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
#include "lyra/backend/llvm/fn_abi.hpp"
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

auto CodeGenTypes::GetFunctionType(const FnAbi& abi) -> llvm::FunctionType* {
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

auto ShapeOf(const lir::IntegralType& integral) -> value::IntegralShape {
  value::IntegralShape shape{.width = integral.bit_width};
  switch (integral.signedness) {
    case lir::Signedness::kSigned:
      shape.signedness = value::Signedness::kSigned;
      break;
    case lir::Signedness::kUnsigned:
      shape.signedness = value::Signedness::kUnsigned;
      break;
  }
  shape.domain = lir::StateDomainOf(integral.state_kind);
  return shape;
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

namespace {

// The runtime object a value of a LIR type is held as, and nothing for a type
// whose value is held as itself or reached where it lives; a scalar, and a
// value laid out by its type, is named by the domain its cells and containers
// hold it in.
auto HeldObjectOf(const lir::Type& type)
    -> std::optional<support::RuntimeObject> {
  using Object = std::optional<support::RuntimeObject>;
  const std::optional<lir::Holding> held = type.HeldAs();
  if (!held.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](const value::Scalar& scalar) -> Object {
            return value::FactsOf(scalar).domain;
          },
          [](const lir::LaidOutByType&) -> Object {
            return support::ValueDomain::kLaidOut;
          },
          [](const support::RuntimeObject& object) -> Object { return object; },
          // The address of storage a pointer owns is the pointer itself, and a
          // closure is ended by the body its declaration states for it, so
          // neither is an object of an entry's.
          [](const lir::OwnedStorageAddress&) -> Object {
            return std::nullopt;
          },
          [](const lir::ClosureStorage&) -> Object { return std::nullopt; }},
      *held);
}

}  // namespace

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain> {
  const std::optional<support::RuntimeObject> held =
      HeldObjectOf(unit.types.Get(type));
  if (!held.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](support::ValueDomain domain)
              -> std::optional<support::ValueDomain> { return domain; },
          [](support::LibraryDecl) -> std::optional<support::ValueDomain> {
            return std::nullopt;
          },
          [](support::LibraryObject) -> std::optional<support::ValueDomain> {
            return std::nullopt;
          }},
      *held);
}

auto MemberSlotRoleOf(const lir::TypeDeclaration& declaration)
    -> MemberSlotRole {
  return std::visit(
      Overloaded{
          [](const lir::ObjectType&) { return MemberSlotRole::kVariable; },
          [](const lir::ClosureType&) { return MemberSlotRole::kSnapshot; }},
      declaration);
}

auto DeclaredStorageOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::DeclaredMemberStorage> {
  using Declared = std::optional<support::DeclaredMemberStorage>;
  // A value the owner holds, in the slot its role asks for. It is storage only
  // where the runtime realizes values of that type at all.
  const auto held_value = [&](lir::TypeId value) -> Declared {
    return ValueDomainOf(unit, value)
        .transform(
            [&](support::ValueDomain domain) -> support::DeclaredMemberStorage {
              switch (role) {
                case MemberSlotRole::kVariable:
                  return support::ValueCellMember{.domain = domain};
                case MemberSlotRole::kSnapshot:
                  return support::InlineValueMember{.domain = domain};
              }
              throw InternalError("llvm codegen: unknown member slot role");
            });
  };
  const auto value_of = [&](const auto&) { return held_value(type); };
  // Storage the owner holds over values of one domain -- a cell, a net's
  // resolution node, a history. Each is that storage only where the runtime
  // realizes values of the domain it holds, for the same reason a value member
  // is: the storage is built from the domain and there is nothing to build it
  // from otherwise.
  const auto over_values =
      [&](lir::TypeId value,
          auto (*storage)(support::ValueDomain)->support::DeclaredMemberStorage)
      -> Declared { return ValueDomainOf(unit, value).transform(storage); };
  const auto borrowed = [](const auto&) -> Declared {
    return support::BorrowedHandleMember{};
  };
  const auto none = [](const auto&) -> Declared { return std::nullopt; };
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const lir::ObservableType& observable) {
            return over_values(
                observable.value,
                [](support::ValueDomain domain)
                    -> support::DeclaredMemberStorage {
                  return support::ObservableCellMember{.domain = domain};
                });
          },
          [&](const lir::ResolvedType& net) {
            return over_values(
                net.value,
                [](support::ValueDomain domain)
                    -> support::DeclaredMemberStorage {
                  return support::ResolvedNetMember{.domain = domain};
                });
          },
          // A driver is a handle on a contribution the net owns and issues (LRM
          // 6.5); a declaration standing for several objects keeps a handle on
          // the sequence of them, built once where the owner is built; and a
          // code address names a body that outlives every owner there is. None
          // of these owns what it names.
          [&](const lir::DriverType& t) { return borrowed(t); },
          [&](const lir::VectorType& t) { return borrowed(t); },
          // A reference names storage living elsewhere too, and it is more than
          // an address: the variable that storage belongs to travels with it.
          [](const lir::VariableRefType&) -> Declared {
            return support::LibraryObjectMember{
                .object = support::LibraryObject::kReference};
          },
          [](const lir::AnyReferenceType&) -> Declared {
            return support::LibraryObjectMember{
                .object = support::LibraryObject::kAnyReference};
          },
          [&](const lir::MachineFunctionType& t) { return borrowed(t); },
          // A pointer is the one whose ownership decides the answer. A borrowed
          // one likewise names storage somebody else ends; a unique one is the
          // address of what its holder owns, filled once and ended with the
          // holder; a shared one is a hold: it keeps what it names in existence
          // and the storage ends once no holder is left, which is how a scope
          // outlives the control flow that left it (LRM 6.21).
          [&](const lir::PointerType& pointer) -> Declared {
            switch (pointer.ownership) {
              case lir::PointerOwnership::kUnique:
                return support::InlineBytesMember{};
              case lir::PointerOwnership::kBorrowed:
                return support::BorrowedHandleMember{};
              case lir::PointerOwnership::kShared:
                return support::LibraryObjectMember{
                    .object = support::LibraryObject::kSharedPointer};
            }
            throw InternalError("llvm codegen: unknown pointer ownership");
          },
          // A type the library declares is the member its declaration says:
          // none, the value it is, its own object, a handle on what outlives
          // the member, or a history over the values it holds.
          [&](const lir::LibraryType& library) -> Declared {
            return std::visit(
                Overloaded{
                    [](const support::NoMember&) -> Declared {
                      return std::nullopt;
                    },
                    [&](const support::ValueMember&) {
                      return held_value(type);
                    },
                    [&](const support::OwnObjectMember&) -> Declared {
                      return support::LibraryDeclMember{.decl = library.decl};
                    },
                    [&](const support::BorrowedMember& m) {
                      return borrowed(m);
                    },
                    [&](const support::HistoryMember& history) {
                      return over_values(
                          library.TypeArg(history.values),
                          [](support::ValueDomain domain)
                              -> support::DeclaredMemberStorage {
                            return support::SampledHistoryMember{
                                .domain = domain};
                          });
                    }},
                support::DeclarationOf(library.decl).member);
          },
          // A class handle is a value the member holds rather than a pointer it
          // merely points with: the object stays alive because the member
          // refers to it (LRM 8.3), so a write copies the handle's share of
          // ownership and not just its address. That is what separates it from
          // every borrowed form above, and from a reference owning nothing,
          // whose value is the bare address it carries (LRM 6.14), and which a
          // member holds as the value it is all the same.
          [&](const lir::ManagedRefType& t) { return value_of(t); },
          [&](const lir::UnmanagedRefType& t) { return value_of(t); },
          [&](const lir::IntegralType& t) { return value_of(t); },
          [&](const lir::UnpackedArrayType& t) { return value_of(t); },
          [&](const lir::RealType& t) { return value_of(t); },
          [&](const lir::TupleType& t) { return value_of(t); },
          [&](const lir::RecordType& t) { return value_of(t); },
          // A machine integer is a computed value rather than a declaration's
          // storage, so no variable is one; a closure built with one holds it
          // as that integer, which is what a read of the capture hands back.
          [&](const lir::MachineIntType&) -> Declared {
            switch (role) {
              case MemberSlotRole::kVariable:
                return std::nullopt;
              case MemberSlotRole::kSnapshot:
                return support::InlineBytesMember{};
            }
            throw InternalError("llvm codegen: unknown member slot role");
          },
          // The rest name no storage a member can be. Any other machine
          // primitive is a computed value no owner holds; an object-tree node,
          // a closure and a coroutine are reached through a handle, so a
          // member holding one holds that handle and arrives here as its own
          // type; and `void` has no runtime realization at all.
          [&](const lir::MachineCStringType& t) { return none(t); },
          [&](const lir::MachineBoolType& t) { return none(t); },
          [&](const lir::MachineFloatType& t) { return none(t); },
          [&](const lir::MachineArrayType& t) { return none(t); },
          [&](const lir::MachineSpanType& t) { return none(t); },
          [&](const lir::VoidType& t) { return none(t); },
          [&](const lir::ObjectType& t) { return none(t); },
          [&](const lir::ClosureType& t) { return none(t); },
          [&](const lir::ErasedCallableType& t) { return none(t); },
          [&](const lir::CoroutineType& t) { return none(t); },
          // A write is open for the length of the full-expression doing it,
          // which no owner outlasts, and so is a part designated within it,
          // and so is a write into an object's properties.
          [&](const lir::OpenWriteType& t) { return none(t); },
          [&](const lir::DesignationType& t) { return none(t); },
          [&](const lir::ObjectWriteType& t) { return none(t); }});
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
