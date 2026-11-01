#include "lyra/mir/type_builders.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::mir {

auto PackedVectorOf(
    const TypePool& types, std::uint64_t width, IntegralStateKind state_kind)
    -> TypeId {
  return types.Intern(
      Type{IntegralType{
          .bit_width = width,
          .signedness = Signedness::kUnsigned,
          .state_kind = state_kind}});
}

namespace {

constexpr auto SignednessOf(value::Signedness signedness) -> Signedness {
  switch (signedness) {
    case value::Signedness::kSigned:
      return Signedness::kSigned;
    case value::Signedness::kUnsigned:
      return Signedness::kUnsigned;
  }
  std::unreachable();
}

constexpr auto StateKindOf(value::StateDomain domain) -> IntegralStateKind {
  switch (domain) {
    case value::StateDomain::kTwoState:
      return IntegralStateKind::kTwoState;
    case value::StateDomain::kFourState:
      return IntegralStateKind::kFourState;
  }
  std::unreachable();
}

// The type the value library reads a position at, which is where it is
// stated.
constexpr IntegralType kPosition{
    .bit_width = value::Position::kWidth,
    .signedness = SignednessOf(value::Position::kSignedness),
    .state_kind = StateKindOf(value::Position::kDomain)};

}  // namespace

auto PositionType(const TypePool& types) -> TypeId {
  return types.Intern(Type{kPosition});
}

auto IsPositionType(const TypePool& types, TypeId id) -> bool {
  const Type& type = types.Get(id);
  return type.IsIntegral() && type.Integral() == kPosition;
}

auto MachineArrayOf(const TypePool& types, TypeId element, std::size_t size)
    -> TypeId {
  return types.Intern(
      Type{MachineArrayType{
          .element = element, .size = static_cast<std::uint32_t>(size)}});
}

auto DynamicArrayOf(const TypePool& types, TypeId element) -> TypeId {
  return types.Intern(
      Type{LibraryType{
          .decl = support::LibraryDecl::kDynamicArray, .args = {element}}});
}

auto QueueOf(const TypePool& types, TypeId element, support::QueueBound bound)
    -> TypeId {
  return types.Intern(
      Type{LibraryType{
          .decl = support::LibraryDecl::kQueue, .args = {element, bound}}});
}

auto AssociativeArrayOf(const TypePool& types, TypeId index, TypeId element)
    -> TypeId {
  return types.Intern(
      Type{LibraryType{
          .decl = support::LibraryDecl::kAssociativeArray,
          .args = {index, element}}});
}

auto WithElementType(const TypePool& types, TypeId container, TypeId element)
    -> TypeId {
  LibraryType rewritten = types.Get(container).Get<LibraryType>();
  const std::optional<std::size_t> position =
      support::ParamOf(rewritten.decl, support::LibraryParam::kElement);
  if (!position.has_value()) {
    throw InternalError(
        "mir: the element type of a type that holds no elements was replaced "
        "-- please report this as a bug");
  }
  rewritten.args.at(*position) = element;
  return types.Intern(Type{std::move(rewritten)});
}

auto LibraryTypeOf(const TypePool& types, support::LibraryDecl decl) -> TypeId {
  return types.Intern(Type{LibraryType{.decl = decl, .args = {}}});
}

auto ErasedFunction(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{MachineFunctionType{
          .params = {}, .result = types.Intern(Type{VoidType{}})}});
}

// Asked of a finished unit by whatever reads it, so it reads the type rather
// than interning the one it is compared against, which would add to the unit.
auto IsErasedFunction(const TypePool& types, TypeId id) -> bool {
  const auto* function = types.Get(id).As<MachineFunctionType>();
  return function != nullptr && function->params.empty() &&
         types.Get(function->result).Is<VoidType>();
}

auto ErasedPointer(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee = types.Intern(Type{VoidType{}}),
          .ownership = PointerOwnership::kBorrowed}});
}

auto BoxOf(const TypePool& types, TypeId value) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee = value, .ownership = PointerOwnership::kUnique}});
}

auto AllocatedType(const TypePool& types, TypeId owner)
    -> std::optional<TypeId> {
  const Type& type = types.Get(owner);
  if (const auto* handle = type.As<ManagedRefType>()) {
    return handle->pointee;
  }
  const auto* pointer = type.As<PointerType>();
  if (pointer == nullptr || pointer->ownership != PointerOwnership::kUnique) {
    return std::nullopt;
  }
  return pointer->pointee;
}

auto ClassDefinitionPointer(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee =
              LibraryTypeOf(types, support::LibraryDecl::kObjectDefinition),
          .ownership = PointerOwnership::kBorrowed,
          .mutability = Mutability::kReadOnly}});
}

auto FunctionTypeOf(const TypePool& types, const CallableCode& code) -> TypeId {
  std::vector<TypeId> params;
  params.reserve(code.params.size());
  for (const LocalId param : code.params) {
    params.push_back(code.locals.Get(param).type);
  }
  return types.Intern(
      Type{MachineFunctionType{
          .params = std::move(params), .result = code.result_type}});
}

auto ObjectRootPointer(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee = LibraryTypeOf(types, support::LibraryDecl::kObjectRoot),
          .ownership = PointerOwnership::kBorrowed}});
}

auto ConstructionTablePointer(const TypePool& types) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee =
              LibraryTypeOf(types, support::LibraryDecl::kConstructionTable),
          .ownership = PointerOwnership::kBorrowed,
          .mutability = Mutability::kReadOnly}});
}

auto ObservableCellOf(const TypePool& types, TypeId value_type) -> TypeId {
  if (!IsDataType(types, value_type)) {
    return value_type;
  }
  return types.Intern(Type{ObservableType{.value = value_type}});
}

}  // namespace lyra::mir
