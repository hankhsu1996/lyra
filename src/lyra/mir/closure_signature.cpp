#include "lyra/mir/closure_signature.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::mir {

namespace {

// Whether `pointer` is a box: the one owner of what it points at.
auto IsBox(const PointerType& pointer) -> bool {
  switch (pointer.ownership) {
    case PointerOwnership::kUnique:
      return true;
    case PointerOwnership::kShared:
    case PointerOwnership::kBorrowed:
      return false;
  }
  throw InternalError("mir::IsBox: unknown PointerOwnership");
}

}  // namespace

auto ClosureReceiverType(
    const TypePool& types, ClosureId closure, support::ClosureKind kind)
    -> TypeId {
  const TypeId value = types.Intern(Type{ClosureType{.closure_id = closure}});
  const auto borrow = [&](Mutability mutability) {
    return types.Intern(
        Type{PointerType{
            .pointee = value,
            .ownership = PointerOwnership::kBorrowed,
            .mutability = mutability}});
  };
  switch (kind) {
    case support::ClosureKind::kFn:
      return borrow(Mutability::kReadOnly);
    case support::ClosureKind::kFnMut:
      return borrow(Mutability::kMutable);
    case support::ClosureKind::kFnOnce:
      return BoxOf(types, value);
  }
  throw InternalError("mir::ClosureReceiverType: unknown ClosureKind");
}

auto ClosureKindOf(const TypePool& types, const ClosureDecl& closure)
    -> support::ClosureKind {
  if (!closure.invoke.receiver.has_value()) {
    throw InternalError(
        "mir::ClosureKindOf: a closure's body is entered on no receiver -- "
        "please report this as a bug");
  }
  const auto* pointer =
      types.Get(closure.invoke.locals.Get(*closure.invoke.receiver).type)
          .As<PointerType>();
  if (pointer == nullptr) {
    throw InternalError(
        "mir::ClosureKindOf: a closure's body is entered on something other "
        "than a pointer to the closure -- please report this as a bug");
  }
  switch (pointer->ownership) {
    case PointerOwnership::kUnique:
      return support::ClosureKind::kFnOnce;
    case PointerOwnership::kBorrowed:
      switch (pointer->mutability) {
        case Mutability::kReadOnly:
          return support::ClosureKind::kFn;
        case Mutability::kMutable:
          return support::ClosureKind::kFnMut;
      }
      throw InternalError("mir::ClosureKindOf: unknown Mutability");
    case PointerOwnership::kShared:
      throw InternalError(
          "mir::ClosureKindOf: a closure's body is entered on a shared "
          "pointer to it -- please report this as a bug");
  }
  throw InternalError("mir::ClosureKindOf: unknown PointerOwnership");
}

auto ErasedTypeOf(const TypePool& types, const ClosureDecl& closure) -> TypeId {
  const CallableCode& body = closure.invoke;
  std::vector<TypeId> params;
  params.reserve(body.params.size());
  for (const LocalId param : body.params) {
    params.push_back(body.locals.Get(param).type);
  }
  return types.Intern(
      Type{ErasedCallableType{
          .kind = ClosureKindOf(types, closure),
          .params = std::move(params),
          .result = body.result_type}});
}

auto ClosureErasureOf(const TypePool& types, TypeId from, TypeId to)
    -> std::optional<ClosureErasure> {
  const Type& source = types.Get(from);
  const Type& destination = types.Get(to);
  std::optional<TypeId> closure;
  std::optional<TypeId> erased;
  if (const auto* box = destination.As<PointerType>()) {
    const auto* other = source.As<PointerType>();
    if (other != nullptr && IsBox(*box) && IsBox(*other) &&
        other->mutability == box->mutability) {
      closure = other->pointee;
      erased = box->pointee;
    }
  } else if (const auto* borrow = destination.As<ReferenceType>()) {
    const auto* other = source.As<ReferenceType>();
    if (other != nullptr && other->mutability == borrow->mutability) {
      closure = other->referent;
      erased = borrow->referent;
    }
  }
  if (!closure.has_value() || !types.Get(*erased).Is<ErasedCallableType>()) {
    return std::nullopt;
  }
  const auto* known = types.Get(*closure).As<ClosureType>();
  if (known == nullptr) {
    return std::nullopt;
  }
  return ClosureErasure{.closure = known->closure_id, .erased = *erased};
}

}  // namespace lyra::mir
