#include "lyra/backend/llvm/type_realization.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/scalar.hpp"

namespace lyra::backend::llvm_backend {

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

}  // namespace lyra::backend::llvm_backend
