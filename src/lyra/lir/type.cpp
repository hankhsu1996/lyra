#include "lyra/lir/type.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/scalar.hpp"

namespace lyra::lir {

namespace {

void Combine(std::size_t& seed, std::uint64_t value) {
  base::HashField(seed, value);
}

void Combine(std::size_t& seed, TypeId id) {
  Combine(seed, id.value);
}

void Combine(std::size_t& seed, ClassId id) {
  Combine(seed, id.value);
}

template <typename E>
  requires std::is_enum_v<E>
void Combine(std::size_t& seed, E value) {
  Combine(seed, static_cast<std::uint64_t>(value));
}

void Combine(std::size_t& seed, const std::vector<TypeId>& ids) {
  for (const TypeId id : ids) {
    Combine(seed, id);
  }
}

void Combine(std::size_t& seed, const IntegralType& integral) {
  Combine(seed, integral.bit_width);
  Combine(seed, integral.signedness);
  Combine(seed, integral.state_kind);
}

}  // namespace

auto StateDomainOf(IntegralStateKind state) -> value::StateDomain {
  switch (state) {
    case IntegralStateKind::kTwoState:
      return value::StateDomain::kTwoState;
    case IntegralStateKind::kFourState:
      return value::StateDomain::kFourState;
  }
  throw InternalError("lir: unknown integral state kind");
}

auto BitsOf(MachineIntWidth width) -> std::uint32_t {
  switch (width) {
    case MachineIntWidth::k8:
      return 8;
    case MachineIntWidth::k16:
      return 16;
    case MachineIntWidth::k32:
      return 32;
    case MachineIntWidth::k64:
      return 64;
  }
  throw InternalError("lir: unknown MachineIntWidth");
}

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit(
      Overloaded{
          [&](const IntegralType& t) { Combine(seed, t); },
          [&](const UnpackedArrayType& t) {
            Combine(seed, t.element_type);
            Combine(seed, t.size);
          },
          [](const AnyReferenceType&) {},
          [](const MachineCStringType&) {},
          [](const MachineBoolType&) {},
          [&](const MachineIntType& t) {
            Combine(seed, t.width);
            Combine(seed, t.signedness);
          },
          [&](const MachineFloatType& t) { Combine(seed, t.width); },
          [&](const MachineArrayType& t) {
            Combine(seed, t.element);
            Combine(seed, t.size);
          },
          [&](const MachineSpanType& t) { Combine(seed, t.element); },
          [&](const MachineFunctionType& t) {
            for (const TypeId param : t.params) {
              Combine(seed, param);
            }
            Combine(seed, t.result);
          },
          [&](const RealType& t) { Combine(seed, t.width); },
          [](const VoidType&) {},
          [&](const ObjectType& t) { Combine(seed, t.class_id); },
          [&](const ClosureType& t) { Combine(seed, t.closure_id.value); },
          [&](const ErasedCallableType& t) {
            for (const TypeId param : t.params) {
              Combine(seed, param);
            }
            Combine(seed, t.result);
          },
          [&](const LibraryType& t) {
            Combine(seed, t.decl);
            for (const LibraryType::Arg& arg : t.args) {
              std::visit(
                  Overloaded{
                      [&](TypeId type) { Combine(seed, type); },
                      [&](const support::QueueBound& bound) {
                        Combine(
                            seed, static_cast<std::uint64_t>(bound.index()));
                        if (const auto* most =
                                std::get_if<support::BoundedQueue>(&bound)) {
                          Combine(seed, most->count);
                        }
                      }},
                  arg);
            }
          },
          [&](const CoroutineType& t) { Combine(seed, t.payload); },
          [&](const VariableRefType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.mutability);
          },
          [&](const PointerType& t) {
            Combine(seed, t.pointee);
            Combine(seed, t.ownership);
            Combine(seed, t.mutability);
          },
          [&](const ManagedRefType& t) { Combine(seed, t.pointee); },
          [&](const UnmanagedRefType& t) { Combine(seed, t.pointee); },
          [&](const VectorType& t) { Combine(seed, t.element); },
          [&](const TupleType& t) { Combine(seed, t.elements); },
          [&](const RecordType& t) { Combine(seed, t.declaration.value); },
          [&](const ResolvedType& t) { Combine(seed, t.value); },
          [&](const DriverType& t) { Combine(seed, t.value); },
          [&](const ObservableType& t) { Combine(seed, t.value); },
          [&](const OpenWriteType& t) { Combine(seed, t.value); },
          [&](const DesignationType& t) { Combine(seed, t.value); },
          [](const ObjectWriteType&) {}});
  return seed;
}

auto Type::KindName() const -> std::string_view {
  using Name = std::string_view;
  return Visit(
      Overloaded{
          [](const IntegralType&) -> Name { return "integral"; },
          [](const UnpackedArrayType&) -> Name { return "unpacked array"; },
          [](const AnyReferenceType&) -> Name {
            return "reference to a value of any type";
          },
          [](const MachineCStringType&) -> Name { return "machine C string"; },
          [](const MachineBoolType&) -> Name { return "machine boolean"; },
          [](const MachineIntType&) -> Name { return "machine integer"; },
          [](const MachineFloatType&) -> Name { return "machine float"; },
          [](const MachineArrayType&) -> Name { return "machine array"; },
          [](const MachineSpanType&) -> Name { return "machine span"; },
          [](const MachineFunctionType&) -> Name { return "machine function"; },
          [](const RealType& t) -> Name {
            switch (t.width) {
              case support::FloatWidth::k32:
                return "shortreal";
              case support::FloatWidth::k64:
                return "real";
            }
            throw InternalError("lir: unknown real width");
          },
          [](const VoidType&) -> Name { return "void"; },
          [](const ObjectType&) -> Name { return "class object"; },
          [](const ClosureType&) -> Name { return "closure"; },
          [](const ErasedCallableType&) -> Name { return "erased callable"; },
          [](const RecordType&) -> Name { return "record"; },
          [](const LibraryType& t) -> Name {
            return support::DeclarationOf(t.decl).name;
          },
          [](const CoroutineType&) -> Name { return "coroutine"; },
          [](const VariableRefType&) -> Name { return "variable reference"; },
          [](const PointerType&) -> Name { return "pointer"; },
          [](const ManagedRefType&) -> Name { return "managed reference"; },
          [](const UnmanagedRefType&) -> Name { return "unmanaged reference"; },
          [](const VectorType&) -> Name { return "vector"; },
          [](const TupleType&) -> Name { return "tuple"; },
          [](const ResolvedType&) -> Name { return "net resolution node"; },
          [](const DriverType&) -> Name { return "net driver"; },
          [](const ObservableType&) -> Name { return "observable cell"; },
          [](const OpenWriteType&) -> Name { return "open write"; },
          [](const DesignationType&) -> Name { return "designation"; },
          [](const ObjectWriteType&) -> Name { return "object write"; }});
}

auto Type::Declaration() const -> std::optional<TypeDeclaration> {
  using Declared = std::optional<TypeDeclaration>;
  const auto names_none = []() -> Declared { return std::nullopt; };
  return Visit(
      Overloaded{
          // Member-bearing storage, whose members are reached through its
          // declaration.
          [](const ObjectType& t) -> Declared { return t; },
          [](const ClosureType& t) -> Declared { return t; },

          // A record names a declaration too, but of a value, whose members
          // are reached by position as a tuple's components are.
          [&](const RecordType&) { return names_none(); },

          // A value, however it is shaped and however its elements are held.
          [&](const IntegralType&) { return names_none(); },
          [&](const UnpackedArrayType&) { return names_none(); },
          [&](const AnyReferenceType&) { return names_none(); },
          [&](const RealType&) { return names_none(); },
          [&](const TupleType&) { return names_none(); },
          [&](const VoidType&) { return names_none(); },

          // The machine vocabulary, which crosses to a target on the target's
          // own terms and belongs to no declaration of the design.
          [&](const MachineCStringType&) { return names_none(); },
          [&](const MachineBoolType&) { return names_none(); },
          [&](const MachineIntType&) { return names_none(); },
          [&](const MachineFloatType&) { return names_none(); },
          [&](const MachineArrayType&) { return names_none(); },
          [&](const MachineSpanType&) { return names_none(); },
          [&](const MachineFunctionType&) { return names_none(); },
          [&](const ErasedCallableType&) { return names_none(); },

          // A type the library declares: its declaration is the library's,
          // so no declaration of this unit stands behind it.
          [&](const LibraryType&) { return names_none(); },

          // Storage and an address. Each stands for something else; what a
          // declaration is behind is whatever it stands for.
          [&](const ObservableType&) { return names_none(); },
          [&](const ResolvedType&) { return names_none(); },
          [&](const DriverType&) { return names_none(); },
          [&](const OpenWriteType&) { return names_none(); },
          [&](const DesignationType&) { return names_none(); },
          [&](const ObjectWriteType&) { return names_none(); },
          [&](const CoroutineType&) { return names_none(); },
          [&](const VariableRefType&) { return names_none(); },
          [&](const PointerType&) { return names_none(); },
          [&](const ManagedRefType&) { return names_none(); },
          [&](const UnmanagedRefType&) { return names_none(); },
          [&](const VectorType&) { return names_none(); }});
}

auto Type::ContainerElementType() const -> std::optional<TypeId> {
  using Element = std::optional<TypeId>;
  return Visit(
      Overloaded{
          // A sequence of values whose count is the type's, and a container
          // the library declares, which says which of its arguments is its
          // element type.
          [](const UnpackedArrayType& t) -> Element { return t.element_type; },
          [](const LibraryType& t) -> Element {
            return t.FindTypeArg(support::LibraryParam::kElement);
          },

          // These hold a sequence of values too and are still not containers: a
          // lowering builds them to carry something, where a container is a
          // type a declaration named.
          [](const MachineArrayType&) -> Element { return std::nullopt; },
          [](const MachineSpanType&) -> Element { return std::nullopt; },
          [](const VectorType&) -> Element { return std::nullopt; },

          // One vector of bits: what looks like an element is bits of that
          // vector rather than a value held beside the others.
          [](const IntegralType&) -> Element { return std::nullopt; },

          // Parts side by side, or members overlaid, but never a sequence of
          // one type.
          [](const TupleType&) -> Element { return std::nullopt; },
          [](const RecordType&) -> Element { return std::nullopt; },

          // A single quantity or a single token.
          [](const AnyReferenceType&) -> Element { return std::nullopt; },
          [](const MachineCStringType&) -> Element { return std::nullopt; },
          [](const MachineBoolType&) -> Element { return std::nullopt; },
          [](const MachineIntType&) -> Element { return std::nullopt; },
          [](const MachineFloatType&) -> Element { return std::nullopt; },
          [](const RealType&) -> Element { return std::nullopt; },
          [](const VoidType&) -> Element { return std::nullopt; },

          // Storage that keeps one value, and a write open on such storage;
          // the sequence, where there is one, belongs to the value kept.
          [](const ObservableType&) -> Element { return std::nullopt; },
          [](const ResolvedType&) -> Element { return std::nullopt; },
          [](const DriverType&) -> Element { return std::nullopt; },
          [](const OpenWriteType&) -> Element { return std::nullopt; },
          [](const DesignationType&) -> Element { return std::nullopt; },
          [](const ObjectWriteType&) -> Element { return std::nullopt; },

          // These refer to a value living elsewhere rather than holding one,
          // so a container reached through one is reached by dereferencing it
          // first.
          [](const VariableRefType&) -> Element { return std::nullopt; },
          [](const PointerType&) -> Element { return std::nullopt; },
          [](const ManagedRefType&) -> Element { return std::nullopt; },
          [](const UnmanagedRefType&) -> Element { return std::nullopt; },
          [](const CoroutineType&) -> Element { return std::nullopt; },
          [](const MachineFunctionType&) -> Element { return std::nullopt; },

          // A nominal type names a declaration, and a declaration is not a run
          // of anything.
          [](const ObjectType&) -> Element { return std::nullopt; },
          [](const ClosureType&) -> Element { return std::nullopt; },
          [](const ErasedCallableType&) -> Element { return std::nullopt; }});
}

auto Type::IsQueue() const -> bool {
  const auto* library = As<LibraryType>();
  return library != nullptr && library->FindBound().has_value();
}

auto Type::IndexType() const -> std::optional<TypeId> {
  const auto* library = As<LibraryType>();
  if (library == nullptr) {
    return std::nullopt;
  }
  return library->FindTypeArg(support::LibraryParam::kIndex);
}

auto Type::Pointee() const -> std::optional<TypeId> {
  if (const auto* pointer = As<PointerType>()) {
    return pointer->pointee;
  }
  if (const auto* reference = As<VariableRefType>()) {
    return reference->pointee;
  }
  if (const auto* managed = As<ManagedRefType>()) {
    return managed->pointee;
  }
  if (const auto* unmanaged = As<UnmanagedRefType>()) {
    return unmanaged->pointee;
  }
  return std::nullopt;
}

auto Type::DerefTarget() const -> std::optional<TypeId> {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  return Pointee();
}

auto Type::IsAddressOnly() const -> bool {
  if (const auto* library = As<LibraryType>()) {
    return support::IsAddressOnly(library->decl);
  }
  return Is<ObservableType>() || Is<ResolvedType>() || Is<ObjectType>();
}

auto Type::HeldAs() const -> std::optional<Holding> {
  using Held = std::optional<Holding>;
  using support::LibraryObject;
  using support::RuntimeObject;
  using support::ValueDomain;
  return Visit(
      Overloaded{
          // A scalar the IR lays out is its own bytes: an integral value, a
          // real as the IEEE value of its width (LRM 6.12), and a reference
          // owning nothing as the address it holds, which is the whole value
          // (LRM 6.14, 25.9).
          [](const IntegralType& integral) -> Held {
            return value::Scalar{value::IntegralScalar{
                .width = integral.bit_width,
                .states = StateDomainOf(integral.state_kind)}};
          },
          [](const RealType& real) -> Held {
            return value::Scalar{value::RealScalar{.width = real.width}};
          },
          [](const UnmanagedRefType&) -> Held {
            return value::Scalar{value::UnmanagedRefScalar{}};
          },
          [](const AnyReferenceType&) -> Held {
            return RuntimeObject{LibraryObject::kAnyReference};
          },
          // A value laid out by its type -- a tuple, a structure, a union of
          // either kind, a fixed-size unpacked array (LRM 7.4.2) -- is held as
          // the bytes that type lays out, whatever the type says lies where.
          [](const TupleType&) -> Held { return LaidOutByType{}; },
          [](const RecordType&) -> Held { return LaidOutByType{}; },
          [](const UnpackedArrayType&) -> Held { return LaidOutByType{}; },
          // A class handle (LRM 8.3) refers to an object the simulator owns.
          // Which object it refers to is the whole value, so the value's
          // operations are the ones over a reference -- defaulting to null,
          // copying, and comparing identity.
          [](const ManagedRefType&) -> Held {
            return RuntimeObject{ValueDomain::kManagedRef};
          },

          [](const ClosureType&) -> Held { return ClosureStorage{}; },
          // A shared pointer is a hold on storage that outlives the scope
          // asking for it, and letting the hold go is what ends it; a unique
          // one owns what it points at, and its end ends that and gives the
          // storage back; a borrowed one names storage somebody else ends.
          [](const PointerType& pointer) -> Held {
            switch (pointer.ownership) {
              case PointerOwnership::kShared:
                return RuntimeObject{LibraryObject::kSharedPointer};
              case PointerOwnership::kUnique:
                return OwnedStorageAddress{};
              case PointerOwnership::kBorrowed:
                return std::nullopt;
            }
            throw InternalError("lir: unknown pointer ownership");
          },
          // A type the library declares is held as its declaration says: as
          // an object of the library's, or not at all where the run keeps it
          // or another object holds it, which is reached where it lies and so
          // is not this holder's to end.
          [](const LibraryType& library) -> Held {
            return support::ObjectOf(library.decl);
          },

          // Machine data is held as itself: a scalar, an array, a code address.
          [](const MachineCStringType&) -> Held { return std::nullopt; },
          [](const MachineBoolType&) -> Held { return std::nullopt; },
          [](const MachineIntType&) -> Held { return std::nullopt; },
          [](const MachineFloatType&) -> Held { return std::nullopt; },
          [](const MachineArrayType&) -> Held { return std::nullopt; },
          [](const MachineSpanType&) -> Held { return std::nullopt; },
          [](const MachineFunctionType&) -> Held { return std::nullopt; },
          // Only ever behind a box or a borrow, never held as itself.
          [](const ErasedCallableType&) -> Held { return std::nullopt; },
          // The absence of a type is not a value of one.
          [](const VoidType&) -> Held { return std::nullopt; },
          // An object, the several ways of naming one, and storage an owner
          // holds: each is reached where it lives rather than held, and what it
          // holds or answers with is a value of its own.
          [](const ObjectType&) -> Held { return std::nullopt; },
          [](const ObservableType&) -> Held { return std::nullopt; },
          [](const ResolvedType&) -> Held { return std::nullopt; },
          [](const DriverType&) -> Held { return std::nullopt; },
          [](const VectorType&) -> Held { return std::nullopt; },
          // A body in flight is taken by whoever drives it, the moment it is
          // made, so nothing is left for its maker to hold.
          [](const CoroutineType&) -> Held { return std::nullopt; },
          // Held by the writer for as long as the write lasts; ending it is
          // what reports the write. A part designated within it is built by
          // the step that reaches it and borrows the write.
          [](const OpenWriteType&) -> Held {
            return RuntimeObject{LibraryObject::kOpenWrite};
          },
          [](const DesignationType&) -> Held {
            return RuntimeObject{LibraryObject::kDesignation};
          },
          [](const ObjectWriteType&) -> Held {
            return RuntimeObject{LibraryObject::kObjectWrite};
          },
          // Where a value lies and the variable, if any, it belongs to, so a
          // write through it is told to whoever waits on that variable. The
          // storage it names is somebody else's, so ending it ends nothing.
          [](const VariableRefType&) -> Held {
            return RuntimeObject{LibraryObject::kReference};
          }});
}

auto Type::IsOwnedValue() const -> bool {
  return HeldAs().has_value();
}

auto Type::IsHeldInStorage() const -> bool {
  return IsOwnedValue() && !IsMoveOnly();
}

auto Type::IsMoveOnly() const -> bool {
  const std::optional<Holding> held = HeldAs();
  return held.has_value() && std::holds_alternative<OwnedStorageAddress>(*held);
}

auto Type::RuntimeClassOf() const -> std::optional<support::LibraryDecl> {
  const auto* library = As<LibraryType>();
  if (library == nullptr ||
      !support::DeclarationOf(library->decl).runtime_class.has_value()) {
    return std::nullopt;
  }
  return library->decl;
}

auto Type::MachineIntegerSignedness() const -> std::optional<Signedness> {
  if (const auto* machine = As<MachineIntType>()) {
    return machine->signedness;
  }
  if (Is<MachineBoolType>()) {
    return Signedness::kUnsigned;
  }
  return std::nullopt;
}

}  // namespace lyra::lir
