#include "lyra/mir/library_records.hpp"

#include <concepts>
#include <cstdint>
#include <type_traits>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::mir {

namespace {

// The MIR type each C++ type a member of the library's structures is declared
// with is, as Go's compiler builds its own type for a structure its runtime
// declares from that structure's declaration. A member of a type with no
// answer here does not compile.

template <std::integral T>
consteval auto MachineIntWidthOf() -> MachineIntWidth {
  static_assert(
      sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 || sizeof(T) == 8,
      "a structure of the library holds an integer of no machine width");
  if constexpr (sizeof(T) == 1) {
    return MachineIntWidth::k8;
  } else if constexpr (sizeof(T) == 2) {
    return MachineIntWidth::k16;
  } else if constexpr (sizeof(T) == 4) {
    return MachineIntWidth::k32;
  } else {
    return MachineIntWidth::k64;
  }
}

auto VoidPointer(const TypePool& types, Mutability mutability) -> TypeId {
  return types.Intern(
      Type{PointerType{
          .pointee = types.Intern(Type{VoidType{}}),
          .ownership = PointerOwnership::kBorrowed,
          .mutability = mutability}});
}

template <typename Field>
auto MirTypeOf(const TypePool& types) -> TypeId;

// The prototype of a body the library enters, as the library enters it.
template <typename Pointer>
struct Prototype;

template <typename Result, typename... Params>
struct Prototype<Result (*)(Params...)> {
  static auto Of(const TypePool& types) -> TypeId {
    return types.Intern(
        Type{MachineFunctionType{
            .params = {MirTypeOf<Params>(types)...},
            .result = MirTypeOf<Result>(types)}});
  }
};

template <typename Field>
auto MirTypeOf(const TypePool& types) -> TypeId {
  if constexpr (std::is_void_v<Field>) {
    return types.Intern(Type{VoidType{}});
  } else if constexpr (std::integral<Field>) {
    return types.Intern(
        Type{MachineIntType{
            .width = MachineIntWidthOf<Field>(),
            .signedness = std::is_signed_v<Field> ? Signedness::kSigned
                                                  : Signedness::kUnsigned}});
  } else if constexpr (std::is_same_v<Field, const char*>) {
    return types.Intern(Type{MachineCStringType{}});
  } else if constexpr (std::is_same_v<Field, const void*>) {
    return VoidPointer(types, Mutability::kReadOnly);
  } else if constexpr (std::is_same_v<Field, void*>) {
    return VoidPointer(types, Mutability::kMutable);
  } else if constexpr (std::is_same_v<Field, runtime::GcObject*>) {
    return ObjectRootPointer(types);
  } else if constexpr (std::is_same_v<Field, const value::ObjectRef&>) {
    // A handle, of whatever class, borrowed for the call.
    return types.Intern(
        Type{ReferenceType{
            .referent = types.Intern(
                Type{ManagedRefType{.pointee = ObjectRootType(types)}}),
            .mutability = Mutability::kReadOnly}});
  } else if constexpr (abi::kIsConstantSpan<Field>) {
    // The library lays a view of entries out as where the first one is and
    // how many there are, which is all a view holds; a layout saying more or
    // otherwise is not one this reads as a view.
    using Entry =
        std::remove_const_t<std::remove_pointer_t<decltype(Field::data)>>;
    static_assert(
        std::is_same_v<decltype(Field::data), const Entry*> &&
            std::is_same_v<decltype(Field::size), std::uint64_t>,
        "the library lays a view of entries out as other than where the "
        "first is and how many there are");
    return types.Intern(
        Type{MachineSpanType{.element = MirTypeOf<Entry>(types)}});
  } else if constexpr (std::is_class_v<Field>) {
    return types.Intern(
        Type{RuntimeLibraryType{.kind = LibraryRecordKind<Field>()}});
  } else if constexpr (
      std::is_pointer_v<Field> &&
      std::is_function_v<std::remove_pointer_t<Field>>) {
    return Prototype<Field>::Of(types);
  } else if constexpr (
      std::is_pointer_v<Field> &&
      std::is_const_v<std::remove_pointer_t<Field>> &&
      std::is_class_v<std::remove_pointer_t<Field>>) {
    return types.Intern(
        Type{PointerType{
            .pointee =
                MirTypeOf<std::remove_const_t<std::remove_pointer_t<Field>>>(
                    types),
            .ownership = PointerOwnership::kBorrowed,
            .mutability = Mutability::kReadOnly}});
  } else {
    static_assert(
        false,
        "a structure of the library holds a member of a type MIR states no "
        "counterpart of");
  }
}

template <typename Record>
auto MembersOf(const TypePool& types) -> std::vector<TypeId> {
  std::vector<TypeId> members;
  abi::ForEachField(Record{}, [&](const auto& field) {
    members.push_back(MirTypeOf<std::remove_cvref_t<decltype(field)>>(types));
  });
  return members;
}

}  // namespace

auto DeclareLibraryRecords(const TypePool& types)
    -> std::vector<LibraryRecordDecl> {
  std::vector<LibraryRecordDecl> records;
  ForEachLibraryRecord([&]<typename Record>(RuntimeLibraryKind kind) {
    records.push_back(
        LibraryRecordDecl{.kind = kind, .members = MembersOf<Record>(types)});
  });
  return records;
}

}  // namespace lyra::mir
