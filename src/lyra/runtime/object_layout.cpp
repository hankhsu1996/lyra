#include "lyra/runtime/object_layout.hpp"

#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <type_traits>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/any.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/constraint_problem.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/loop_blocks.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/stated_storage.hpp"
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/synchronization_object.hpp"
#include "lyra/runtime/tail.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_cell.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/weak_reference.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/laid_out_value.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unmanaged_ref.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

namespace {

using support::ObjectLayout;
using support::ValueDomain;

template <typename T>
constexpr auto Of() -> ObjectLayout {
  return ObjectLayout{
      .layout = {.size = sizeof(T), .align = alignof(T)},
      .needs_end = !std::is_trivially_destructible_v<T>};
}

// Each storage stated is exactly the least every class built there fits,
// which a layer compiled without the runtime sizes a frame holding one by: an
// object of a type the library declares, its declaration's, and one of a type
// the IR owns, the storage stated beside it.
consteval auto EveryClassTakesItsStatedStorage() -> bool {
  bool exact = true;
  ForEachDeclaredObjectClass([&]<typename Built, typename Written>(
                                 support::LibraryDecl decl,
                                 BuiltIn<Built, Written>) {
    const std::optional<ObjectLayout> stated = support::ObjectStorageOf(decl);
    exact = exact && (!stated.has_value() ||
                      StorageEnclosing<Built, Written>() == *stated);
  });
  ForEachStatedObjectClass(
      [&]<typename Built, typename Written>(
          const support::RuntimeObject& object, BuiltIn<Built, Written>) {
        exact = exact && support::StatedStorageOf(object) ==
                             StorageEnclosing<Built, Written>();
      });
  return exact;
}
static_assert(EveryClassTakesItsStatedStorage());

// What an object of the class the library's entries build for `decl`
// occupies.
auto ClassLayoutOf(support::LibraryDecl decl) -> ObjectLayout {
  std::optional<ObjectLayout> found;
  ForEachDeclaredObjectClass(
      [&]<typename Built, typename Written>(
          support::LibraryDecl paired, BuiltIn<Built, Written>) {
        if (paired == decl) {
          found = Of<Built>();
        }
      });
  if (!found.has_value()) {
    throw InternalError(
        "object layout: a declaration names no class the runtime defines -- "
        "please report this as a bug");
  }
  return *found;
}

// Hands `visit` each class of the runtime's a class of the source extends,
// with its declaration: an instance of the design hierarchy extends a scope
// (LRM 23.3), an object the program makes the root, and one of a class with a
// generator a class object (LRM 8.3, 18.14.1).
template <typename Visit>
constexpr void ForEachExtendedClass(const Visit& visit) {
  using support::LibraryDecl;
  visit.template operator()<Scope>(LibraryDecl::kScope);
  visit.template operator()<GcObject>(LibraryDecl::kObjectRoot);
  visit.template operator()<ClassObject>(LibraryDecl::kClassObject);
}

// Hands `visit` each declaration of a class of the runtime's, with that class:
// those a class of the source extends, and a process, an event's object and a
// randomization's problem, which the library alone builds (LRM 9.7, 6.17,
// 18.6).
template <typename Visit>
constexpr void ForEachRuntimeClass(const Visit& visit) {
  using support::LibraryDecl;
  ForEachExtendedClass(visit);
  visit.template operator()<RuntimeProcess>(LibraryDecl::kProcess);
  visit.template operator()<SynchronizationObject>(
      LibraryDecl::kSynchronizationObject);
  visit.template operator()<ConstraintProblem>(LibraryDecl::kConstraintProblem);
}

// Every declaration stating a class of the runtime's is listed with its class
// exactly once, and no other is.
consteval auto EveryRuntimeClassIsListedOnce() -> bool {
  bool listed_once = true;
  support::ForEachLibraryDecl([&](support::LibraryDecl decl) {
    std::size_t times = 0;
    ForEachRuntimeClass([&]<typename T>(support::LibraryDecl listed) {
      if (listed == decl) {
        ++times;
      }
    });
    const bool states_one =
        support::DeclarationOf(decl).runtime_class.has_value();
    listed_once = listed_once && times == (states_one ? 1U : 0U);
  });
  return listed_once;
}
static_assert(
    EveryRuntimeClassIsListedOnce(),
    "a declaration stating a class of the runtime's is listed with its class "
    "other than once, or one stating none is listed");

// Every backend reads an object's class from the object's first word, which a
// table pointer the host compiler gave a class of the library would take.
consteval auto NoRuntimeClassIsPolymorphic() -> bool {
  bool none = true;
  ForEachRuntimeClass([&]<typename T>(support::LibraryDecl) {
    none = none && !std::is_polymorphic_v<T>;
  });
  return none;
}
static_assert(NoRuntimeClassIsPolymorphic());

template <typename Answer>
[[noreturn]] auto NotRealized() -> Answer {
  throw InternalError(
      "object layout: this member storage is not realized over this value "
      "domain -- please report this as a bug");
}

// The class the library holds a value of `domain` as, handed to `f` as its
// template argument. Every storage below is one template over that class, so
// this is the one place a domain names its class, and what a storage admits is
// asked of the class it is handed.
template <typename F>
constexpr auto WithValueClass(ValueDomain domain, F f)
    -> decltype(f.template operator()<value::String>()) {
  switch (domain) {
    case ValueDomain::kBit8:
      return f.template operator()<value::BitVector<8>>();
    case ValueDomain::kBit16:
      return f.template operator()<value::BitVector<16>>();
    case ValueDomain::kBit32:
      return f.template operator()<value::BitVector<32>>();
    case ValueDomain::kBit64:
      return f.template operator()<value::BitVector<64>>();
    case ValueDomain::kLogic8:
      return f.template operator()<value::LogicVector<8>>();
    case ValueDomain::kLogic16:
      return f.template operator()<value::LogicVector<16>>();
    case ValueDomain::kLogic32:
      return f.template operator()<value::LogicVector<32>>();
    case ValueDomain::kLogic64:
      return f.template operator()<value::LogicVector<64>>();
    case ValueDomain::kBitWide:
      return f.template operator()<value::WideBitVector>();
    case ValueDomain::kLogicWide:
      return f.template operator()<value::WideLogicVector>();
    case ValueDomain::kWildcardIndex:
      return f.template operator()<value::AnyValue>();
    case ValueDomain::kAny:
      return f.template operator()<value::Any>();
    case ValueDomain::kString:
      return f.template operator()<value::String>();
    case ValueDomain::kBitVec:
      return f.template operator()<value::BitVec>();
    case ValueDomain::kReal:
      return f.template operator()<value::Real>();
    case ValueDomain::kShortReal:
      return f.template operator()<value::ShortReal>();
    case ValueDomain::kUnmanagedRef:
      return f.template operator()<value::UnmanagedRef>();
    case ValueDomain::kLaidOut:
      return f.template operator()<value::LaidOutValue>();
    case ValueDomain::kDynArray:
      return f.template operator()<value::RuntimeDynamicArray>();
    case ValueDomain::kQueue:
      return f.template operator()<value::RuntimeQueue>();
    case ValueDomain::kAssocArray:
      return f.template operator()<value::RuntimeAssociativeArray>();
    case ValueDomain::kManagedRef:
      return f.template operator()<value::ObjectRef>();
  }
  throw InternalError("object layout: unknown value domain");
}

// The class the library's entries build for a declaration whose values are
// objects of a value domain is the class every holder of that domain holds,
// so the two never disagree on what lies in the storage stated for it.
consteval auto EveryDomainObjectIsItsDomainsClass() -> bool {
  bool same = true;
  ForEachDeclaredObjectClass([&]<typename Built, typename Written>(
                                 support::LibraryDecl decl,
                                 BuiltIn<Built, Written>) {
    const auto* held = std::get_if<support::DomainObject>(
        &support::DeclarationOf(decl).holding);
    same = same &&
           (held == nullptr || WithValueClass(held->domain, []<typename T>() {
              return std::is_same_v<T, Built>;
            }));
  });
  return same;
}
static_assert(EveryDomainObjectIsItsDomainsClass());

// A variable a declaration names may be of every value class but an index
// held with its type, which is no type a declaration can name (LRM 7.8.1), and
// a run of bits, which is the stream a streaming operator makes and no
// declaration names either (LRM 11.4.14.1).
template <typename T>
struct AdmitsVariable : std::bool_constant<
                            !std::is_same_v<T, value::AnyValue> &&
                            !std::is_same_v<T, value::BitVec>> {};

// A local of a body -- held inline, in a cell, or among the body's variables --
// may be of every value class a variable may, and a run of bits
// besides, which a streaming operator keeps in a local while it packs or
// unpacks one (LRM 11.4.14).
template <typename T>
struct AdmitsLocal
    : std::bool_constant<
          AdmitsVariable<T>::value || std::is_same_v<T, value::BitVec>> {};

// A history is kept over every variable type but a reference owning nothing,
// whose value is the address it carries (LRM 6.14), which no sampled read can
// answer across.
template <typename T>
struct AdmitsHistory
    : std::bool_constant<
          AdmitsVariable<T>::value && !std::is_same_v<T, value::UnmanagedRef>> {
};

// A net is a declared net or an input port of no port kind (LRM 23.2.2.3), so
// it holds what such a port may be declared as: a value a net holds, but not a
// value of any type, which no declaration names, nor a reference owning
// nothing, which no port may be (LRM 6.14, 25.9).
template <typename T>
struct AdmitsNet : std::bool_constant<
                       AdmitsVariable<T>::value && NetValue<T> &&
                       !std::is_same_v<T, value::Any> &&
                       !std::is_same_v<T, value::UnmanagedRef>> {};

// `Storage` over the value class of `domain`, where `Admits` holds of that
// class.
template <template <typename> class Storage, template <typename> class Admits>
auto StorageOver(ValueDomain domain) -> ObjectLayout {
  return WithValueClass(domain, []<typename T>() -> ObjectLayout {
    if constexpr (Admits<T>::value) {
      return Of<Storage<T>>();
    } else {
      return NotRealized<ObjectLayout>();
    }
  });
}

// What a `Holder` of a value of `T` occupies: the holder, followed by the
// value `held` lays out where it lies inline at the holder's tail.
template <typename Holder, typename T>
auto HolderLayout(const std::optional<ObjectLayout>& held) -> ObjectLayout {
  if constexpr (DynamicallySized<T>) {
    if (!held.has_value()) {
      throw InternalError(
          "object layout: a holder keeping a value at its tail is laid out "
          "with no value to hold");
    }
    return ObjectLayout{
        .layout = HolderLayoutFor<Holder>(held->layout), .needs_end = true};
  } else {
    return Of<Holder>();
  }
}

// How far into a `Holder` the value it answers a read with lies.
template <typename Holder>
auto ReadAt() -> std::uint64_t {
  const Holder holder;
  return std::uint64_t{
      std::bit_cast<std::uintptr_t>(&holder.Get()) -
      std::bit_cast<std::uintptr_t>(&holder)};
}

}  // namespace

auto LayoutOf(ValueDomain domain) -> ObjectLayout {
  return WithValueClass(
      domain, []<typename T>() -> ObjectLayout { return Of<T>(); });
}

auto LayoutOf(support::LibraryObject object) -> ObjectLayout {
  std::optional<ObjectLayout> found;
  ForEachStatedObjectClass(
      [&]<typename Built, typename Written>(
          const support::RuntimeObject& paired, BuiltIn<Built, Written>) {
        if (paired == support::RuntimeObject{object}) {
          found = Of<Built>();
        }
      });
  if (!found.has_value()) {
    throw InternalError(
        "object layout: an object of the library's names no class the runtime "
        "defines -- please report this as a bug");
  }
  return *found;
}

auto LayoutOf(
    support::DeclaredMemberStorage storage, std::optional<ObjectLayout> held)
    -> ObjectLayout {
  return std::visit(
      Overloaded{
          [](const support::InlineValueMember& m) {
            return StorageOver<std::type_identity_t, AdmitsLocal>(m.domain);
          },
          [](const support::InlineBytesMember&) -> ObjectLayout {
            throw InternalError(
                "object layout: the library is asked the layout of storage "
                "generated code lays out itself -- please report this as a "
                "bug");
          },
          [&](const support::ValueCellMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> ObjectLayout {
              if constexpr (AdmitsLocal<T>::value) {
                return HolderLayout<typename ValueCellFor<T>::Type, T>(held);
              } else {
                return NotRealized<ObjectLayout>();
              }
            });
          },
          [&](const support::ObservableCellMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> ObjectLayout {
              if constexpr (AdmitsLocal<T>::value) {
                return HolderLayout<typename VariableOf<T>::Type, T>(held);
              } else {
                return NotRealized<ObjectLayout>();
              }
            });
          },
          [](const support::SampledHistoryMember& m) {
            return StorageOver<SampledHistory, AdmitsHistory>(m.domain);
          },
          [&](const support::ResolvedNetMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> ObjectLayout {
              if constexpr (AdmitsNet<T>::value) {
                return HolderLayout<ResolvedNet<T>, T>(held);
              } else {
                return NotRealized<ObjectLayout>();
              }
            });
          },
          [](const support::BorrowedHandleMember&) { return Of<void*>(); },
          [](const support::LibraryDeclMember& m) {
            return ClassLayoutOf(m.decl);
          },
          [](const support::LibraryObjectMember& m) {
            return LayoutOf(m.object);
          }},
      storage);
}

namespace {

// How far into a `Holder` the address `through` answers with lies, read off one
// built as the library builds it, with a tail for a value of layout `held`.
template <class Holder, class Through, class... Args>
auto ThroughOffset(support::Layout held, Through through, Args... args)
    -> std::uint64_t {
  const std::shared_ptr<Holder> built = MakeWithTail<Holder>(held, args...);
  return std::uint64_t{
      std::bit_cast<std::uintptr_t>(through(*built)) -
      std::bit_cast<std::uintptr_t>(built.get())};
}

// The same for a `Holder` keeping a value of `T` at its tail, built told some
// width or type: where the address lies does not depend on which.
template <class Holder, class T, class Through>
auto TailThroughOffset(Through through) -> std::uint64_t {
  using HeldAs = typename StorageFor<T>::Type;
  if constexpr (value::HeldAsWords<T>) {
    constexpr std::uint64_t kWidth = 65;
    return ThroughOffset<Holder>(
        HeldAs::ValueLayoutFor(kWidth), through, kWidth);
  } else {
    // A probe holds a bit, built at the bit's default, which no read sees.
    static value::ElementScratch scratch;
    static const value::ElementType held(
        value::IntegralValueType::Of<value::Bit>().Type(),
        [](void* out) -> void* {
          return std::construct_at(static_cast<value::Bit*>(out));
        },
        scratch);
    // A net is built holding how it folds values too, which a probe that
    // folds nothing hands as a table of no functions.
    if constexpr (std::constructible_from<
                      Holder, const value::ElementType&,
                      const value::NetFoldTable&>) {
      static constexpr value::NetFoldTable kFoldsNothing{};
      return ThroughOffset<Holder>(
          HeldAs::ValueLayoutFor(held), through, std::cref(held),
          std::cref(kFoldsNothing));
    } else {
      return ThroughOffset<Holder>(
          HeldAs::ValueLayoutFor(held), through, std::cref(held));
    }
  }
}

}  // namespace

// Storage holding no value of the program's holds no contents to reach.
auto HeldContentsOf(support::DeclaredMemberStorage storage)
    -> std::optional<HeldContents> {
  using Held = std::optional<HeldContents>;
  const auto through = [](std::uint64_t offset) {
    return HeldContents{ContentsIndirectAt{.offset = offset}};
  };
  const auto none = [](const auto&) -> Held { return std::nullopt; };
  return std::visit(
      Overloaded{
          [](const support::InlineValueMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> Held {
              if constexpr (HeldInOneWord<T>) {
                return ContentsAt{.offset = 0};
              } else {
                return std::nullopt;
              }
            });
          },
          // A net keeps the address of what it shows, its own value or the
          // object its one driver carries.
          [&](const support::ResolvedNetMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> Held {
              const auto shown = [](const auto& net) {
                return net.ShownThrough();
              };
              if constexpr (!AdmitsNet<T>::value) {
                return NotRealized<Held>();
              } else if constexpr (DynamicallySized<T>) {
                return through(TailThroughOffset<ResolvedNet<T>, T>(shown));
              } else {
                return through(
                    ThroughOffset<ResolvedNet<T>>(
                        support::Layout{.size = 0, .align = 1}, shown));
              }
            });
          },
          [&](const support::ObservableCellMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> Held {
              if constexpr (HeldInOneWord<T>) {
                return ContentsAt{.offset = ReadAt<Var<T>>()};
              } else if constexpr (DynamicallySized<T>) {
                return through(
                    TailThroughOffset<typename VariableOf<T>::Type, T>(
                        [](const auto& cell) {
                          return cell.HeldStorage().BytesThrough();
                        }));
              } else {
                return std::nullopt;
              }
            });
          },
          [&](const support::ValueCellMember& m) {
            return WithValueClass(m.domain, [&]<typename T>() -> Held {
              if constexpr (HeldInOneWord<T>) {
                return ContentsAt{.offset = ReadAt<ValueCell<T>>()};
              } else if constexpr (DynamicallySized<T>) {
                return through(
                    TailThroughOffset<typename ValueCellFor<T>::Type, T>(
                        [](const auto& local) {
                          return local.BytesThrough();
                        }));
              } else {
                return std::nullopt;
              }
            });
          },
          [&](const support::SampledHistoryMember& m) { return none(m); },
          [&](const support::InlineBytesMember& m) { return none(m); },
          [&](const support::BorrowedHandleMember& m) { return none(m); },
          [&](const support::LibraryDeclMember& m) { return none(m); },
          [&](const support::LibraryObjectMember& m) { return none(m); }},
      storage);
}

auto LibraryPartLayoutOf(support::LibraryDecl decl) -> ObjectLayout {
  std::optional<ObjectLayout> found;
  ForEachRuntimeClass([&]<typename T>(support::LibraryDecl listed) {
    if (listed == decl) {
      found = Of<T>();
    }
  });
  if (!found.has_value()) {
    throw InternalError(
        "object layout: a declaration states no class of the runtime's -- "
        "please report this as a bug");
  }
  return *found;
}

auto DefinitionLayoutOf() -> DefinitionLayout {
  return DefinitionLayout{
      .end = offsetof(abi::ObjectDefinition, end),
      .methods = offsetof(abi::ObjectDefinition, methods)};
}

auto ClosurePart() -> support::Layout {
  return {.size = sizeof(ClosureValue), .align = alignof(ClosureValue)};
}

namespace {

// Calls `f` with a pointer, at `at`, to the class of the runtime's `decl`
// states, which a value of a source class extends.
template <typename F>
void WithExtendedClass(support::LibraryDecl decl, void* at, F f) {
  bool found = false;
  ForEachExtendedClass([&]<typename T>(support::LibraryDecl listed) {
    if (listed == decl) {
      f(static_cast<T*>(at));
      found = true;
    }
  });
  if (!found) {
    throw InternalError(
        "object layout: a part a value extends is read as of a declaration "
        "stating no class a value of the source extends -- please report "
        "this as a bug");
  }
}

}  // namespace

void BeginLibraryPart(support::LibraryDecl decl, void* at) {
  WithExtendedClass(decl, at, [](auto* part) { std::construct_at(part); });
}

void EndLibraryPart(support::LibraryDecl decl, void* at) {
  WithExtendedClass(decl, at, [](auto* part) { std::destroy_at(part); });
}

}  // namespace lyra::runtime
