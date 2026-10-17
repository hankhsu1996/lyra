#include "lyra/runtime/object_layout.hpp"

#include <bit>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeinfo>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/loop_blocks.hpp"
#include "lyra/runtime/named_event.hpp"
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
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/tail.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_cell.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_fixed_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_record.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

namespace {

using support::MemberStorageKind;
using support::ObjectLayout;
using support::ValueDomain;

template <typename T>
constexpr auto Of() -> ObjectLayout {
  return ObjectLayout{
      .size = static_cast<std::uint32_t>(sizeof(T)),
      .align = static_cast<std::uint32_t>(alignof(T)),
      .ends_with_nothing_to_do = std::is_trivially_destructible_v<T>};
}

// Each object takes the storage stated for it, which a layer compiled without
// the runtime sizes a frame holding one by.
static_assert(Of<Trigger>() == support::kTriggerStorage);
static_assert(Of<Observation>() == support::kObservationStorage);
static_assert(Of<ReadReport>() == support::kReadReportStorage);
static_assert(Of<Wait>() == support::kWaitStorage);
static_assert(Of<HierarchySegment>() == support::kHierarchySegmentStorage);
static_assert(
    Of<ChannelCancellation>() == support::kChannelCancellationStorage);

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
auto WithValueClass(ValueDomain domain, F f)
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
    case ValueDomain::kString:
      return f.template operator()<value::String>();
    case ValueDomain::kReal:
      return f.template operator()<value::Real>();
    case ValueDomain::kShortReal:
      return f.template operator()<value::ShortReal>();
    case ValueDomain::kChandle:
      return f.template operator()<value::Chandle>();
    case ValueDomain::kEmpty:
      return f.template operator()<value::Empty>();
    case ValueDomain::kRecord:
      return f.template operator()<value::RuntimeRecord>();
    case ValueDomain::kDynArray:
      return f.template operator()<value::RuntimeDynamicArray>();
    case ValueDomain::kUnpackedArray:
      return f.template operator()<value::RuntimeFixedArray>();
    case ValueDomain::kQueue:
      return f.template operator()<value::RuntimeQueue>();
    case ValueDomain::kAssocArray:
      return f.template operator()<value::RuntimeAssociativeArray>();
    case ValueDomain::kManagedRef:
      return f.template operator()<value::ObjectRef>();
  }
  throw InternalError("object layout: unknown value domain");
}

// A variable may be of every value class but the empty one, which is only ever
// a tagged union's payload (LRM 7.3.2), and an index held with its type, which
// is no type a declaration can name (LRM 7.8.1).
template <typename T>
struct AdmitsVariable : std::bool_constant<
                            !std::is_same_v<T, value::Empty> &&
                            !std::is_same_v<T, value::AnyValue>> {};

// A history is kept over every variable type but the chandle, whose value is
// the pointer it carries (LRM 6.14), which no sampled read can answer across.
template <typename T>
struct AdmitsHistory
    : std::bool_constant<
          AdmitsVariable<T>::value && !std::is_same_v<T, value::Chandle>> {};

// A net resolves only what LRM 6.7.1 admits as a net's data type, which of the
// integral types is the four-state ones.
template <typename T>
struct AdmitsNet
    : std::bool_constant<AdmitsVariable<T>::value && value::NetResolvable<T>> {
};

template <BitAddressed T>
struct AdmitsNet<T> : std::bool_constant<T::kFourState> {};

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
    const value::Layout holder = HolderLayoutFor<Holder>(
        value::Layout{.size = held->size, .align = held->align});
    return ObjectLayout{
        .size = static_cast<std::uint32_t>(holder.size),
        .align = static_cast<std::uint32_t>(holder.align),
        .ends_with_nothing_to_do = false};
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
  switch (object) {
    case support::LibraryObject::kClosure:
      return Of<OwnedClosure>();
    case support::LibraryObject::kPrintItem:
      return Of<value::PrintItem>();
    case support::LibraryObject::kFormatSpec:
      return Of<value::FormatSpec>();
    case support::LibraryObject::kFormatArg:
      return Of<value::FormatArg>();
    case support::LibraryObject::kHierarchySegment:
      return support::kHierarchySegmentStorage;
    case support::LibraryObject::kTrigger:
      return support::kTriggerStorage;
    case support::LibraryObject::kObservation:
      return support::kObservationStorage;
    case support::LibraryObject::kReadReport:
      return support::kReadReportStorage;
    case support::LibraryObject::kWait:
      return support::kWaitStorage;
    case support::LibraryObject::kDpiBitBuffer:
      return Of<value::DpiBitBuffer>();
    case support::LibraryObject::kDpiLogicBuffer:
      return Of<value::DpiLogicBuffer>();
    case support::LibraryObject::kDpiOpenArray:
      return Of<value::DpiOpenArray>();
    case support::LibraryObject::kChannelCancellation:
      return support::kChannelCancellationStorage;
    case support::LibraryObject::kExecution:
      return Of<Coroutine<void>>();
    case support::LibraryObject::kSharedPointer:
      return Of<SharedPointer>();
    case support::LibraryObject::kOpenWrite:
      return Of<OpenWrite>();
    case support::LibraryObject::kDesignation:
      return Of<ErasedDesignation>();
    case support::LibraryObject::kObjectWrite:
      return Of<ErasedObjectWrite>();
    case support::LibraryObject::kReference:
      return Of<ErasedReference>();
  }
  throw InternalError("object layout: unknown library object");
}

auto LayoutOf(const support::RuntimeObject& object) -> ObjectLayout {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) { return LayoutOf(domain); },
          [](support::LibraryObject library) { return LayoutOf(library); }},
      object);
}

auto LayoutOf(
    support::DeclaredMemberStorage storage, std::optional<ObjectLayout> held)
    -> ObjectLayout {
  switch (storage.kind) {
    case MemberStorageKind::kInlineValue:
      return StorageOver<std::type_identity_t, AdmitsVariable>(storage.domain);
    case MemberStorageKind::kValueCell:
      return WithValueClass(storage.domain, [&]<typename T>() -> ObjectLayout {
        if constexpr (AdmitsVariable<T>::value) {
          return HolderLayout<typename LocalOf<T>::Type, T>(held);
        } else {
          return NotRealized<ObjectLayout>();
        }
      });
    case MemberStorageKind::kObservableCell:
      return WithValueClass(storage.domain, [&]<typename T>() -> ObjectLayout {
        if constexpr (AdmitsVariable<T>::value) {
          return HolderLayout<typename VariableOf<T>::Type, T>(held);
        } else {
          return NotRealized<ObjectLayout>();
        }
      });
    case MemberStorageKind::kSampledHistory:
      return StorageOver<SampledHistory, AdmitsHistory>(storage.domain);
    case MemberStorageKind::kResolvedNet:
      return WithValueClass(storage.domain, [&]<typename T>() -> ObjectLayout {
        if constexpr (AdmitsNet<T>::value) {
          return HolderLayout<ResolvedNet<T>, T>(held);
        } else {
          return NotRealized<ObjectLayout>();
        }
      });
    case MemberStorageKind::kBorrowedHandle:
      return Of<void*>();
    case MemberStorageKind::kReference:
      return Of<ErasedReference>();
    case MemberStorageKind::kSharedPointer:
      return Of<SharedPointer>();
    case MemberStorageKind::kChannelCancellation:
      return support::kChannelCancellationStorage;
    case MemberStorageKind::kNamedEvent:
      return Of<NamedEvent>();
    case MemberStorageKind::kCancellationTarget:
      return Of<CancellationTarget>();
    case MemberStorageKind::kEvaluationAttempts:
      return Of<EvaluationAttempts>();
    case MemberStorageKind::kTimer:
      return Of<Timer>();
    case MemberStorageKind::kLoopBlocks:
      return Of<LoopBlocks>();
  }
  throw InternalError("object layout: unknown member storage kind");
}

namespace {

// How far into a `Holder` the address `through` answers with lies, read off one
// built as the library builds it, with a tail for a value of layout `held`.
template <class Holder, class Through, class... Args>
auto ThroughOffset(value::Layout held, Through through, Args... args)
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
    const value::ValueType& type =
        value::IntegralValueType::Of<value::Bit>().Type();
    return ThroughOffset<Holder>(
        HeldAs::ValueLayoutFor(type), through, std::cref(type));
  }
}

}  // namespace

auto HeldContentsOf(support::DeclaredMemberStorage storage)
    -> std::optional<HeldContents> {
  return WithValueClass(
      storage.domain, [&]<typename T>() -> std::optional<HeldContents> {
        const auto through = [](std::uint64_t offset) {
          return HeldContents{ContentsIndirectAt{.offset = offset}};
        };
        const auto shown = [](const auto& net) { return net.ShownThrough(); };
        switch (storage.kind) {
          case MemberStorageKind::kInlineValue:
            if constexpr (HeldInOneWord<T>) {
              return ContentsAt{.offset = 0};
            } else {
              return std::nullopt;
            }
          // A net keeps the address of what it shows, its own value or the
          // object its one driver carries.
          case MemberStorageKind::kResolvedNet:
            if constexpr (!BitAddressed<T> && !value::LaidOutByType<T>) {
              return std::nullopt;
            } else if constexpr (!AdmitsNet<T>::value) {
              return NotRealized<std::optional<HeldContents>>();
            } else if constexpr (DynamicallySized<T>) {
              return through(TailThroughOffset<ResolvedNet<T>, T>(shown));
            } else {
              return through(
                  ThroughOffset<ResolvedNet<T>>(
                      value::Layout{.size = 0, .align = 1}, shown));
            }
          case MemberStorageKind::kObservableCell:
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
          case MemberStorageKind::kValueCell:
            if constexpr (HeldInOneWord<T>) {
              return ContentsAt{.offset = ReadAt<ValueCell<T>>()};
            } else if constexpr (DynamicallySized<T>) {
              return through(
                  TailThroughOffset<typename LocalOf<T>::Type, T>(
                      [](const auto& local) { return local.BytesThrough(); }));
            } else {
              return std::nullopt;
            }
          case MemberStorageKind::kSampledHistory:
          case MemberStorageKind::kBorrowedHandle:
          case MemberStorageKind::kReference:
          case MemberStorageKind::kSharedPointer:
          case MemberStorageKind::kChannelCancellation:
          case MemberStorageKind::kNamedEvent:
          case MemberStorageKind::kCancellationTarget:
          case MemberStorageKind::kEvaluationAttempts:
            return std::nullopt;
        }
        throw InternalError("object layout: unknown member storage kind");
      });
}

auto LayoutOf(support::RuntimeClass klass) -> ObjectLayout {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return Of<Scope>();
    case support::RuntimeClass::kObject:
      return Of<GcObject>();
    case support::RuntimeClass::kProcess:
      return Of<RuntimeProcess>();
  }
  throw InternalError("object layout: unknown runtime class");
}

auto ClosureCapturesAt() -> std::uint64_t {
  return sizeof(ClosureValue);
}

// The Itanium ABI names a class's type information `_ZTI` followed by the
// class's mangled name, which is what its `type_info` reports as its name.
auto TypeInfoSymbolOf(support::RuntimeClass klass) -> std::string {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return std::string("_ZTI") + typeid(Scope).name();
    case support::RuntimeClass::kObject:
      return std::string("_ZTI") + typeid(GcObject).name();
    case support::RuntimeClass::kProcess:
      throw InternalError(
          "object layout: a process is read as a class a value extends -- "
          "please report this as a bug");
  }
  throw InternalError("object layout: unknown runtime class");
}

// The Itanium mangling of the constructors and destructors below is written
// out, since neither has an address the language lets a program take. A wrong
// one fails every program's link, since every class a design declares extends
// one of these and calls both.
auto BaseObjectConstructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "_ZN4lyra7runtime5ScopeC2EPS1_NS0_16HierarchySegmentEPKNS0_"
             "16ObjectDefinitionE";
    case support::RuntimeClass::kObject:
      return "_ZN4lyra7runtime8GcObjectC2Ev";
    case support::RuntimeClass::kProcess:
      break;
  }
  throw InternalError(
      "object layout: a process is read as a class a value extends -- please "
      "report this as a bug");
}

auto BaseObjectDestructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "_ZN4lyra7runtime5ScopeD2Ev";
    case support::RuntimeClass::kObject:
      return "_ZN4lyra7runtime8GcObjectD2Ev";
    case support::RuntimeClass::kProcess:
      break;
  }
  throw InternalError(
      "object layout: a process is read as a class a value extends -- please "
      "report this as a bug");
}

// The Itanium mangling of each member function of `lyra::runtime::Scope`. A
// pointer to a member function names no symbol, so these are written out; a
// wrong one fails every program's link, since every class extending the scope
// that does not override one names it.
auto VirtualFunctionSymbolOf(support::LibraryVirtual function)
    -> std::string_view {
  switch (function) {
    case support::LibraryVirtual::kScopeResolve:
      return "_ZN4lyra7runtime5Scope10sv_resolveEv";
    case support::LibraryVirtual::kScopeInitialize:
      return "_ZN4lyra7runtime5Scope13sv_initializeEv";
    case support::LibraryVirtual::kScopeCreateProcesses:
      return "_ZN4lyra7runtime5Scope19sv_create_processesEv";
  }
  throw InternalError("object layout: unknown library virtual");
}

}  // namespace lyra::runtime
