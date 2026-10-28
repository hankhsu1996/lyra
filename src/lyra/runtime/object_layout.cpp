#include "lyra/runtime/object_layout.hpp"

#include <bit>
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
#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
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
      .layout = {.size = sizeof(T), .align = alignof(T)},
      .ends_with_nothing_to_do = std::is_trivially_destructible_v<T>};
}

// Each object takes the storage stated for it, which a layer compiled without
// the runtime sizes a frame holding one by.
static_assert(Of<Trigger>() == support::kTriggerStorage);
static_assert(Of<Observation>() == support::kObservationStorage);
static_assert(Of<ReadReport>() == support::kReadReportStorage);
static_assert(Of<Wait>() == support::kWaitStorage);
static_assert(Of<LeasedReference>() == support::kLeasedReferenceStorage);
static_assert(Of<HierarchySegment>() == support::kHierarchySegmentStorage);
static_assert(
    Of<ChannelCancellation>() == support::kChannelCancellationStorage);
static_assert(Of<WeakObjectRef>() == support::kWeakReferenceStorage);
static_assert(Of<ErasedReference>() == support::kReferenceStorage);
static_assert(Of<AnyReference>() == support::kAnyReferenceStorage);
static_assert(Of<SharedPointer>() == support::kSharedPointerStorage);
static_assert(Of<OpenWrite>() == support::kOpenWriteStorage);
static_assert(Of<ErasedDesignation>() == support::kDesignationStorage);
static_assert(Of<ErasedObjectWrite>() == support::kObjectWriteStorage);
static_assert(Of<value::String>() == support::kStringStorage);
static_assert(Of<value::Real>() == support::kRealStorage);
static_assert(Of<value::ShortReal>() == support::kShortRealStorage);
static_assert(Of<value::Chandle>() == support::kChandleStorage);
static_assert(Of<value::ObjectRef>() == support::kObjectRefStorage);
static_assert(
    Of<value::RuntimeDynamicArray>() == support::kDynamicArrayStorage);
static_assert(Of<value::RuntimeQueue>() == support::kQueueStorage);
static_assert(
    Of<value::RuntimeAssociativeArray>() == support::kAssociativeArrayStorage);
static_assert(Of<value::WildcardKey>() == support::kWildcardKeyStorage);
static_assert(Of<value::Any>() == support::kAnyStorage);
static_assert(Of<value::Empty>() == support::kEmptyStorage);
static_assert(Of<value::BitVec>() == support::kBitVecStorage);
static_assert(Of<value::PrintItem>() == support::kPrintItemStorage);
static_assert(
    Of<value::PrintLiteralItem>() == support::kPrintLiteralItemStorage);
static_assert(Of<value::PrintValueItem>() == support::kPrintValueItemStorage);
static_assert(Of<value::FormatSpec>() == support::kFormatSpecStorage);
static_assert(Of<value::FormatArg>() == support::kFormatArgStorage);
static_assert(Of<value::TimeFormat>() == support::kTimeFormatStorage);
static_assert(Of<value::DpiBitBuffer>() == support::kDpiBitBufferStorage);
static_assert(Of<value::DpiLogicBuffer>() == support::kDpiLogicBufferStorage);
static_assert(Of<value::DpiOpenArray>() == support::kDpiOpenArrayStorage);

// Every backend reads an object's class from the object's first word, which a
// table pointer the host compiler gave a class of the library would take.
static_assert(!std::is_polymorphic_v<GcObject>);
static_assert(!std::is_polymorphic_v<ClassObject>);
static_assert(!std::is_polymorphic_v<RuntimeProcess>);
static_assert(!std::is_polymorphic_v<Scope>);
static_assert(!std::is_polymorphic_v<SynchronizationObject>);
static_assert(!std::is_polymorphic_v<ConstraintProblem>);

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
    case ValueDomain::kChandle:
      return f.template operator()<value::Chandle>();
    case ValueDomain::kEmpty:
      return f.template operator()<value::Empty>();
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

// A variable a declaration names may be of every value class but the empty
// one, which is only ever a tagged union's payload (LRM 7.3.2), an index held
// with its type, which is no type a declaration can name (LRM 7.8.1), and a
// run of bits, which is the stream a streaming operator makes and no
// declaration names either (LRM 11.4.14.1).
template <typename T>
struct AdmitsVariable : std::bool_constant<
                            !std::is_same_v<T, value::Empty> &&
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
    return ObjectLayout{
        .layout = HolderLayoutFor<Holder>(held->layout),
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
      return support::kSharedPointerStorage;
    case support::LibraryObject::kOpenWrite:
      return support::kOpenWriteStorage;
    case support::LibraryObject::kDesignation:
      return support::kDesignationStorage;
    case support::LibraryObject::kObjectWrite:
      return support::kObjectWriteStorage;
    case support::LibraryObject::kReference:
      return support::kReferenceStorage;
    case support::LibraryObject::kAnyReference:
      return support::kAnyReferenceStorage;
    case support::LibraryObject::kWeakReference:
      return support::kWeakReferenceStorage;
    case support::LibraryObject::kLeasedReference:
      return support::kLeasedReferenceStorage;
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
      return StorageOver<std::type_identity_t, AdmitsLocal>(storage.domain);
    case MemberStorageKind::kValueCell:
      return WithValueClass(storage.domain, [&]<typename T>() -> ObjectLayout {
        if constexpr (AdmitsLocal<T>::value) {
          return HolderLayout<typename LocalOf<T>::Type, T>(held);
        } else {
          return NotRealized<ObjectLayout>();
        }
      });
    case MemberStorageKind::kObservableCell:
      return WithValueClass(storage.domain, [&]<typename T>() -> ObjectLayout {
        if constexpr (AdmitsLocal<T>::value) {
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
    case MemberStorageKind::kAnyReference:
      return Of<AnyReference>();
    case MemberStorageKind::kWeakReference:
      return support::kWeakReferenceStorage;
    case MemberStorageKind::kSharedPointer:
      return Of<SharedPointer>();
    case MemberStorageKind::kChannelCancellation:
      return support::kChannelCancellationStorage;
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
                      support::Layout{.size = 0, .align = 1}, shown));
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
          case MemberStorageKind::kAnyReference:
          case MemberStorageKind::kWeakReference:
          case MemberStorageKind::kSharedPointer:
          case MemberStorageKind::kChannelCancellation:
          case MemberStorageKind::kCancellationTarget:
          case MemberStorageKind::kEvaluationAttempts:
          case MemberStorageKind::kTimer:
          case MemberStorageKind::kLoopBlocks:
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
    case support::RuntimeClass::kClassObject:
      return Of<ClassObject>();
    case support::RuntimeClass::kProcess:
      return Of<RuntimeProcess>();
    case support::RuntimeClass::kSynchronizationObject:
      return Of<SynchronizationObject>();
    case support::RuntimeClass::kConstraintProblem:
      return Of<ConstraintProblem>();
  }
  throw InternalError("object layout: unknown runtime class");
}

auto DefinitionLayoutOf() -> DefinitionLayout {
  return DefinitionLayout{
      .end = offsetof(ObjectDefinition, end),
      .methods = offsetof(ObjectDefinition, methods)};
}

auto ClosurePart() -> support::Layout {
  return {.size = sizeof(ClosureValue), .align = alignof(ClosureValue)};
}

namespace {

// Calls `f` with a pointer, at `at`, to the class of the library `klass` is, as
// its template argument: one a value of a source class extends.
template <typename F>
void WithExtendedClass(support::RuntimeClass klass, void* at, F f) {
  switch (klass) {
    case support::RuntimeClass::kScope:
      f(static_cast<Scope*>(at));
      return;
    case support::RuntimeClass::kObject:
      f(static_cast<GcObject*>(at));
      return;
    case support::RuntimeClass::kClassObject:
      f(static_cast<ClassObject*>(at));
      return;
    case support::RuntimeClass::kProcess:
    case support::RuntimeClass::kSynchronizationObject:
    case support::RuntimeClass::kConstraintProblem:
      throw InternalError(
          "object layout: a process, an event's object or a randomization's "
          "problem is read as a class a value extends -- please report this "
          "as a bug");
  }
  throw InternalError("object layout: unknown runtime class");
}

}  // namespace

void BeginLibraryPart(support::RuntimeClass klass, void* at) {
  WithExtendedClass(klass, at, [](auto* part) { std::construct_at(part); });
}

void EndLibraryPart(support::RuntimeClass klass, void* at) {
  WithExtendedClass(klass, at, [](auto* part) { std::destroy_at(part); });
}

}  // namespace lyra::runtime
