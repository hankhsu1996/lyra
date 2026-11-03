#pragma once

#include <algorithm>
#include <cstddef>
#include <memory>
#include <optional>
#include <type_traits>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/any.hpp"
#include "lyra/runtime/assertion_control.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/loop_blocks.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/runtime/weak_reference.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/associative_array.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/dynamic_array.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unmanaged_ref.hpp"

namespace lyra::runtime {

// Whether an object of `T` fits the storage `stated` for it, as the compiler
// building this lays `T` out. MIR counts what a frame keeps by the storage
// stated, and on the C++ backend the host compiler lays out every class and
// every frame, so a class it lays out larger than stated would make a frame
// keep more than MIR counted; this header is compiled with every emitted unit,
// so such a host is refused where it compiles the program.
template <typename T>
constexpr auto FitsStatedStorage(const support::ObjectLayout& stated) -> bool {
  return sizeof(T) <= stated.layout.size && alignof(T) <= stated.layout.align &&
         (stated.needs_end || std::is_trivially_destructible_v<T>);
}

// The least storage every one of `T...` fits: as large and as aligned as the
// largest and the most aligned of them, and ended where any of them has
// anything to end.
template <typename... T>
constexpr auto StorageEnclosing() -> support::ObjectLayout {
  return support::ObjectLayout{
      .layout =
          {.size = std::max({sizeof(T)...}),
           .align = std::max({alignof(T)...})},
      .needs_end = (!std::is_trivially_destructible_v<T> || ...)};
}

// The classes built in one storage a declaration or the IR states: `Built`,
// which the library's own entries build there and the execution backend calls
// them to, and `Written`, which the C++ backend writes there. They are one
// class but where the C++ backend writes a class template over the type's
// arguments -- written here over a string, since such a class holds its
// elements elsewhere and is laid out alike whatever they are -- or a class of
// its own for the same value.
template <typename Built, typename Written = Built>
struct BuiltIn {};

// Hands `visit` each declaration of the library's whose object is laid out as
// a class: by value where a holder gives the object storage of its own, which
// the declaration states, and otherwise as a member of an object holding it
// whole. Every check of the storage a declaration states, and every layout
// read off the class a declaration stands for, reads this pairing.
template <typename Visit>
constexpr void ForEachDeclaredObjectClass(const Visit& visit) {
  using support::LibraryDecl;
  visit(LibraryDecl::kTrigger, BuiltIn<Trigger>{});
  visit(LibraryDecl::kObservation, BuiltIn<Observation>{});
  visit(LibraryDecl::kReadReport, BuiltIn<ReadReport>{});
  visit(LibraryDecl::kWait, BuiltIn<Wait>{});
  visit(LibraryDecl::kLeasedReference, BuiltIn<LeasedReference>{});
  visit(LibraryDecl::kHierarchySegment, BuiltIn<HierarchySegment>{});
  visit(LibraryDecl::kChannelCancellation, BuiltIn<ChannelCancellation>{});
  visit(LibraryDecl::kWeakReference, BuiltIn<WeakObjectRef>{});
  visit(LibraryDecl::kString, BuiltIn<value::String>{});
  visit(
      LibraryDecl::kDynamicArray,
      BuiltIn<
          value::RuntimeDynamicArray, value::DynamicArray<value::String>>{});
  visit(
      LibraryDecl::kQueue,
      BuiltIn<value::RuntimeQueue, value::Queue<value::String>>{});
  visit(
      LibraryDecl::kAssociativeArray,
      BuiltIn<
          value::RuntimeAssociativeArray,
          value::AssociativeArray<value::String, value::String>>{});
  visit(
      LibraryDecl::kWildcardIndex,
      BuiltIn<value::AnyValue, value::WildcardKey>{});
  visit(LibraryDecl::kAny, BuiltIn<value::Any>{});
  visit(LibraryDecl::kBitVec, BuiltIn<value::BitVec>{});
  visit(LibraryDecl::kPrintItem, BuiltIn<value::PrintItem>{});
  visit(LibraryDecl::kFormatSpec, BuiltIn<value::FormatSpec>{});
  visit(LibraryDecl::kFormatArg, BuiltIn<value::FormatArg>{});
  visit(LibraryDecl::kDpiBitBuffer, BuiltIn<value::DpiBitBuffer>{});
  visit(LibraryDecl::kDpiLogicBuffer, BuiltIn<value::DpiLogicBuffer>{});
  visit(LibraryDecl::kDpiOpenArray, BuiltIn<value::DpiOpenArray>{});
  visit(LibraryDecl::kCancellationTarget, BuiltIn<CancellationTarget>{});
  visit(LibraryDecl::kTimer, BuiltIn<Timer>{});
  visit(LibraryDecl::kEvaluationAttempts, BuiltIn<EvaluationAttempts>{});
  visit(LibraryDecl::kAssertionControl, BuiltIn<AssertionControl>{});
  visit(LibraryDecl::kLoopBlocks, BuiltIn<LoopBlocks>{});
}

// The same for each object of a type the IR owns that a holder gives storage
// of its own, with the object whose storage is stated for it.
template <typename Visit>
constexpr void ForEachStatedObjectClass(const Visit& visit) {
  using support::LibraryObject;
  visit(LibraryObject::kExecution, BuiltIn<Coroutine<void>>{});
  visit(
      LibraryObject::kReference,
      BuiltIn<ErasedReference, VariableRef<value::String>>{});
  visit(LibraryObject::kAnyReference, BuiltIn<AnyReference>{});
  visit(
      LibraryObject::kSharedPointer,
      BuiltIn<SharedPointer, std::shared_ptr<value::String>>{});
  visit(LibraryObject::kOpenWrite, BuiltIn<OpenWrite>{});
  visit(LibraryObject::kDesignation, BuiltIn<ErasedDesignation>{});
  visit(
      LibraryObject::kObjectWrite,
      BuiltIn<ErasedObjectWrite, ObjectWrite<GcObject>>{});
  visit(support::ValueDomain::kManagedRef, BuiltIn<value::ObjectRef>{});
}

// Every declaration laid out as a class -- one held by value, or as a member
// of an object holding it whole -- is paired with its classes exactly once,
// and no other is, so a declaration added without its classes fails to
// compile here rather than going unchecked against the storage it states.
consteval auto EveryClassIsPairedOnce() -> bool {
  bool paired_once = true;
  support::ForEachLibraryDecl([&](support::LibraryDecl decl) {
    std::size_t times = 0;
    ForEachDeclaredObjectClass([&](support::LibraryDecl paired, auto) {
      if (paired == decl) {
        ++times;
      }
    });
    const bool laid_out_as_a_class =
        !std::holds_alternative<support::StorageElsewhere>(
            support::DeclarationOf(decl).holding);
    paired_once = paired_once && times == (laid_out_as_a_class ? 1U : 0U);
  });
  return paired_once;
}
static_assert(
    EveryClassIsPairedOnce(),
    "a declaration of the library's laid out as a class is paired with its "
    "classes other than once, or one laid out otherwise is paired with some");

consteval auto EveryClassFitsItsStatedStorage() -> bool {
  bool fits = true;
  ForEachDeclaredObjectClass([&]<typename Built, typename Written>(
                                 support::LibraryDecl decl,
                                 BuiltIn<Built, Written>) {
    const std::optional<support::ObjectLayout> stated =
        support::ObjectStorageOf(decl);
    fits =
        fits && (!stated.has_value() || (FitsStatedStorage<Built>(*stated) &&
                                         FitsStatedStorage<Written>(*stated)));
  });
  ForEachStatedObjectClass([&]<typename Built, typename Written>(
                               const support::RuntimeObject& object,
                               BuiltIn<Built, Written>) {
    const std::optional<support::ObjectLayout> stated =
        support::StatedStorageOf(object);
    fits = fits && stated.has_value() && FitsStatedStorage<Built>(*stated) &&
           FitsStatedStorage<Written>(*stated);
  });
  return fits;
}
static_assert(
    EveryClassFitsItsStatedStorage(),
    "a class a backend builds is larger, more aligned or harder to end than "
    "the storage stated for it");

}  // namespace lyra::runtime
