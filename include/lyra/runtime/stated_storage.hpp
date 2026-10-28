#pragma once

#include <type_traits>

#include "lyra/runtime/any.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/runtime/weak_reference.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"

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
         (!stated.ends_with_nothing_to_do ||
          std::is_trivially_destructible_v<T>);
}

static_assert(FitsStatedStorage<Trigger>(support::kTriggerStorage));
static_assert(FitsStatedStorage<Observation>(support::kObservationStorage));
static_assert(FitsStatedStorage<ReadReport>(support::kReadReportStorage));
static_assert(FitsStatedStorage<Wait>(support::kWaitStorage));
static_assert(
    FitsStatedStorage<LeasedReference>(support::kLeasedReferenceStorage));
static_assert(
    FitsStatedStorage<HierarchySegment>(support::kHierarchySegmentStorage));
static_assert(FitsStatedStorage<ChannelCancellation>(
    support::kChannelCancellationStorage));
static_assert(FitsStatedStorage<WeakObjectRef>(support::kWeakReferenceStorage));
static_assert(FitsStatedStorage<ErasedReference>(support::kReferenceStorage));
static_assert(FitsStatedStorage<AnyReference>(support::kAnyReferenceStorage));
static_assert(FitsStatedStorage<SharedPointer>(support::kSharedPointerStorage));
static_assert(FitsStatedStorage<OpenWrite>(support::kOpenWriteStorage));
static_assert(
    FitsStatedStorage<ErasedDesignation>(support::kDesignationStorage));
static_assert(
    FitsStatedStorage<ErasedObjectWrite>(support::kObjectWriteStorage));
static_assert(FitsStatedStorage<value::String>(support::kStringStorage));
static_assert(FitsStatedStorage<value::Real>(support::kRealStorage));
static_assert(FitsStatedStorage<value::ShortReal>(support::kShortRealStorage));
static_assert(FitsStatedStorage<value::Chandle>(support::kChandleStorage));
static_assert(FitsStatedStorage<value::ObjectRef>(support::kObjectRefStorage));
static_assert(FitsStatedStorage<value::RuntimeDynamicArray>(
    support::kDynamicArrayStorage));
static_assert(FitsStatedStorage<value::RuntimeQueue>(support::kQueueStorage));
static_assert(FitsStatedStorage<value::RuntimeAssociativeArray>(
    support::kAssociativeArrayStorage));
static_assert(
    FitsStatedStorage<value::WildcardKey>(support::kWildcardKeyStorage));
static_assert(FitsStatedStorage<value::Any>(support::kAnyStorage));
static_assert(FitsStatedStorage<value::Empty>(support::kEmptyStorage));
static_assert(FitsStatedStorage<value::BitVec>(support::kBitVecStorage));
static_assert(FitsStatedStorage<value::PrintItem>(support::kPrintItemStorage));
static_assert(FitsStatedStorage<value::PrintLiteralItem>(
    support::kPrintLiteralItemStorage));
static_assert(
    FitsStatedStorage<value::PrintValueItem>(support::kPrintValueItemStorage));
static_assert(
    FitsStatedStorage<value::FormatSpec>(support::kFormatSpecStorage));
static_assert(FitsStatedStorage<value::FormatArg>(support::kFormatArgStorage));
static_assert(
    FitsStatedStorage<value::TimeFormat>(support::kTimeFormatStorage));
static_assert(
    FitsStatedStorage<value::DpiBitBuffer>(support::kDpiBitBufferStorage));
static_assert(
    FitsStatedStorage<value::DpiLogicBuffer>(support::kDpiLogicBufferStorage));
static_assert(
    FitsStatedStorage<value::DpiOpenArray>(support::kDpiOpenArrayStorage));

}  // namespace lyra::runtime
