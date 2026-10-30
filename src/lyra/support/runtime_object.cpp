#include "lyra/support/runtime_object.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

auto StatedStorageOf(const RuntimeObject& object)
    -> std::optional<ObjectLayout> {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) -> std::optional<ObjectLayout> {
            switch (domain) {
              case ValueDomain::kWildcardIndex:
                return kWildcardKeyStorage;
              case ValueDomain::kAny:
                return kAnyStorage;
              case ValueDomain::kString:
                return kStringStorage;
              case ValueDomain::kBitVec:
                return kBitVecStorage;
              case ValueDomain::kReal:
                return kRealStorage;
              case ValueDomain::kShortReal:
                return kShortRealStorage;
              case ValueDomain::kChandle:
                return kChandleStorage;
              case ValueDomain::kEmpty:
                return kEmptyStorage;
              case ValueDomain::kDynArray:
                return kDynamicArrayStorage;
              case ValueDomain::kQueue:
                return kQueueStorage;
              case ValueDomain::kAssocArray:
                return kAssociativeArrayStorage;
              case ValueDomain::kManagedRef:
                return kObjectRefStorage;
              // An integral value is held as its bits, and a value laid out
              // by its type as the bytes its type lays out, so neither is an
              // object of the library's here.
              case ValueDomain::kBit8:
              case ValueDomain::kBit16:
              case ValueDomain::kBit32:
              case ValueDomain::kBit64:
              case ValueDomain::kLogic8:
              case ValueDomain::kLogic16:
              case ValueDomain::kLogic32:
              case ValueDomain::kLogic64:
              case ValueDomain::kBitWide:
              case ValueDomain::kLogicWide:
              case ValueDomain::kLaidOut:
                return std::nullopt;
            }
            throw InternalError("runtime object: unknown value domain");
          },
          [](LibraryObject library) -> std::optional<ObjectLayout> {
            switch (library) {
              case LibraryObject::kPrintItem:
                return kPrintItemStorage;
              case LibraryObject::kFormatSpec:
                return kFormatSpecStorage;
              case LibraryObject::kFormatArg:
                return kFormatArgStorage;
              case LibraryObject::kHierarchySegment:
                return kHierarchySegmentStorage;
              case LibraryObject::kTrigger:
                return kTriggerStorage;
              case LibraryObject::kObservation:
                return kObservationStorage;
              case LibraryObject::kReadReport:
                return kReadReportStorage;
              case LibraryObject::kWait:
                return kWaitStorage;
              case LibraryObject::kDpiBitBuffer:
                return kDpiBitBufferStorage;
              case LibraryObject::kDpiLogicBuffer:
                return kDpiLogicBufferStorage;
              case LibraryObject::kDpiOpenArray:
                return kDpiOpenArrayStorage;
              case LibraryObject::kChannelCancellation:
                return kChannelCancellationStorage;
              case LibraryObject::kSharedPointer:
                return kSharedPointerStorage;
              case LibraryObject::kOpenWrite:
                return kOpenWriteStorage;
              case LibraryObject::kDesignation:
                return kDesignationStorage;
              case LibraryObject::kObjectWrite:
                return kObjectWriteStorage;
              case LibraryObject::kReference:
                return kReferenceStorage;
              case LibraryObject::kAnyReference:
                return kAnyReferenceStorage;
              case LibraryObject::kWeakReference:
                return kWeakReferenceStorage;
              case LibraryObject::kLeasedReference:
                return kLeasedReferenceStorage;
              // An execution is taken by whoever drives it the moment it is
              // made, so nothing holds one to end.
              case LibraryObject::kExecution:
                return std::nullopt;
            }
            throw InternalError("runtime object: unknown library object");
          }},
      object);
}

auto LibraryObjectName(LibraryObject object) -> std::string_view {
  switch (object) {
    case LibraryObject::kPrintItem:
      return "print_item";
    case LibraryObject::kFormatSpec:
      return "format_spec";
    case LibraryObject::kFormatArg:
      return "format_arg";
    case LibraryObject::kHierarchySegment:
      return "hierarchy_segment";
    case LibraryObject::kTrigger:
      return "trigger";
    case LibraryObject::kObservation:
      return "observation";
    case LibraryObject::kReadReport:
      return "read_report";
    case LibraryObject::kWait:
      return "wait";
    case LibraryObject::kDpiBitBuffer:
      return "dpi_bit_buffer";
    case LibraryObject::kDpiLogicBuffer:
      return "dpi_logic_buffer";
    case LibraryObject::kDpiOpenArray:
      return "dpi_open_array";
    case LibraryObject::kChannelCancellation:
      return "channel_cancellation";
    case LibraryObject::kExecution:
      return "execution";
    case LibraryObject::kSharedPointer:
      return "shared_pointer";
    case LibraryObject::kOpenWrite:
      return "open_write";
    case LibraryObject::kDesignation:
      return "designation";
    case LibraryObject::kObjectWrite:
      return "object_write";
    case LibraryObject::kReference:
      return "reference";
    case LibraryObject::kAnyReference:
      return "any_reference";
    case LibraryObject::kWeakReference:
      return "weak_reference";
    case LibraryObject::kLeasedReference:
      return "leased_reference";
  }
  throw InternalError("runtime object: unknown library object");
}

auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) { return ValueDomainName(domain); },
          [](LibraryObject library) { return LibraryObjectName(library); }},
      object);
}

}  // namespace lyra::support
