#include "lyra/support/runtime_object.hpp"

#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

auto LibraryObjectName(LibraryObject object) -> std::string_view {
  switch (object) {
    case LibraryObject::kClosure:
      return "closure";
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
