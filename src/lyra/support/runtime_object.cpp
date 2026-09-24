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
    case LibraryObject::kDpiBitBuffer:
      return "dpi_bit_buffer";
    case LibraryObject::kDpiLogicBuffer:
      return "dpi_logic_buffer";
    case LibraryObject::kDpiOpenArray:
      return "dpi_open_array";
    case LibraryObject::kChannelCancellation:
      return "channel_cancellation";
    case LibraryObject::kErasedValue:
      return "erased_value";
    case LibraryObject::kExecution:
      return "execution";
    case LibraryObject::kPromotedScope:
      return "promoted_scope";
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
