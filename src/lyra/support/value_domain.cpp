#include "lyra/support/value_domain.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ValueDomainName(ValueDomain domain) -> std::string_view {
  switch (domain) {
    case ValueDomain::kPacked:
      return "packed";
    case ValueDomain::kString:
      return "string";
    case ValueDomain::kReal:
      return "real";
    case ValueDomain::kShortReal:
      return "shortreal";
    case ValueDomain::kChandle:
      return "chandle";
    case ValueDomain::kEmpty:
      return "empty";
    case ValueDomain::kTuple:
      return "tuple";
    case ValueDomain::kUnion:
      return "union";
    case ValueDomain::kTaggedUnion:
      return "tagged_union";
    case ValueDomain::kDynArray:
      return "dynarray";
    case ValueDomain::kUnpackedArray:
      return "unpackedarray";
    case ValueDomain::kQueue:
      return "queue";
    case ValueDomain::kAssocArray:
      return "assocarray";
    case ValueDomain::kManagedRef:
      return "managedref";
  }
  throw InternalError("value domain: unknown domain");
}

auto ValueDomainIsItsOwnHandle(ValueDomain domain) -> bool {
  switch (domain) {
    case ValueDomain::kChandle:
      return true;
    case ValueDomain::kPacked:
    case ValueDomain::kString:
    case ValueDomain::kReal:
    case ValueDomain::kShortReal:
    case ValueDomain::kEmpty:
    case ValueDomain::kTuple:
    case ValueDomain::kUnion:
    case ValueDomain::kTaggedUnion:
    case ValueDomain::kDynArray:
    case ValueDomain::kUnpackedArray:
    case ValueDomain::kQueue:
    case ValueDomain::kAssocArray:
    case ValueDomain::kManagedRef:
      return false;
  }
  throw InternalError("value domain: unknown domain");
}

}  // namespace lyra::support
