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
    case ValueDomain::kTuple:
      return "tuple";
    case ValueDomain::kDynArray:
      return "dynarray";
    case ValueDomain::kUnpackedArray:
      return "unpackedarray";
    case ValueDomain::kQueue:
      return "queue";
    case ValueDomain::kAssocArray:
      return "assocarray";
  }
  throw InternalError("value domain: unknown domain");
}

}  // namespace lyra::support
