#include "lyra/support/value_domain.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto ValueDomainName(ValueDomain domain) -> std::string_view {
  switch (domain) {
    case ValueDomain::kBit8:
      return "bit8";
    case ValueDomain::kBit16:
      return "bit16";
    case ValueDomain::kBit32:
      return "bit32";
    case ValueDomain::kBit64:
      return "bit64";
    case ValueDomain::kLogic8:
      return "logic8";
    case ValueDomain::kLogic16:
      return "logic16";
    case ValueDomain::kLogic32:
      return "logic32";
    case ValueDomain::kLogic64:
      return "logic64";
    case ValueDomain::kBitWide:
      return "bit_wide";
    case ValueDomain::kLogicWide:
      return "logic_wide";
    case ValueDomain::kWildcardIndex:
      return "wildcard_index";
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
    case ValueDomain::kRecord:
      return "record";
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

auto IsIntegralLayout(ValueDomain domain) -> bool {
  switch (domain) {
    case ValueDomain::kBit8:
    case ValueDomain::kBit16:
    case ValueDomain::kBit32:
    case ValueDomain::kBit64:
    case ValueDomain::kLogic8:
    case ValueDomain::kLogic16:
    case ValueDomain::kLogic32:
    case ValueDomain::kLogic64:
      return true;
    case ValueDomain::kBitWide:
    case ValueDomain::kLogicWide:
    case ValueDomain::kWildcardIndex:
    case ValueDomain::kString:
    case ValueDomain::kReal:
    case ValueDomain::kShortReal:
    case ValueDomain::kChandle:
    case ValueDomain::kEmpty:
    case ValueDomain::kRecord:
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
