#include "lyra/support/data_object.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto NetTypeKeyword(NetType net_type) -> std::string_view {
  switch (net_type) {
    case NetType::kWire:
      return "wire";
    case NetType::kTri:
      return "tri";
    case NetType::kWand:
      return "wand";
    case NetType::kTriand:
      return "triand";
    case NetType::kWor:
      return "wor";
    case NetType::kTrior:
      return "trior";
    case NetType::kTri0:
      return "tri0";
    case NetType::kTri1:
      return "tri1";
    case NetType::kSupply0:
      return "supply0";
    case NetType::kSupply1:
      return "supply1";
    case NetType::kUwire:
      return "uwire";
    case NetType::kTrireg:
      return "trireg";
  }
  throw InternalError("NetTypeKeyword: unknown net type");
}

}  // namespace lyra::support
