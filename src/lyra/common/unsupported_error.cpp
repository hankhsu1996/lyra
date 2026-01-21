#include "lyra/common/unsupported_error.hpp"

#include <utility>

namespace lyra::common {

UnsupportedErrorException::UnsupportedErrorException(UnsupportedError error)
    : error_(std::move(error)) {
}

UnsupportedErrorException::UnsupportedErrorException(
    UnsupportedLayer layer, UnsupportedKind kind, OriginId origin,
    std::string detail)
    : error_{layer, kind, origin, std::move(detail)} {
}

auto ToString(UnsupportedLayer layer) -> const char* {
  switch (layer) {
    case UnsupportedLayer::kAstToHir:
      return "AstToHir";
    case UnsupportedLayer::kHirToMir:
      return "HirToMir";
    case UnsupportedLayer::kMirToLlvm:
      return "MirToLlvm";
    case UnsupportedLayer::kExecution:
      return "Execution";
  }
  return "Unknown";
}

auto ToString(UnsupportedKind kind) -> const char* {
  switch (kind) {
    case UnsupportedKind::kType:
      return "Type";
    case UnsupportedKind::kOperation:
      return "Operation";
    case UnsupportedKind::kFeature:
      return "Feature";
  }
  return "Unknown";
}

}  // namespace lyra::common
