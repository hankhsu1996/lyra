#include "lyra/support/library_decl.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <span>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/movability.hpp"

namespace lyra::support {

auto IsRuntimeStoredValue(LibraryDecl decl) -> bool {
  return std::holds_alternative<DomainObject>(DeclarationOf(decl).holding);
}

auto IsAddressOnly(LibraryDecl decl) -> bool {
  if (ObjectStorageOf(decl).has_value()) {
    return false;
  }
  switch (DeclarationOf(decl).movability) {
    case Movability::kImmovable:
      return true;
    case Movability::kCopyable:
    case Movability::kMovable:
      return false;
  }
  throw InternalError("support: unknown movability");
}

auto ParamOf(LibraryDecl decl, LibraryParam param)
    -> std::optional<std::size_t> {
  const std::span<const LibraryParam> params = DeclarationOf(decl).params;
  const auto found = std::ranges::find(params, param);
  if (found == params.end()) {
    return std::nullopt;
  }
  return static_cast<std::size_t>(found - params.begin());
}

auto TakesAType(LibraryParam param) -> bool {
  switch (param) {
    case LibraryParam::kType:
    case LibraryParam::kElement:
    case LibraryParam::kIndex:
      return true;
    case LibraryParam::kBound:
      return false;
  }
  throw InternalError("support: unknown library parameter");
}

}  // namespace lyra::support
