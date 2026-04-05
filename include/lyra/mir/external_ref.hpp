#pragma once

#include <cstdint>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"

namespace lyra::mir {

// Body-local handle for a non-local access requirement.
// Indexes into CompiledModuleBody::external_refs.
struct ExternalRefId {
  uint32_t value = 0;
  auto operator==(const ExternalRefId&) const -> bool = default;
};

// Declares the required access kind for an external reference.
// A kRead ref lowers to loads through the bound handle at runtime.
// A kWrite ref lowers to stores. A kReadWrite supports both.
// External refs are first-class places for both reads and writes;
// one kExternalRef place root handles both directions symmetrically.
enum class ExternalAccessKind : uint8_t {
  kRead,
  kWrite,
  kReadWrite,
};

// Body-local declaration of a non-local access requirement.
// Compile time knows only the type and access kind.
// The actual target is resolved at construction time.
struct ExternalAccessRecipe {
  ExternalRefId ref_id;
  TypeId type;
  ExternalAccessKind access_kind = ExternalAccessKind::kRead;
  common::OriginId origin = common::OriginId::Invalid();
};

}  // namespace lyra::mir
