#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

inline auto AddChildBody(std::vector<mir::Body>& bodies, mir::Body body)
    -> mir::BodyId {
  const mir::BodyId id{static_cast<std::uint32_t>(bodies.size())};
  bodies.push_back(std::move(body));
  return id;
}

}  // namespace lyra::lowering::hir_to_mir
