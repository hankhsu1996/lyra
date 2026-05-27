#pragma once

#include <cstdint>
#include <string>

#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

struct StructuralSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
};

}  // namespace lyra::hir
