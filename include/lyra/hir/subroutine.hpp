#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

// LRM 13.5 argument directions. A formal is also a local variable of the
// subroutine body; `var` indexes the body's procedural-var arena.
enum class ParamDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
  kRef,
  kConstRef,
};

struct SubroutineParam {
  ProceduralVarId var = {};
  ParamDirection direction = ParamDirection::kInput;
};

struct StructuralSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  ProceduralBody body;
};

}  // namespace lyra::hir
