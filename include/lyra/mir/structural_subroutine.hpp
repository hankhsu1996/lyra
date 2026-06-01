#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// LRM 13.5 argument direction. MIR carries the semantic direction only; the
// C++ argument-passing mode is the backend's decision.
enum class ParamDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
  kRef,
  kConstRef,
};

// A formal argument of a subroutine. `name` is the procedural var the body
// reads through; the backend renders it as the C++ parameter of that name.
struct SubroutineParam {
  std::string name;
  TypeId type;
  ParamDirection direction = ParamDirection::kInput;
};

// A subroutine is a callable peer of a process: its body is a ProceduralScope,
// the same shape a process body uses. `params` names which of the scope's vars
// are formals (the rest are body locals).
struct StructuralSubroutineDecl {
  std::string name;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  ProceduralScope root_procedural_scope;
};

}  // namespace lyra::mir
