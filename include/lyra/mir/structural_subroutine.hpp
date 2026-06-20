#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// LRM 13.3 / 13.4: a task may consume simulation time and is enabled as a
// statement, while a function executes in zero time and yields a value in an
// expression. MIR carries the semantic kind; how each is realized in C++ is the
// backend's decision.
enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

// LRM 13.5 argument direction. MIR carries the semantic direction only; the
// C++ argument-passing mode is the backend's decision. A `ref` / `const ref`
// formal is not a direction here: its `type` is a `RefType` and it is passed by
// value (`kInput`) -- the reference value carries the aliasing.
enum class ParamDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
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
// are formals (the rest are body locals). Static-lifetime body locals are
// realized as structural vars on the enclosing structural scope at HIR -> MIR;
// they do not appear here.
struct StructuralSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  ProceduralScope root_procedural_scope;
};

}  // namespace lyra::mir
