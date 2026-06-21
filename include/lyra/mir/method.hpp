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
enum class MethodKind : std::uint8_t {
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

// A formal argument of a method. `name` is the local the body reads through;
// the backend renders it as the C++ parameter of that name.
struct MethodParam {
  std::string name;
  TypeId type;
  ParamDirection direction = ParamDirection::kInput;
};

// A method is a callable peer of a process: its body is a Block, the same shape
// a process body uses. `params` names which of the body's locals are formals
// (the rest are interior locals). Static-lifetime body locals are realized as
// members on the enclosing class at HIR -> MIR; they do not appear here.
struct MethodDecl {
  std::string name;
  MethodKind kind;
  TypeId result_type;
  std::vector<MethodParam> params;
  Block root_block;
};

}  // namespace lyra::mir
