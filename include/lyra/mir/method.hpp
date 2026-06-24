#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

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
// a process body uses. `result_type` carries the call protocol -- a coroutine
// type for a time-consuming task (LRM 13.3), a value or void type for a
// zero-time function (LRM 13.4) -- so the backend reads task-versus-function
// from the result type, not a side enum. `params` names which of the body's
// locals are formals (the rest are interior locals). Static-lifetime body
// locals are realized as members on the enclosing class at HIR -> MIR; they do
// not appear here.
struct MethodDecl {
  std::string name;
  TypeId result_type;
  std::vector<MethodParam> params;
  Block root_block;
};

}  // namespace lyra::mir
