#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"
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

// A static-lifetime body local (LRM 13.3.1). It does not live in the
// activation; the backend gives it one storage slot per module instance. `var`
// indexes `root_procedural_scope.vars` (body references reach it through that
// id); `init` indexes `root_procedural_scope.exprs` and is evaluated once when
// the instance is built, not on each call.
struct StaticLocal {
  ProceduralVarId var;
  ExprId init;
};

// A subroutine is a callable peer of a process: its body is a ProceduralScope,
// the same shape a process body uses. `params` names which of the scope's vars
// are formals (the rest are body locals). `static_locals` lists the body locals
// whose lifetime is static; the backend stores them per instance instead of in
// the activation.
struct StructuralSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  ProceduralScope root_procedural_scope;
  std::vector<StaticLocal> static_locals;
};

}  // namespace lyra::mir
