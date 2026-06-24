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

// How a method binds its receiver and dispatches -- a generic object-oriented
// distinction (static function versus virtual instance method), not a C++
// keyword. `kStatic`: the receiver `self` is the method's explicit first
// parameter and the call is direct; this is every SystemVerilog function, task,
// and process body. `kVirtual`: `self` is the implicit receiver and the method
// overrides a runtime-base slot the engine calls polymorphically; this is the
// synthesized lifecycle bodies (the resolve and initialize phases). The backend
// reads the form to spell the receiver and the dispatch, never re-derives it.
enum class MethodForm : std::uint8_t {
  kStatic,
  kVirtual,
};

// The single callable shape MIR carries: a signature plus a body. Every
// SystemVerilog function and task, every process body, and the synthesized
// lifecycle bodies are this one concept, distinguished only by `form` (receiver
// binding) and by how a referencing site uses them. `result_type` carries the
// call protocol -- a coroutine type for a time-consuming task or a process (LRM
// 13.3), a value or void type for a zero-time function (LRM 13.4) -- so the
// backend reads task-versus-function from the result type, not a side enum.
// `params` names which of the body's locals are formals (the rest are interior
// locals); the receiver `self` is `root_block.vars[0]`, not a formal here.
// Static-lifetime body locals are realized as members on the enclosing class at
// HIR -> MIR; they do not appear here.
struct MethodDecl {
  std::string name;
  TypeId result_type;
  std::vector<MethodParam> params;
  Block root_block;
  MethodForm form = MethodForm::kStatic;
};

}  // namespace lyra::mir
