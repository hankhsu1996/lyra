#pragma once

#include <cstdint>
#include <optional>
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

// LRM 13.5 data flow at the call boundary. `output` and `inout` carry a value
// back to the actual at return, so a call desugars them into a temp with a
// copy-out assignment after the call (`inout` also copies in before). `input`
// passes a value and `ref` / `const ref` alias the actual directly, so neither
// writes back. Only the writeback directions force the call into statement
// position, where the copy-out can be sequenced.
[[nodiscard]] constexpr auto RequiresWriteback(ParamDirection direction)
    -> bool {
  return direction == ParamDirection::kOutput ||
         direction == ParamDirection::kInOut;
}

// LRM 13.4.1 implicit result variable: a non-void function implicitly declares
// a body-local variable of the return type that the body reads and writes
// through the function name. `result_var` indexes the body's procedural-var
// arena for that variable; it is absent for void functions and tasks, which
// yield no value.
struct StructuralSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  std::optional<ProceduralVarId> result_var;
  ProceduralBody body;
};

}  // namespace lyra::hir
