#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {

// An SSA-style value: a function parameter or the result of one instruction.
// Values are numbered per function in the function's value arena.
struct ValueId {
  std::uint32_t value;

  auto operator<=>(const ValueId&) const -> std::strong_ordering = default;
};

struct BlockId {
  std::uint32_t value;

  auto operator<=>(const BlockId&) const -> std::strong_ordering = default;
};

// Whether a value is a callable parameter or a temporary the lowering minted
// for an instruction result.
enum class LocalKind : std::uint8_t { kParam, kTemp };

struct Local {
  std::string name;
  TypeId type;
  LocalKind kind;
};

struct Use {
  ValueId value;
};

struct IntConst {
  mir::IntegralConstant value;
  TypeId type;
};

struct StrConst {
  std::string value;
  TypeId type;
};

// An instruction input: a prior value, or an inline constant. A constant is an
// operand rather than a value of its own because it has no storage and no
// dataflow origin to name -- it is materialized at the use site.
using Operand = std::variant<Use, IntConst, StrConst>;

// A reference to a class method of this unit: the class and the method's index
// in that class's method list. Resolved against the LIR unit's own class arena.
struct MethodRef {
  ClassId class_id;
  std::uint32_t index;
};

struct BuiltinTarget {
  support::BuiltinFn fn;
};

struct MethodTarget {
  MethodRef method;
};

struct ConstructTarget {
  TypeId result;
};

// The target of a call: a runtime builtin, a class method of this unit, or a
// value constructor named by the call's result type.
using CallTarget = std::variant<BuiltinTarget, MethodTarget, ConstructTarget>;

struct CallInstr {
  CallTarget target;
  std::vector<Operand> args;
};

// Builds an aggregate value from its elements -- an array literal feeding a
// runtime container or argument pack.
struct AggregateInstr {
  std::vector<Operand> elements;
};

using InstrData = std::variant<CallInstr, AggregateInstr>;

// One instruction: it defines `result` (whose type lives on the function's
// value arena) from `data`.
struct Instr {
  ValueId result;
  InstrData data;
};

// Returns from the callable. `is_coroutine` distinguishes a coroutine
// completion from a plain return; the value, when present, rides the result.
struct ReturnTerm {
  std::optional<Operand> value;
  bool is_coroutine = false;
};

using TerminatorData = std::variant<ReturnTerm>;

struct Terminator {
  TerminatorData data;
};

struct BasicBlock {
  std::vector<Instr> instrs;
  Terminator terminator;
};

// A callable lowered to a CFG. `values` holds every value of the body --
// parameters first, then instruction temporaries; `params` names the parameter
// subset in signature order, with the receiver `self` at `params[0]`. The entry
// block is `blocks[0]`.
struct Function {
  std::string name;
  base::Arena<Local, ValueId> values;
  std::vector<ValueId> params;
  TypeId result_type;
  std::vector<BasicBlock> blocks;
};

}  // namespace lyra::lir
