#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lir {

struct ClassId {
  std::uint32_t value;

  auto operator<=>(const ClassId&) const -> std::strong_ordering = default;
};

// One compiled class: its name, the base it extends, its construction logic,
// and its callable bodies. Mirrors `mir::Class` with every body lowered to a
// CFG.
struct Class {
  std::string name;
  std::optional<mir::ClassRef> base;
  Function constructor;
  std::vector<Function> methods;
};

// The LIR of one compilation unit. `source` is the MIR this was lowered from --
// the frozen shared context for types and class metadata; the LIR unit must not
// outlive it. `root` identifies the top class.
struct CompilationUnit {
  const mir::CompilationUnit* source = nullptr;
  base::Arena<Class, ClassId> classes;
  ClassId root{};
};

}  // namespace lyra::lir
