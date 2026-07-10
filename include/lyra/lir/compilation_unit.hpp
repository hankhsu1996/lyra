#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::lir {

// Which runtime object-tree base a class extends. A tree node extends one of
// these; the kind is read from the base's identity at MIR-to-LIR and carries no
// reference back to MIR.
enum class RuntimeBaseKind : std::uint8_t { kInstance, kGenScope, kScope };

struct RuntimeLibraryBase {
  RuntimeBaseKind kind;
};

using Base = std::variant<RuntimeLibraryBase>;

// A typed member of a class instance -- the storage a member place reaches by a
// member projection. Its position in this list is its class-local member
// identity. The C++ backend realizes a member as a native field; a generic
// runtime instance realizes it as runtime-owned storage.
struct Member {
  std::string name;
  TypeId type;
};

// One compiled class: its name, the base it extends, its member slots, its
// construction logic, and its callable bodies. Mirrors `mir::Class` with every
// body lowered to a CFG.
struct Class {
  std::string name;
  std::optional<Base> base;
  std::vector<Member> members;
  Function constructor;
  std::vector<Function> methods;
};

// The LIR of one compilation unit: its own type graph, its classes, and the top
// class. Self-contained -- it holds no reference to the MIR it was lowered
// from.
struct CompilationUnit {
  base::Arena<Type, TypeId> types;
  base::Arena<Class, ClassId> classes;
  ClassId root{};
};

}  // namespace lyra::lir
