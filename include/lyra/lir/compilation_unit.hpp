#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::lir {

// A base class defined in this compilation unit, named by its LIR class
// identity. The layout of the base is visible to the artifact this class
// emits into.
struct IntraUnitBase {
  ClassId class_id;
};

// A base class declared outside this compilation unit, named by its target-
// language qualified name.
struct ExternalBase {
  std::string qualified_name;
};

using Base = std::variant<IntraUnitBase, ExternalBase>;

// A typed member of a class instance -- the storage a member place reaches by a
// member projection. Its position in this list is its class-local member
// identity. The C++ backend realizes a member as a native field; a generic
// runtime instance realizes it as runtime-owned storage.
struct Member {
  std::string name;
  TypeId type;
};

// One compiled class: its name, the base it extends, the members it declares,
// its construction logic, and its callable bodies. Mirrors `mir::Class` with
// every body lowered to a CFG.
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
