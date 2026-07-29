#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
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
// and the interface it publishes -- its constructor and its methods, named by
// the identities of the unit's functions. A class lists a function rather than
// holding it because the function is the same kind of thing wherever it is
// listed; what the listing adds is that the class's realization must present
// it, which is what a virtual slot indexes.
struct Class {
  std::string name;
  std::optional<Base> base;
  std::vector<Member> members;
  FunctionId constructor{};
  std::vector<FunctionId> methods;
};

// The LIR of one compilation unit: its own type graph, its classes, every
// function it compiles, and the top class. Self-contained -- it holds no
// reference to the MIR it was lowered from.
//
// Every body is a function here, whatever declared it, and its position is the
// identity a call names. A class reaches its own bodies the same way any other
// caller does.
struct CompilationUnit {
  base::Arena<Type, TypeId> types;
  base::Arena<Class, ClassId> classes;
  base::Arena<Function, FunctionId> functions;
  ClassId root{};
};

}  // namespace lyra::lir
