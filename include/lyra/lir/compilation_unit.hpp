#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/external_unit_object_id.hpp"
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

// The object of a unit this one references, as far as that unit published it:
// which unit defines it and the class an instance of it is, both resolved at
// link time, and the members it published at the positions their storage sits
// in. This unit compiles none of it, which is why it sits apart from the
// classes above: no walk that emits those can reach it.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
  std::vector<Member> members;
};

// The LIR of one compilation unit: its own type graph, its classes, the objects
// of other units it compiled against, every function it compiles, and the class
// its object tree is rooted at, when it roots one -- a unit that declares only
// a namespace compiles functions and roots no objects. Self-contained -- it
// holds no reference to the MIR it was lowered from.
//
// Every body is a function here, whatever declared it, and its position is the
// identity a call names. A class reaches its own bodies the same way any other
// caller does.
struct CompilationUnit {
  base::Arena<Type, TypeId> types;
  base::Registry<Class, ClassId> classes;
  base::Arena<ExternalUnitObject, ExternalUnitObjectId> external_unit_objects;
  base::Registry<Function, FunctionId> functions;
  std::optional<ClassId> root;
};

}  // namespace lyra::lir
