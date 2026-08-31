#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
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

// A base class another compilation unit declares, named by that unit and the
// class's canonical name.
struct CrossUnitBase {
  std::string unit_name;
  std::string class_name;
};

// A base class the runtime library defines, named by the library symbol.
struct RuntimeBase {
  std::string symbol;
};

using Base = std::variant<IntraUnitBase, CrossUnitBase, RuntimeBase>;

// A typed member of whatever declares it -- the storage a member place reaches
// by a member projection. Its position in the declaring list is its member
// identity there. The C++ backend realizes a member as a native field; a
// generic runtime value realizes it as runtime-owned storage.
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

// One compiled closure: the captures it holds and the one body that reads them.
// Its captures are initialized where a value of it is built rather than by a
// body of its own, and nothing dispatches on it, so it shares the member
// vocabulary with a class and no part of its interface.
struct Closure {
  std::string name;
  std::vector<Member> captures;
  FunctionId invoke{};
};

// Storage this unit defines that no instance owns: one cell for the whole
// program, reached by its linkage symbol rather than through a receiver.
// `type` is the storage's own type, which is what tells whoever realizes it
// what to build; a reference to it is an operand typed as a pointer to that.
// The unit that declares the storage lists it, and only that unit does, so
// every referrer -- this one included -- names it and none defines it twice.
struct StaticStorage {
  std::string symbol;
  TypeId type;
};

// The LIR of one compilation unit: its own type graph, its classes, its
// closures, the objects of other units it compiled against, the storage it
// shares program-wide, every function it compiles, and the class its object
// tree is rooted at, when it roots one -- a unit that declares only a namespace
// compiles functions and roots no objects. Self-contained -- it holds no
// reference to the MIR it was lowered from.
//
// Every body is a function here, whatever declared it, and its position is the
// identity a call names. A class reaches its own bodies the same way any other
// caller does.
struct CompilationUnit {
  base::Arena<Type, TypeId> types;
  base::Registry<Class, ClassId> classes;
  base::Registry<Closure, ClosureId> closures;
  base::Arena<ExternalUnitObject, ExternalUnitObjectId> external_unit_objects;
  base::Registry<Function, FunctionId> functions;
  std::vector<StaticStorage> static_storage;
  std::optional<ClassId> root;
};

}  // namespace lyra::lir
