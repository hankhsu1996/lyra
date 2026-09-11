#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/external_unit_object_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

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

// The runtime's object tree as a base: what a class extends by standing in that
// tree, and the three functions it supplies to be driven through.
//
// The runtime drives every object of the tree through them, in the order they
// stand here: every route and alias is bound while the tree is complete and
// nothing has run, then every cell takes the value its declaration gives it
// (LRM 10.5), then every process is created (LRM 9.2). Each is entered on one
// instance and returns before the next begins, which is why the three are
// separate functions rather than one with phases inside it. Standing in the
// tree and being driven are one fact, so a class in the tree that states no way
// to run, and a way to run on a class outside it, are both unspellable.
//
// What the runtime calls the class it provides is a spelling of whichever
// target emits against that library, so it is not here: this states that the
// base is the runtime's, and how the class stands in it.
struct ObjectTreeBase {
  FunctionId resolve_state;
  FunctionId initialize_state;
  FunctionId create_processes;
};

using Base = std::variant<IntraUnitBase, CrossUnitBase, ObjectTreeBase>;

// A typed member of whatever declares it -- the storage a member place reaches
// by a member projection. Its position in the declaring list is its member
// identity there. The C++ backend realizes a member as a native field; a
// generic runtime value realizes it as runtime-owned storage.
struct Member {
  std::string name;
  TypeId type;
};

// One behavior a class takes over from its lineage (LRM 8.20): which behavior,
// and the body this class answers it with. Taking one over without a body would
// leave it exactly as it was, so nothing states that.
struct DispatchTakeover {
  StatedDispatchRef method;
  FunctionId body;
};

// One subroutine a scope answers a hierarchical name with (LRM 23.8.1): the
// identifier such a name spells, and the body it reaches. Every subroutine a
// scope declares is answered for, a unit compiled alone having no way to know
// which of them a name will reach.
//
// This is the scope's own namespace, so a body no name reaches is absent from
// it rather than listed with nothing to say. The table is what is asked -- what
// does this name reach here -- and a body's presence in it is a relation the
// namespace holds, never a property the body carries.
struct PublishedSubroutine {
  std::string name;
  FunctionId body;
};

// One compiled class: its name, the base it extends, the members it declares,
// its constructor, the behaviors it introduces, the ones it takes over, and the
// subroutines it answers a name with. A class lists a function rather than
// holding it because the function is the same kind of thing wherever it is
// listed.
//
// A class states what it adds to its lineage and nothing about the lineage
// itself -- the same way it states its own members and not its base's. What a
// value of it holds and what a value of it answers are read from the lineage,
// which is what keeps one declaration's meaning independent of what extends it.
//
// An introduction's position in the list is the behavior's identity here, the
// way a member's position is its identity above, and a body is absent where the
// behavior is declared without an implementation (LRM 8.21); no value answers
// such a behavior, because a class leaving one unanswered is never constructed.
// `name` is the class's own, as its unit declared it -- not a symbol. What the
// class links under, what its constructor links under, and what the record
// describing it links under are three different symbols over those same parts,
// so none of them is derivable from another and each is composed where it is
// used.
// One behavior a class introduces (LRM 8.20): the identifier a referrer spells,
// and the body answering it -- absent where the class declares the behavior
// without one (LRM 8.21 pure virtual). The name is here because a referrer that
// cannot name the class counts no position for itself and asks by name instead;
// nothing on the simulation path reads it.
struct Introduction {
  std::string name;
  std::optional<FunctionId> body;
};

// One class a scope answers a name with (LRM 23.9). A class declared inside a
// design element is nameable only inside the scope declaring it, so a referrer
// outside reaches it by walking to that scope and asking -- the same way it
// reaches a cell or a subroutine the scope's unit never published.
struct DeclaredClass {
  std::string name;
  ClassId declaration;
};

struct Class {
  std::string name;
  std::optional<Base> base;
  std::vector<Member> members;
  FunctionId constructor{};
  std::vector<Introduction> introduces;
  std::vector<DispatchTakeover> takeovers;
  std::vector<PublishedSubroutine> subroutines;
  std::vector<DeclaredClass> declares;
};

// How values of this class stand in the runtime's object tree, or nothing where
// they stand outside it. Extending a class -- this unit's or another's -- is
// what a class of the source language does and says nothing about the tree, so
// which base a class has is the whole of the answer.
//
// One level is the whole answer because a scope is sealed: nothing extends one,
// so a class either takes the tree as its base or stands outside it entirely.
// Walking a lineage here would be looking for a shape the source language
// cannot write.
[[nodiscard]] inline auto ObjectTreeBaseOf(const Class& cls)
    -> const ObjectTreeBase* {
  if (!cls.base.has_value()) {
    return nullptr;
  }
  return std::get_if<ObjectTreeBase>(&*cls.base);
}

// Whether they stand in it at all, for a reader that wants nothing else. Asking
// this is the read above with the answer dropped, so the two cannot disagree.
[[nodiscard]] inline auto IsObjectTreeNode(const Class& cls) -> bool {
  return ObjectTreeBaseOf(cls) != nullptr;
}

// A class of another unit this one reaches a property on, as far as that unit
// published it: which unit declares it and its canonical name, both resolved at
// link time, and the properties it published at the slots that class gave them.
// Those properties are a prefix of the class's own storage, so a slot counted
// here is the slot the declaring unit gave. This unit compiles none of it,
// which is why it sits apart from the classes above.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // The class it extends, as its own unit promised. What it inherited is not
  // among the members below, so a value of it carries a member of an ancestor
  // by way of this chain.
  std::optional<CrossUnitBase> base;
  std::vector<Member> members;
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
// It carries no name: the source declares no closure, so its position in its
// unit is the whole of its identity and what it links under is composed from
// that.
struct Closure {
  std::vector<Member> captures;
  FunctionId invoke{};
};

// One struct declaration: the fields values of it hold, and nothing else. It
// shares the member vocabulary with a class and a closure because a field is
// reached the same way a property and a capture are; what it does not share is
// any code, since a struct is storage a builder fills rather than a thing that
// runs.
// `name` is the struct's own, as its unit named it -- not a symbol.
struct Struct {
  std::string name;
  std::vector<Member> fields;
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
  // The unit's own name, carried from the program this one was lowered from.
  // Every identity below indexes a pool this unit owns and numbers from zero,
  // so a reader holding two units needs the pool named to know which one an id
  // belongs to.
  std::string name;
  TypePool types;
  base::Registry<Class, ClassId> classes;
  base::Registry<Closure, ClosureId> closures;
  base::Registry<Struct, StructId> structs;
  // One record per unit this one compiled against, under the same
  // declare-then-define lifecycle a class has: a member of one record may name
  // another of them -- an interface port's does (LRM 25.3) -- so a record's
  // identity exists before its members are filled in.
  base::Registry<ExternalUnitObject, ExternalUnitObjectId>
      external_unit_objects;
  // One entry per class of another unit this one reaches a property on, found
  // by the pair that names the class -- the pair every reference to one
  // carries, so a reference and the record its slot is counted out of cannot
  // come apart.
  std::vector<ExternalClass> external_classes;
  base::Registry<Function, FunctionId> functions;
  std::vector<StaticStorage> static_storage;
  // The nullary function building each type's runtime descriptor, one answer
  // per type and present only for a described one. Building a value is an
  // instruction sequence like any other, so a description is code here, and the
  // type it describes is what reaches it.
  base::Translation<TypeId, std::optional<FunctionId>> packed_type_initializers;
  std::optional<ClassId> root;
};

// The type naming the class `base` extends, in this unit's own pool, or nothing
// where the lineage ends there. A base the runtime library defines ends one: it
// declares nothing of the source language, so there is no class of the program
// past it.
[[nodiscard]] inline auto BaseType(
    const CompilationUnit& unit, const Base& base) -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [&](const IntraUnitBase& intra) -> std::optional<TypeId> {
            return unit.types.Intern(
                Type{ObjectType{.class_id = intra.class_id}});
          },
          [&](const CrossUnitBase& cross) -> std::optional<TypeId> {
            return unit.types.Intern(
                Type{CrossUnitClassType{
                    .unit_name = cross.unit_name,
                    .class_name = cross.class_name}});
          },
          [](const ObjectTreeBase&) -> std::optional<TypeId> {
            return std::nullopt;
          }},
      base);
}

// The record kept of the class `class_name` of unit `unit_name`, or nothing
// where this unit consumed no promise about it -- which is the state of every
// class it merely names.
[[nodiscard]] inline auto FindExternalClass(
    const CompilationUnit& unit, std::string_view unit_name,
    std::string_view class_name) -> const ExternalClass* {
  for (const ExternalClass& record : unit.external_classes) {
    if (record.unit_name == unit_name && record.class_name == class_name) {
      return &record;
    }
  }
  return nullptr;
}

}  // namespace lyra::lir
