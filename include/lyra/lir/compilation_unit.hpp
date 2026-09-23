#pragma once

#include <cstdint>
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
#include "lyra/lir/integral_constant_id.hpp"
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

// The runtime's object tree as a base: what a class extends to be rooted in
// that tree.
//
// A base form names a class and nothing else. How the runtime drives an object
// is a property of whichever class supplies the functions it drives one
// through, so that class states it.
//
// What the runtime calls the class it provides is a spelling of whichever
// target emits against that library, so this states that the base is the
// runtime's and nothing more.
struct ObjectTreeBase {
  auto operator==(const ObjectTreeBase&) const -> bool = default;
};

// The functions the runtime enters on an object of a class it drives. The
// runtime enters all three or none, so they are read as a group: every route
// and alias is bound while the tree is complete and nothing has run, then every
// cell takes the value its declaration gives it (LRM 10.5), then every process
// is created (LRM 9.2). Each is entered on one instance and returns before the
// next begins, which is why they are three functions rather than one with
// phases inside it.
struct ObjectTreeProgram {
  FunctionId resolve_state;
  FunctionId initialize_state;
  FunctionId create_processes;
};

using Base = std::variant<IntraUnitBase, CrossUnitBase, ObjectTreeBase>;

// A typed member of whatever declares it -- the storage a member place reaches
// by a member projection. Its position in the declaring list is its member
// identity there, and it carries no other, because nothing below here reaches a
// member by a name: this layer's consumers index. The C++ backend realizes a
// member as a native field; a generic runtime value realizes it as
// runtime-owned storage.
struct Member {
  TypeId type;
};

// One member a class answers a name with, and where it sits. A referrer that
// cannot name the class counts no position for itself and asks by name instead;
// nothing on the simulation path reads this. Only what the source declared
// takes part, since a name is all such a referrer has to ask with and the
// compiler minted none for the rest.
struct NamedMember {
  std::string name;
  std::uint32_t position;
};

// One behavior a class takes over from its lineage (LRM 8.20): which behavior,
// and the body this class answers it with. Taking one over without a body would
// leave it exactly as it was, so nothing states that.
struct DispatchTakeover {
  StatedDispatchRef method;
  FunctionId body;
};

// One name a scope answers a call under, and the entry that call reaches. A
// scope answers in more than one name space -- a hierarchical name spells the
// SystemVerilog identifier a subroutine was declared under (LRM 23.8.1), a
// foreign caller spells the program-global C identifier an export publishes
// (LRM 35.4) -- and one declaration may answer in both under different
// spellings, so the name space is which list holds the entry rather than
// anything the entry carries.
//
// A body no name reaches is absent rather than listed with nothing to say. The
// table is what is asked -- what does this name reach here -- and an entry's
// presence in it is a relation the name space holds, never a property of the
// body.
struct PublishedCallable {
  std::string name;
  FunctionId entry;
};

// One behavior a class introduces (LRM 8.20): the identifier a referrer spells,
// and the body answering it -- absent where the class declares the behavior
// without one (LRM 8.21 pure virtual). The name is here because a referrer that
// cannot name the class counts no position for itself and asks by name instead;
// nothing on the simulation path reads it.
struct Introduction {
  std::string name;
  std::optional<FunctionId> body;
};

// One body a class answers a name with, where the class the access names is
// what decides which body runs whatever the value turns out to be (LRM 8.14).
// The name is here for the same reason it is on an introduction: a referrer
// that cannot name the class asks by name. A behavior answering a dispatch
// position is not here, because for one of those the value decides and what
// such a referrer needs is the position rather than the body.
struct DeclaredBody {
  std::string name;
  FunctionId body;
};

// One class a scope answers a name with (LRM 23.9). A class declared inside a
// design element is nameable only inside the scope declaring it, so a referrer
// outside reaches it by walking to that scope and asking -- the same way it
// reaches a cell or a subroutine the scope's unit never published.
struct DeclaredClass {
  std::string name;
  ClassId declaration;
};

// One compiled class: its name, the base it extends, the members it declares,
// its constructor, the behaviors it introduces, the bodies it answers a name
// with outright, the behaviors it takes over, and the entries it answers a name
// with in each name space a caller spells one in. A class lists a function
// rather than holding it because the function is the same kind of thing
// wherever it is listed.
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
// What the class links under, what its constructor links under, and what the
// record describing it links under are three different symbols over the same
// parts, so none of them is derivable from another and each is composed where
// it is used.
struct Class {
  // The identifier the source declared this class under, absent for a scope of
  // the design hierarchy, which the lowering built. Every symbol qualified by
  // this class is composed from it where it is present and from the class's own
  // position where it is not, so the two ranges never meet.
  std::optional<std::string> name;
  std::optional<Base> base;
  // How the runtime drives values of this class, for a class it drives at all.
  // A class rooted in the tree that supplies none is one nothing constructs --
  // what a unit promises of its object, which states what may be reached and
  // never how it runs.
  std::optional<ObjectTreeProgram> tree_program;
  std::vector<Member> members;
  std::vector<NamedMember> named_members;
  FunctionId constructor{};
  std::vector<Introduction> introduces;
  std::vector<DeclaredBody> bodies;
  std::vector<DispatchTakeover> takeovers;
  std::vector<PublishedCallable> subroutines;
  std::vector<PublishedCallable> exports;
  std::vector<DeclaredClass> declares;
};

// How the runtime drives values of this class, or nothing where it drives none.
// A class supplies these functions or it does not, and the class itself is the
// only thing that can say so, because they are its own bodies.
[[nodiscard]] inline auto TreeProgramOf(const Class& cls)
    -> const ObjectTreeProgram* {
  return cls.tree_program.has_value() ? &*cls.tree_program : nullptr;
}

// A class of another unit this one reaches into, as far as that unit published
// it: which unit declares it and its canonical name, both resolved at link
// time, what it extends, and the properties it published at the slots that
// class gave them. Those properties are a prefix of the class's own storage, so
// a slot counted here is the slot the declaring unit gave.
//
// What a unit promised of its own object is such a class too, and lists no
// properties, because what it published is reached by performing a behavior
// rather than by a slot. Nothing about the name tells the two apart and nothing
// needs to: what differs is what each extends, which is where the question of
// whether its values stand in a tree is asked of either.
//
// This unit compiles none of it, which is why it sits apart from the classes
// above.
struct ExternalClass {
  std::string unit_name;
  std::string class_name;
  // The class it extends, as its own unit promised. What it inherited is not
  // among the members below, so a value of it carries a member of an ancestor
  // by way of this chain, and so does the question of whether values of it
  // stand in the declaring unit's object tree. A class one unit declares is
  // never the base of a class another declares, so the intra-unit form this
  // shares with a compiled class's base never arrives here.
  std::optional<Base> base;
  std::vector<Member> members;
};

// The object of a unit this one references, as far as that unit published it:
// which unit defines it and the class an instance of it is, both resolved at
// link time. What that unit published is reached by performing a behavior of
// the promise rather than by stepping into storage, so nothing here describes
// storage and nothing may. This unit compiles none of it, which is why it sits
// apart from the classes above: no walk that emits those can reach it.
struct ExternalUnitObject {
  std::string unit_name;
  std::string class_name;
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
// It carries no name: the source declares no such aggregate, so its position in
// the unit's registry is the whole of what identifies it.
struct Struct {
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
  // The nullary function building each value the unit holds, one answer per
  // entry of the pool that holds it. Building a value is an instruction
  // sequence like any other, so a description and a constant are both code
  // here, and the entry naming one is what reaches its function.
  base::Translation<TypeDescriptorId, FunctionId> type_descriptor_initializers;
  base::Translation<IntegralConstantId, FunctionId>
      integral_constant_initializers;
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

// Whether values of a class stand in the runtime's object tree, which is a
// different question from what drives them and is answered by what the class
// extends. It is the lineage rather than one base that answers: a unit's object
// is a promise standing in the tree and a class realizing it, so a class one
// step from the tree and a class two steps from it are equally in it, and only
// the realizing one supplies the bodies. A class extending a class of the
// source language leaves the lineage before reaching the tree, which is what
// makes the walk end on an answer rather than run out of steps.
//
// The walk crosses the unit boundary, because a class this unit compiles and
// one another unit promised are the same kind of thing asked the same question,
// and taking what a class extends rather than the class lets one walk serve
// both. A step into a promise this unit never consumed ends it: what goes
// unrecorded there is a class of the source language, since a promise standing
// in the tree reaches it in one step and is recorded wherever it is named.
[[nodiscard]] inline auto StandsInObjectTree(
    const CompilationUnit& unit, const std::optional<Base>& extends) -> bool {
  const std::optional<Base>* standing = &extends;
  while (standing->has_value()) {
    if (std::holds_alternative<ObjectTreeBase>(**standing)) {
      return true;
    }
    if (const auto* intra = std::get_if<IntraUnitBase>(&**standing)) {
      standing = &unit.classes.Get(intra->class_id).base;
      continue;
    }
    const auto& cross = std::get<CrossUnitBase>(**standing);
    const ExternalClass* promised =
        FindExternalClass(unit, cross.unit_name, cross.class_name);
    if (promised == nullptr) {
      return false;
    }
    standing = &promised->base;
  }
  return false;
}

}  // namespace lyra::lir
