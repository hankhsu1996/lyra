#pragma once

#include <map>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/library_callback.hpp"

namespace lyra::mir {

// One data object a scope describes (LRM 21.7.1.2): what its declaration
// states -- the identifier it is declared under, the net type it is declared
// with, none for a variable, and its type as the source declared it, a
// constant of the unit -- each port of the scope connecting it, and the record
// of the block, task or function enclosing it.
struct DescribedDataObject {
  std::string identifier;
  std::optional<support::NetType> net_type;
  ConstantId typespec;
  std::vector<support::DescribedPort> ports;
  std::optional<ConstantId> enclosing;

  auto operator==(const DescribedDataObject&) const -> bool = default;
};

// The two bodies through which a reader reaching the design by name reaches
// every data object a scope describes, each handed the object, the position of
// the data object among those the scope describes, and what the reader gave:
// one hands the serializer the value it holds, the other hands the subscriber
// the trigger a wait for a change of it enrols. They are methods of the
// scope's class, or bodies of a namespace's unit, as serde's generated code is
// one serialize function per type handling every field.
struct DataObjectBodies {
  CallableId read;
  CallableId subscribe;

  auto operator==(const DataObjectBodies&) const -> bool = default;
};

// What a scope of the design hierarchy states of itself (LRM 37.10, 37.12):
// the record of the definition it comes from, the record of each scope it
// declares that the run builds and of each it builds nothing for, each a
// constant of the unit, each data object it declares, in the order it lists
// them, and the two bodies reaching those.
struct ScopeDeclarations {
  ConstantId definition;
  std::vector<ConstantId> elaborated;
  std::vector<ConstantId> lexical;
  std::vector<DescribedDataObject> data_objects;
  DataObjectBodies bodies;

  auto operator==(const ScopeDeclarations&) const -> bool = default;
};

// The callback of the runtime library the body `body` answers among the two
// reaching what `declarations` describes, none where it is neither.
[[nodiscard]] inline auto DataObjectCallbackOf(
    const ScopeDeclarations& declarations, CallableId body)
    -> std::optional<support::LibraryCallback> {
  if (body == declarations.bodies.read) {
    return support::LibraryCallback::kScopeReadDataObject;
  }
  if (body == declarations.bodies.subscribe) {
    return support::LibraryCallback::kScopeSubscribeDataObject;
  }
  return std::nullopt;
}

// A class static property (LRM 8.9): a named, mutable type-associated storage
// cell the class owns, shared by every instance.
// What the cell starts out holding is a design-init fact (LRM 10.5) and is not
// on this declaration: the assignment lands in whatever brings the cell's owner
// up, which is the declaring instance's construction where a structural scope
// replicates the class and the declaring unit's namespace bring-up where
// nothing does. Initializer timing and per-cell identity are separate concerns,
// and a statement list is the one home a backend reads for either.
// It carries no name, for the reason a field carries none: the pool also takes
// what a class's bodies keep for the whole class, and those cells the source
// never declared.
struct StaticPropertyDecl {
  TypeId type;
};

// One entry of the relation between a class and the type-associated storage it
// answers by name: the name the source reaches it by within the class -- a
// static property, or a static variable one of its methods declares (LRM
// 6.21) -- and the cell it reaches.
struct NamedStaticProperty {
  support::QualifiedName name;
  StaticPropertyId slot;
};

// The name `slot` answers to among `named`, or nothing where nothing names it.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedStaticProperty> named, StaticPropertyId slot)
    -> const support::QualifiedName* {
  for (const NamedStaticProperty& entry : named) {
    if (entry.slot == slot) {
      return &entry.name;
    }
  }
  return nullptr;
}

// One behavior of an interface a class answers (LRM 8.26.2), and the
// behavior of the class's lineage answering it, each named the way a call
// names one. A class extending this one that takes the answering behavior over
// answers the interface's with its own body too. Nothing answers it where an
// abstract class leaves the implementation to a class extending it.
struct ConformingBehavior {
  VirtualSlot interface_behavior;
  std::optional<VirtualSlot> answered_by;
};

// An interface class the class's own declaration names (LRM 8.26.2) that is a
// type of an instance (LRM 6.22), and the callable of the class answering,
// handed the instance the class belongs to, the instance that name reaches.
// It takes no object: an interface class has none, and the name reaches the
// same instance whatever object asks. The library follows these through the
// definitions for a cast toward an interface class (LRM 8.26.5).
struct InterfaceReach {
  ClassId of;
  CallableId reach;
};

// A method the class defines that something reaches without knowing the
// object's class, and the body of the class through which it is entered on an
// object handed over as the root every object is.
struct MethodEntry {
  CallableId method;
  CallableId entry;
};

// One behavior an object of a class dispatches on (LRM 8.20) and the method
// whose entry answers it on such an object: the one the nearest class of its
// lineage gives, or none where the class leaves the behavior abstract (LRM
// 8.21).
struct DispatchedBehavior {
  VirtualSlot behavior;
  std::optional<CallableId> answered_by;
};

// What an object of a class answers the methods one interface declares
// itself with (LRM 8.26.5), in the order it declares them: for each, the method
// of the class's lineage whose entry answers it, or none where an abstract
// class leaves it.
struct InterfaceTable {
  ClassId of;
  std::vector<std::optional<CallableId>> answered_by;
};

struct Class {
  // The unit that defines the class, where it is another unit's. Such a class
  // is declared here and defined there, as an imported declaration is: its
  // fields are its storage as that unit published it, the named ones first and
  // the rest by type alone; the callables it declares and its construction
  // state what each takes and, for a method, its name and dispatch, and no
  // body; and it has no constant or record value here, which are that unit's
  // to emit.
  //
  // What the class declares among the unit's callables is read off their
  // owners, which name the class, so a class still being built already
  // declares them.
  std::optional<std::string> defining_unit;
  // The path another unit reaches this class by, for a class the source
  // declared (LRM 8.3) and for what a scope of the design hierarchy published,
  // which a hierarchical name reaches (LRM 23.6); for a class the lowering
  // builds for its own use, which nothing outside this unit names, its place
  // among those. It is the class's identity, which is why it sits here rather
  // than in a relation: what a referrer resolves and what a symbol is made from
  // are the same steps.
  support::ClassName identity;
  std::optional<ClassRef> base;
  // The interfaces the class's declaration names, in the order written. A
  // value of the class is also a value of each, of every interface those
  // extend, and of whatever its base is; none of that is restated here.
  std::vector<ClassId> implements;
  // For each behavior of each interface a value of this class is also a value
  // of, what answers it. Empty for an interface, which answers nothing.
  std::vector<ConformingBehavior> conforming;
  // The entry of each method of this class that takes part in dispatch or
  // answers what the runtime library asks of an object. Empty for a class
  // another unit defines, whose entries are named by their methods.
  std::vector<MethodEntry> entries;
  // What an object of the class runs for each behavior it dispatches on: those
  // of the class it extends first, in that class's order, then those it
  // introduces, in the order it declares them -- so a class extending another
  // lists each of the other's behaviors in the same order. A class another unit
  // defines lists its behaviors and answers none.
  std::vector<DispatchedBehavior> dispatch;
  // What an object of the class answers each interface it is also an
  // object of with (LRM 8.26), each interface once however many ways it
  // is reached (LRM 8.26.6.3). Empty for an interface and for a class
  // another unit defines.
  std::vector<InterfaceTable> interface_tables;
  bool is_final = false;
  bool is_interface = false;
  TypeId self_pointer_type;
  // What a class realizing a scope of the design hierarchy states of its
  // instances, which the class's own definition points the runtime at. Absent
  // for every other class.
  std::optional<ScopeDeclarations> scope_declarations;
  base::Arena<FieldDecl, FieldId> fields;
  // The identifiers the source wrote for storage this class holds, each paired
  // with the field it reaches. A declared variable, an instance and a signal a
  // scope offers take part; the storage a lowering synthesized for its own use
  // does not, which is what makes "did the source write this" a question the
  // class answers rather than one a consumer reads out of a spelling.
  std::vector<NamedField> named_fields;
  // The classes this one structurally owns -- the children it builds. Each
  // names a registry identity, in construction order. Ownership of the
  // declarations is the unit's registry; this is the containment relation over
  // those identities. Where a backend places a class is the backend's own
  // affair and is not stated here.
  std::vector<ClassId> contained;
  // The class's static properties (LRM 8.9): mutable type-associated storage
  // cells shared by every instance. Peer to `fields` on the instance-versus-
  // type-associated axis: a static property is one cell owned by the type,
  // never a member replicated into each instance.
  base::Arena<StaticPropertyDecl, StaticPropertyId> static_properties;
  // The identifiers the source declared for type-associated storage this class
  // holds. A property the source wrote takes part; a cell a body keeps for the
  // whole class does not.
  std::vector<NamedStaticProperty> named_static_properties;
  // The names this class answers and which body each reaches (LRM 8.3, one
  // name space over a class's members). A method the source declared is here
  // because its symbol is spelled from that name, the one a call site outside
  // this unit spells where this is the class it names; a body the compiler
  // synthesized is not, because nothing spells one.
  std::vector<NamedCallable> named_callables;
  // The bodies of this class the runtime library calls on its objects (LRM
  // 18.6, and the phases a scope is driven through), each under the callback
  // it answers. A body answering one is no method of the source and takes no
  // part in dispatch; the library finds it through the class's definition.
  std::map<support::LibraryCallback, CallableId> library_callbacks;
  // The body answering, of an object, the instance the class belongs to (LRM
  // 6.22), as the object keeps it; none where the class belongs to none, and
  // for an interface class, which keeps nothing.
  std::optional<CallableId> declaring_instance;
  // One for each name of an interface class of an instance the class's own
  // declaration writes, in the order written; what the class it extends names
  // is that class's.
  std::vector<InterfaceReach> interface_reaches;
  // The DPI-C exports an instance of the class answers (LRM 35.5.3): the C
  // identifier the foreign side calls each by, and the body it reaches. Only a
  // class an instance of the design hierarchy is built of has any.
  std::vector<NamedCallable> exports;
  // The value of the class's definition: the one constant every object of the
  // class points at, which the runtime library reads to answer what cannot be
  // asked where the asker is compiled -- what the class extends, how an object
  // of it ends, what an instance of the design hierarchy does in each phase and
  // the DPI-C exports it answers, what `randomize()` asks of an object of a
  // source class, the interfaces it implements, and the body each behavior it
  // dispatches on runs. Every unit names it by the class, so it is the class's
  // and not one of the unit's constants, and its type is the same for every
  // class, so only its value is here. It is stated once every class of the
  // unit is, since it reads what the class's whole lineage gives, and none
  // where another unit defines the class.
  std::optional<ConstantValue> definition;

  // The callable this class answers the library's `callback` with, or nothing
  // where the class gives it none of its own.
  [[nodiscard]] auto CallbackBody(support::LibraryCallback callback) const
      -> std::optional<CallableId> {
    const auto answered = library_callbacks.find(callback);
    if (answered == library_callbacks.end()) {
      return std::nullopt;
    }
    return answered->second;
  }

  // The library callback `callable` answers, or nothing where it answers none.
  [[nodiscard]] auto CallbackAnsweredBy(CallableId callable) const
      -> std::optional<support::LibraryCallback> {
    for (const auto& [callback, body] : library_callbacks) {
      if (body == callable) {
        return callback;
      }
    }
    return std::nullopt;
  }

  // The body through which `method` is entered on an object handed over as the
  // root every object is, or nothing where the class gives it none.
  [[nodiscard]] auto EntryOf(CallableId method) const
      -> std::optional<CallableId> {
    for (const MethodEntry& entry : entries) {
      if (entry.method == method) {
        return entry.entry;
      }
    }
    return std::nullopt;
  }
};

}  // namespace lyra::mir
