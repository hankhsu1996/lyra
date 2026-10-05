#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::mir {

// A constant a class holds: plain machine data of type `type`, whose value is
// `initializer` -- an expression over nothing that runs, only names, addresses
// and literals -- so a target states it as data. The runtime library reads such
// a constant as one of its own structures or as an array of them.
struct ClassConstantDecl {
  TypeId type;
  ValueBuild initializer;
};

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
// answers by name: the identifier the source declared the property under, and
// the cell it reaches.
struct NamedStaticProperty {
  std::string name;
  StaticPropertyId slot;
};

// The identifier `slot` answers to among `named`, or nothing where nothing
// names it.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedStaticProperty> named, StaticPropertyId slot)
    -> std::optional<std::string_view> {
  for (const NamedStaticProperty& entry : named) {
    if (entry.slot == slot) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

// The class's construction protocol. The constructor is a bare body block the
// class owns directly, not a member of the callable arena: it is never a call
// target and never dispatches, so it carries no callable identity. `code` runs
// the constructor body, which is where every step of construction the body can
// express already lives -- a property's declared initializer among them (LRM
// 8.7), lowered as an ordinary write in declaration order. What remains beside
// it is the one step no body statement reaches: the base is constructed before
// the body runs, so a consumer initializes the base first, then descends into
// `code`.
struct ConstructorDecl {
  // A class that states a construction defines it, so this code is always a
  // definition; an empty body is a class that constructs nothing.
  CallableCode code = CallableCode::Defined();
  // What the base's constructor is entered with (LRM 8.7), each argument
  // evaluated in this constructor's own local scope. Which base that is, and
  // whether the class has one, is the class's own declaration. The list is
  // every argument that construction takes, so a consumer forwards it as it
  // stands and supplies nothing of its own.
  std::vector<ExprId> base_args;
};

// One behavior of an interface class a class answers (LRM 8.26.2), and the
// behavior of the class's lineage answering it, each named the way a call
// names one. A class extending this one that takes the answering behavior over
// answers the interface's with its own body too. Nothing answers it where an
// abstract class leaves the implementation to a class extending it.
struct ConformingBehavior {
  VirtualSlot interface_behavior;
  std::optional<VirtualSlot> answered_by;
};

struct Class {
  // The name another unit reaches this class by, absent where nothing outside
  // this unit ever names it -- a class the lowering builds for its own use.
  // Present for a class the source declared (LRM 8.3) and for what a scope of
  // the design hierarchy published, which a hierarchical name reaches (LRM
  // 23.6). It is the class's cross-unit identity, which is why it sits here
  // rather than in a relation: a name a referrer resolves and a name a backend
  // spells are the same string.
  std::optional<std::string> name;
  // Further names another unit reaches this same class by: a loop whose blocks
  // compiled to one scope publishes that scope once per block (LRM 27.4), each
  // block under a name of its own, and a referrer names the block it reached.
  std::vector<std::string> aliases;
  std::optional<ClassRef> base;
  // The interfaces the class's declaration names, in the order written. A
  // value of the class is also a value of each, of every interface those
  // extend, and of whatever its base is; none of that is restated here.
  std::vector<DeclaredClassRef> implements;
  // For each behavior of each interface a value of this class is also a value
  // of, what answers it. Empty for an interface class, which answers nothing.
  std::vector<ConformingBehavior> conforming;
  bool is_final = false;
  bool is_interface_class = false;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The runtime is
  // told both where a class standing in the design hierarchy is declared, so
  // the engine can take the design-global minimum (LRM 3.14.3) and delays
  // scale to it.
  TimeResolution time_resolution;
  base::Arena<FieldDecl, FieldId> fields;
  // The identifiers the source wrote for storage this class holds, each paired
  // with the field it reaches. A declared variable, an instance and a signal a
  // scope offers take part; the storage a lowering synthesized for its own use
  // does not, which is what makes "did the source write this" a question the
  // class answers rather than one a consumer reads out of a spelling.
  std::vector<NamedField> named_fields;
  // How an object of this class is built, absent for a class no object is ever
  // built of: an interface class (LRM 8.26) holds no storage and is never
  // constructed, so it has no construction to state.
  std::optional<ConstructorDecl> constructor;
  // The classes this one structurally owns -- the children it builds. Each
  // names a registry identity, in construction order. Ownership of the
  // declarations is the unit's registry; this is the containment relation over
  // those identities. Where a backend places a class is the backend's own
  // affair and is not stated here.
  std::vector<ClassId> contained;
  // Every callable this class owns, in one pool: instance methods (LRM 8.6),
  // process and lifecycle bodies, and the receiver-less static methods (LRM
  // 8.10). A foreign function is never here: its name is program-global and
  // belongs to no class (LRM 35.4), so the unit owns it. An instance method
  // carries `self` as its first parameter and a static callable omits it,
  // but both are one `CallableDecl` reached by one `CallableId` -- the
  // receiver is a property of the signature, not a separate declaration
  // space. The constructor is not here: it is a bare body block on the
  // protocol, never a call target.
  //
  // A callable a peer body may name -- a subroutine or method, reachable by a
  // forward or mutual call (LRM 13.7) -- takes its identity while the class's
  // shape is declared, so the call resolves whatever order the two bodies
  // lower in; its body arrives later, against that identity. A callable
  // nothing names early -- a process, a continuous assign, a synthesized
  // method -- takes identity and body together where it is built. Both kinds
  // share this pool, which is why the pool admits the gap.
  base::Registry<CallableDecl, CallableId> callables;
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
  // The read-only tables the class's record below points at. Nothing outside
  // the class names one.
  base::Arena<ClassConstantDecl, ClassConstantId> constants;
  // The value of the class's record: the one constant every object of the class
  // points at, which the runtime library reads to answer what cannot be asked
  // where the asker is compiled -- what the class extends, and, for a class an
  // instance of the design hierarchy is built of, its timescale and the DPI-C
  // exports its instances answer (LRM 35.4), whose names the foreign side
  // spells. Other units name it by the class, which is why it is not among
  // `constants`, and its type is the same for every class, which is why only
  // its value is here.
  ValueBuild object_definition_initializer;

  // Which of the dispatch positions this class introduces (LRM 8.20)
  // `callable` is, or nothing where it introduces none. The positions are
  // counted in the order the class declares its callables, which is the one
  // numbering a dispatch, an override and a class's table all have to agree on.
  [[nodiscard]] auto IntroductionOrdinal(CallableId callable) const
      -> std::optional<BehaviorOrdinal> {
    if (!IntroducesSlot(callables.Get(callable).virtual_dispatch)) {
      return std::nullopt;
    }
    BehaviorOrdinal ordinal{.value = 0};
    for (const CallableId here : callables.Ids()) {
      if (here == callable) {
        break;
      }
      if (IntroducesSlot(callables.Get(here).virtual_dispatch)) {
        ++ordinal.value;
      }
    }
    return ordinal;
  }
};

}  // namespace lyra::mir
