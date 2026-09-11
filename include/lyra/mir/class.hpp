#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// A class-level static constant: a named immutable value the class owns with
// static storage, built once at compile time from `value` (the root of the
// expression tree `body` owns; its `stmts` are empty, only `exprs` is used).
// The data dual of a static method. A runtime scope's generated-behavior
// record is one such constant; the constructor hands its address to the
// runtime base.
// It carries no name: nothing in the source declares one, so its position in
// the class's own arena is the whole of its identity, and what it is spelled as
// in a target language is that target's to mint.
struct StaticConstantDecl {
  TypeId type;
  Block body;
  ExprId value;
};

// A class static property (LRM 8.9): a named, mutable type-associated storage
// cell the class owns, shared by every instance. Peer of `FieldDecl` on the
// instance-member axis and of `StaticConstantDecl` on the type-associated
// axis, but a class of its own: unlike `FieldDecl` a static property has no
// per-instance replication, and unlike `StaticConstantDecl` its value is a
// run-time cell writable through ordinary assignment.
// What the cell starts out holding is a design-init fact (LRM 10.5) and is not
// on this declaration: the assignment lands in whatever brings the cell's owner
// up, which is the declaring instance's construction where a structural scope
// replicates the class and the declaring unit's namespace bring-up where
// nothing does. Initializer timing and per-cell identity are separate concerns,
// and a statement list is the one home a backend reads for either.
struct StaticPropertyDecl {
  std::string name;
  TypeId type;
};

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
  // A class always defines its own construction, so this code is always a
  // definition; an empty body is a class that constructs nothing.
  CallableCode code = CallableCode::Defined();
  // What the base's constructor is entered with (LRM 8.7), each argument
  // evaluated in this constructor's own local scope. Which base that is, and
  // whether the class has one, is the class's own declaration. The list is
  // every argument that construction takes, so a consumer forwards it as it
  // stands and supplies nothing of its own.
  std::vector<ExprId> base_args;
};

struct Class {
  std::string name;
  std::optional<ClassRef> base;
  std::vector<ClassRef> implements;
  bool is_final = false;
  bool is_interface_class = false;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  base::Arena<FieldDecl, FieldId> fields;
  ConstructorDecl constructor;
  // The classes this one structurally owns -- the children it builds. Each
  // names a registry identity, in construction order. Ownership of the
  // declarations is the unit's registry; this is the containment relation over
  // those identities. Where a backend places a class is the backend's own
  // affair and is not stated here.
  std::vector<ClassId> contained;
  // Every callable this class owns, in one pool: instance methods (LRM 8.6),
  // process and lifecycle bodies, and the receiver-less static callables (a
  // DPI-C import, LRM 35.4; a static method, LRM 8.10). An instance method
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
  // The runtime-callback adapters this class owns -- callables whose identity
  // is a plain function pointer the runtime holds, semantically distinct from
  // the instance callables above. Referenced from the class's
  // generated-behavior constant by `FunctionRef`; never called through a MIR
  // `CallExpr`. Empty for a class that has no runtime callback surface.
  base::Arena<AbiAdapter, AbiAdapterId> abi_adapters;
  // The class-level static constants this class owns, emitted as static
  // members. A runtime scope's generated-behavior record is one such constant;
  // the constructor forwards its address to the runtime base through the
  // construction protocol.
  base::Arena<StaticConstantDecl, StaticConstantId> static_constants;
  // The class's static properties (LRM 8.9): mutable type-associated storage
  // cells shared by every instance. Peer to `fields` on the instance-versus-
  // type-associated axis: a static property is one cell owned by the type,
  // never a member replicated into each instance.
  base::Arena<StaticPropertyDecl, StaticPropertyId> static_properties;
  // The names this class answers and which body each reaches (LRM 8.3, one
  // name space over a class's members). A method the source declared is here
  // because a call site outside this unit spells it; a body the compiler
  // synthesized is not, because nothing spells one.
  std::vector<NamedCallable> named_callables;
};

}  // namespace lyra::mir
