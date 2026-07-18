#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// A class-level static constant: a named immutable value the class owns with
// static storage, built once at compile time from `value` (the root of the
// expression tree `body` owns; its `stmts` are empty, only `exprs` is used).
// The data dual of a static method. A runtime scope's generated-behavior record
// (its `ScopeProgram`, or `UnitDefinition` for a unit instance) is one such
// constant; the constructor hands its address to the runtime base.
struct StaticConstantDecl {
  std::string name;
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
// The source-written `= value` initializer, when present, is a design-init
// fact (LRM 10.5): the assignment lands as an `AssignExpr` statement in the
// class's `static_init` body, not on this declaration -- initializer timing
// (once at program startup, before any initial / always) and per-cell
// identity are separate concerns, and the class-wide statement list is the
// one home a backend reads for construction-time state.
struct StaticPropertyDecl {
  std::string name;
  TypeId type;
};

// A constructor's invocation of its base's constructor. Every arg evaluates in
// the enclosing constructor's local scope. The callee identity is implicit --
// it is the base declared on the class this ConstructorDecl belongs to; there
// is exactly one base and exactly one constructor per class, so restating the
// callee here would duplicate structure the class already fixes. Absent when
// the class extends nothing.
struct BaseInit {
  std::vector<ExprId> args;
};

// The class's construction protocol. The constructor is a bare body block the
// class owns directly, not a member of the callable arena: it is never a call
// target and never dispatches, so it carries no callable identity. `code` runs
// the constructor body; the construction facts the body does not itself express
// -- how the base is initialized and how a field gets its pre-body initializer
// -- live alongside it. A consumer that emits construction reads `base_init`
// and `member_inits` in declaration order, then descends into `code`.
struct ConstructorDecl {
  CallableCode code;
  std::optional<BaseInit> base_init;
  std::vector<FieldInit> member_inits;
};

// The declaration-facing view of a callable: the facts a peer's body lowering
// needs to know about it while its own body is being lowered. The callable's
// body itself is not here; the finished `CallableDecl` on the class carries the
// body once every body composes. `virtual_dispatch` is here so a peer that
// calls this callable picks between direct and virtual invocation from a stated
// fact, with no dependency on which class's body lowered first.
struct CallableSignature {
  std::optional<VirtualDispatchRole> virtual_dispatch;
};

// The structural portion of a class declaration: the facts a peer needs to
// read about a class while its own body is being lowered. Each field has the
// same semantics as the same-named field on `Class`; the executable parts
// (`constructor`, callable bodies) are not represented here.
struct ClassShape {
  std::string name;
  std::optional<ClassRef> base;
  TypeId self_pointer_type;
  TimeResolution time_resolution;
  base::Arena<ParamDecl, ParamId> ctor_prefix_params;
  base::Arena<FieldDecl, FieldId> fields;
  base::Arena<StaticPropertyDecl, StaticPropertyId> static_properties;
  base::Arena<CallableSignature, CallableId> callable_signatures;
  std::vector<ClassId> contained;
  // Whether the class occupies a node of the runtime object tree -- a module
  // instance, a named generate scope, or a named procedural block. A backend
  // that walks the tree for emission uses this to skip classes that emit
  // standalone.
  bool is_scope_tree_node = false;
  // Whether the class is final (LRM 8.13). A structural class always is; an
  // SV class carries the source-declared value.
  bool is_final = false;
};

struct Class {
  std::string name;
  std::optional<ClassRef> base;
  bool is_scope_tree_node = false;
  bool is_final = false;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  base::Arena<FieldDecl, FieldId> fields;
  // The class's construction protocol: the constructor body together with the
  // base and per-field initialization not expressible in that body.
  ConstructorDecl constructor;
  // The classes this one structurally owns -- the children it builds and, for a
  // backend that nests, emits inside itself. Each names a registry identity, in
  // construction order. Ownership of the declarations is the unit's registry;
  // this is the containment relation over those identities.
  std::vector<ClassId> contained;
  // The compiler-generated structs this class nests -- a promoted automatic
  // scope synthesized while lowering one of this class's bodies (its own, or a
  // closure emitted inline within it). Each names a `StructId` in the unit's
  // struct registry; this is the emission-nesting relation over those
  // identities, parallel to `contained`. A backend that nests emits each struct
  // inside this class by iterating this list -- no walk over the body tree.
  std::vector<StructId> structs;
  // Every callable this class owns, in one arena: instance methods (LRM 8.6),
  // process and lifecycle bodies, and the receiver-less static callables (a
  // DPI-C import, LRM 35.4; a static method, LRM 8.10). An instance method
  // carries `self` as its first parameter and a static callable omits it,
  // but both are one `CallableDecl` reached by one `CallableId` -- the
  // receiver is a property of the signature, not a separate declaration
  // space. The constructor is not here: it is a bare body block on the
  // protocol, never a call target.
  base::Arena<CallableDecl, CallableId> callables;
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
  // The class-level design-init body (LRM 10.5): a receiver-less callable the
  // runtime invokes once at program startup, before any initial or always
  // procedure and before any instance's constructor. Its body is a sequence
  // of `AssignExpr` statements, one per source-written static property
  // initializer in declaration order; a static property without a source
  // initializer takes its type's Table 7-1 default and gets no statement
  // here. Peer to `constructor.code` on the axis "code the runtime runs at
  // construction time," but per-class rather than per-instance and
  // signature-less: `code.params` is empty (no `self`, no formals), and
  // `code.result_type` is void.
  CallableCode static_init;
  // The foreign-linkage wrappers this class exposes -- one per `export "DPI-C"`
  // of one of its callables (LRM 35.5). Each is an external entry a backend
  // materializes beside the class; empty for a class that exports nothing.
  std::vector<ForeignExportWrapper> foreign_export_wrappers;
};

}  // namespace lyra::mir
