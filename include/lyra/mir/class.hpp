#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/method_id.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/static_callable.hpp"
#include "lyra/mir/static_callable_id.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type_alias.hpp"
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

// A constructor's invocation of its base's constructor. Every arg evaluates in
// the enclosing constructor's local scope. The callee identity is implicit --
// it is the base declared on the class this ConstructorDecl belongs to; there
// is exactly one base and exactly one constructor per class, so restating the
// callee here would duplicate structure the class already fixes. Absent when
// the class extends nothing.
struct BaseInit {
  std::vector<ExprId> args;
};

// The class's construction protocol -- distinct from the callable code that
// runs the constructor body. The body is an ordinary callable stored beside
// the other class methods, reached by the `method` index; construction facts
// the body does not itself express -- how the base is initialized and how a
// field gets its pre-body initializer -- live here. A consumer that emits
// construction reads `base_init` and `member_inits` in declaration order,
// then descends into the method body.
struct ConstructorDecl {
  MethodId method;
  std::optional<BaseInit> base_init;
  std::vector<FieldInit> member_inits;
};

// The declaration-facing view of a method: the facts a peer's body lowering
// needs to know about this method while its own body is being lowered. The
// method's body itself is not here; the finished `MethodDecl` on the class
// carries the body once every body composes. `virtual_dispatch` is here so
// a peer that calls this method picks between direct and virtual invocation
// from a stated fact, with no dependency on which class's body lowered
// first.
struct MethodSignature {
  std::optional<VirtualDispatchRole> virtual_dispatch;
};

// The structural portion of a class declaration: the facts a peer needs to
// read about a class while its own body is being lowered. Each field has the
// same semantics as the same-named field on `Class`; the executable parts
// (`constructor`, method bodies) are not represented here.
struct ClassShape {
  std::string name;
  std::optional<ClassRef> base;
  TypeId self_pointer_type;
  TimeResolution time_resolution;
  base::Arena<ParamDecl, ParamId> ctor_prefix_params;
  base::Arena<FieldDecl, FieldId> fields;
  base::Arena<MethodSignature, MethodId> method_signatures;
  std::vector<ClassId> contained;
  std::vector<TypeAliasDecl> type_aliases;
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
  // The class's construction protocol. The callable code that runs the ctor
  // body is stored beside the other methods; base and per-field
  // initialization not expressible in that body live on the protocol itself.
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
  base::Arena<MethodDecl, MethodId> methods;
  // The runtime-callback adapters this class owns -- callables whose
  // identity is a plain function pointer the runtime holds, semantically
  // distinct from the instance methods above. Referenced from the class's
  // generated-behavior constant by `FunctionRef`; never called through a
  // MIR CallExpr. Empty for a class that has no runtime callback surface.
  base::Arena<AbiAdapter, AbiAdapterId> abi_adapters;
  // The class-level static constants this class owns, emitted as static
  // members. A runtime scope's generated-behavior record is one such constant;
  // the constructor forwards its address to the runtime base through the
  // construction protocol.
  base::Arena<StaticConstantDecl, StaticConstantId> static_constants;
  // The class-level static callables this class owns -- receiver-less callables
  // in its associated namespace, the callable peer of the static constants. A
  // DPI-C import (LRM 35.4) lives here as an external-symbol callable; nothing
  // in the instance method arena is one. Empty for a class that declares no
  // static callable.
  base::Arena<StaticCallableDecl, StaticCallableId> static_callables;
  // The foreign-linkage wrappers this class exposes -- one per `export "DPI-C"`
  // of one of its methods (LRM 35.5). Each is an external entry a backend
  // materializes beside the class; empty for a class that exports nothing.
  std::vector<ForeignExportWrapper> foreign_export_wrappers;
  std::vector<TypeAliasDecl> type_aliases;

  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

// The ctor's callable code. A convenience for consumers that read the ctor
// as an opaque callable rather than dereferencing the construction
// protocol's method index by hand.
//
// Read-only: the arena backing the method is append-only, and the ctor is
// finalized before insertion, so any post-add mutation would violate value
// immutability.
inline auto GetConstructorCode(const Class& c) -> const CallableCode& {
  return c.methods.Get(c.constructor.method).code;
}

}  // namespace lyra::mir
