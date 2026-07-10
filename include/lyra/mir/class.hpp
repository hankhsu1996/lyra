#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/method.hpp"
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

// The structural portion of a class declaration: the fields a peer needs to
// read about a class while its own body is being lowered. Each field has the
// same semantics as the same-named field on `Class`; the executable parts
// (`constructor_block`, `methods`) are not represented here.
struct ClassShape {
  std::string name;
  std::optional<ClassRef> base;
  TypeId self_pointer_type;
  TimeResolution time_resolution;
  base::Arena<ParamDecl, ParamId> ctor_prefix_params;
  base::Arena<ParamDecl, ParamId> params;
  base::Arena<FieldDecl, FieldId> fields;
  std::vector<ClassId> contained;
  std::vector<TypeAliasDecl> type_aliases;
};

struct Class {
  std::string name;
  // The base this object extends, or absent if it extends nothing. A consumer
  // resolves it to the base contract -- the base type that renders the
  // inheritance, whether the object is a runtime tree node, the constructor
  // prefix it forwards. A runtime tree node realizes the registering
  // constructor and the lifecycle overrides; an object that extends nothing is
  // a plain struct.
  std::optional<ClassRef> base;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
  // The C++ ctor params that forward to the runtime base ctor -- the
  // `(parent, segment, services)` trio for a Scope-derived class, the
  // empty list for a baseless plain object. The lowering populates them
  // by reading the runtime SDK contract for the chosen base; the render
  // walks the list to emit the prefix signature and the base init list
  // generically. Decoupling type names from the render layer: the only
  // place that knows "Scope* / HierarchySegment / RuntimeServices&" mean
  // those C++ tokens is the MIR-level RenderTypeAsCpp dispatch.
  base::Arena<ParamDecl, ParamId> ctor_prefix_params;
  // Structural ctor params that also install a same-named member field
  // (a genvar binding, an upward-class param). Each renders as a ctor
  // param after the prefix list and as a `field(param)` mem-init entry.
  base::Arena<ParamDecl, ParamId> params;
  base::Arena<FieldDecl, FieldId> fields;
  // Construction logic, run when the object is allocated. A callable like any
  // other: `params[0]` is the receiver `self`, body locals live in its `locals`
  // arena. The backend's C++ constructor is the allocation shell that kicks
  // this off; an empty body needs no kickoff.
  CallableCode constructor;
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
  // The class-level static constants this class owns, emitted as static
  // members. A runtime scope's generated-behavior record is one such constant;
  // the constructor forwards its address to the runtime base through
  // `base_init`.
  base::Arena<StaticConstantDecl, StaticConstantId> static_constants;
  // The class-level static callables this class owns -- receiver-less callables
  // in its associated namespace, the callable peer of the static constants. A
  // DPI-C import (LRM 35.4) lives here as an external-symbol callable; nothing
  // in the instance method arena is one. Empty for a class that declares no
  // static callable.
  base::Arena<StaticCallableDecl, StaticCallableId> static_callables;
  // The base-constructor arguments beyond the forwarded prefix params, as
  // ordinary MIR expressions the backend translates in order (their expression
  // tree lives in `constructor.body.exprs`). A runtime tree node passes the
  // address of its generated-behavior constant here; a plain object passes
  // none.
  std::vector<ExprId> base_init;
  std::vector<TypeAliasDecl> type_aliases;

  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
