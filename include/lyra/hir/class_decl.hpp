#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/random_qualifier.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

// A source-written property initializer (LRM 8.7): the assignment that
// runs against the receiver during construction, before the user
// constructor body, on the property named by `target`. `value` lives in
// the constructor body's expression arena so an initializer that reads
// another property or a formal parameter reaches the same procedural var
// the body would.
struct FieldInit {
  FieldId target;
  ExprId value;

  auto operator==(const FieldInit&) const -> bool = default;
};

// A source-written static property initializer (LRM 8.9 / 10.5): the
// assignment that runs once before any initial or always procedure, on the
// static property named by `target`. Distinct from `FieldInit` because it runs
// once for the cell rather than once per instance constructed. `value` lives in
// the arena below, separate from the constructor body's, because the
// initializer cannot read a per-instance formal or self and must not share
// the constructor body's expression identities.
struct StaticPropertyInit {
  StaticPropertyId target;
  ExprId value;

  auto operator==(const StaticPropertyInit&) const -> bool = default;
};

// A construction-protocol fact (LRM 8.7): the arguments the base class's
// constructor is entered with, before this class's own body runs. Its
// expressions live in the enclosing constructor body's expression arena, so an
// argument that reads a formal parameter reaches the same procedural var the
// body would. The list is complete however the source arrived at it -- an
// explicit `super.new(...)`, the arguments on an extends specifier (LRM 8.17),
// or none where the base constructor declares no formal -- so an empty list
// means the base takes nothing rather than that nobody computed one.
//
// Entering the base is a construction of it, so it carries what any
// construction of the base carries: the instance an object of it belongs to
// (LRM 6.22). `declaring_instance` is how this class's own declaring scope
// reaches the base's, and a name in the base's bodies is searched outward from
// there (LRM 23.9) whichever class extends it. Present exactly where the base
// takes that instance, which is the base's own declaration to say.
struct BaseCall {
  std::optional<DeclaringInstanceReach> declaring_instance;
  std::vector<ExprId> arguments;

  auto operator==(const BaseCall&) const -> bool = default;
};

// One behavior of an interface class a class answers (LRM 8.26.2), and the
// behavior of the class's own lineage that answers it. The front end requires
// the implementation to be virtual, so it is a behavior of the lineage, and a
// class extending this one that overrides that behavior answers the
// interface's with its own body too. Both are named the way an override names a
// behavior. Nothing answers it where the class is abstract and leaves the
// implementation to a class extending it.
struct ConformingBehavior {
  ClassMethodTarget interface_behavior;
  std::optional<ClassMethodTarget> answered_by;

  auto operator==(const ConformingBehavior&) const -> bool = default;
};

// A method a class answers an interface class's behavior with where it inherits
// the implementation rather than declaring one (LRM 8.26.2): its body enters
// that implementation, and it overrides the implementation's behavior. No
// declaration of the source stands for it, so it is no member of the class and
// takes no position among them.
struct InterfaceForward {
  SubroutineDecl method;
  ClassMethodTarget overrides;

  auto operator==(const InterfaceForward&) const -> bool = default;
};

// The bodies of a class this unit declares (LRM 8): what lowering the class
// compiles, beside what the class declares, which every unit reading it asks
// about and which the class's declaration states -- what it extends and
// implements, whether it is an interface class, what each method is in
// dispatch. A class is a scope of the name tree (LRM 23.9), and the scope that
// declares it says so -- a class declared inside a structural scope is a type
// of that scope's instance (LRM 6.22), so its bodies name that instance's
// declarations and count their hops from it. Each method (LRM 8.6) is a
// subroutine reached through the object, reading the receiver and the class's
// properties through it. A method's identity exists before its lowered form
// does: one method's body may name another the source declared later
// (LRM 13.7), and a method declared `extern` has its body outside the class
// body entirely (LRM 8.24). Every member is at the position the class's
// declaration gives it, so the declaration and the bodies are read with one
// coordinate: a property's is where the class places it, and a method's is its
// position among the methods the class declares.
//
// `conforming` states, for every behavior of every interface class a value of
// this class is also a value of (LRM 8.26.5) -- the ones it names, the ones
// those extend, and the ones the class it extends is (LRM 8.26) -- which
// behavior of this class's lineage answers it. An interface class answers
// nothing and states none.
//
// `constructor` is the class's `new` (LRM 8.7). Every regular class has
// one: the user-written `function new` when the source declares it,
// otherwise a synthesized empty constructor, matching the implicit `new`
// the LRM provides when the source omits one. An interface class carries
// a synthesized stub that no consumer invokes -- an interface class
// object cannot be constructed (LRM 8.26.5).
//
// `base_call` peers with `constructor` because forwarding to the base is a
// class-level construction fact, not a statement inside the ctor body (LRM
// 8.7) -- the source may state it nowhere, or on the extends specifier, and it
// happens either way. Any argument expression referencing the ctor's own
// formals lives in that body's arena. It is read only where the class extends
// another, and is complete there: a class that extends another enters that
// base's construction, so "nobody computed the arguments" is not a state this
// reaches.
//
// `field_inits` peers with `constructor` for the same reason: an initializer
// executes as part of the class's construction protocol before the user
// body's statements, and its expressions live in the constructor body's
// arena. Only fields the source declared with an explicit `= value` appear
// here; a field without one takes its type's Table 7-1 default at
// construction.
//
// `static_init` is the arena hosting each static property's initializer
// expression tree (LRM 10.5), kept separate from the constructor body because
// such an initializer runs once for the cell rather than once per instance
// constructed. It is a bare `ProceduralBody` -- no `self`, no receiver --
// because the code carries no per-instance context. Where those expressions
// run is not stated here: it follows what replicates the class declaration,
// which is the declaring scope's to answer.
//
// `static_property_inits` pairs each source-written `= value` initializer
// with its static property in source order; expressions live in
// `static_init.exprs`. A static property without a source initializer takes
// its type's Table 7-1 default and does not appear here.
//
// `fixed_name` is the class's name as far as its text fixes it (LRM
// 21.2.1.5): the class and whatever declares it short of an instance.
//
// `procedural_scopes` owns the lexical declaration scopes (LRM 9.3.4) of
// every body this class declares -- each method's root scope and the
// begin/end, fork, and foreach scopes nested inside it. A class owns its
// methods' declaration scopes the same way a structural scope owns its
// processes', so a body reaches its scope tree the same way wherever it was
// declared.
struct ClassDecl {
  std::vector<ConformingBehavior> conforming;
  base::Registry<SubroutineDecl, MethodId> methods;
  std::vector<InterfaceForward> interface_forwards;
  SubroutineDecl constructor;
  BaseCall base_call;
  std::vector<FieldInit> field_inits;
  ProceduralBody static_init;
  std::vector<StaticPropertyInit> static_property_inits;
  base::Registry<ProceduralScopeDecl, ProceduralScopeId> procedural_scopes;
  FixedName fixed_name;

  auto operator==(const ClassDecl&) const -> bool = default;
};

}  // namespace lyra::hir
