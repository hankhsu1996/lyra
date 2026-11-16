#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/library_callback.hpp"

namespace lyra::lowering::hir_to_mir {

// The behavior the method `self` answers, read off its stated dispatch role
// and named the way a call names one: an introducer names itself, and an
// override the slot of the behavior it overrides, whichever unit's class
// introduced it. A one-arm dispatch, never a chain walk.
[[nodiscard]] auto CanonicalVirtualSlot(
    mir::CallableId self, const mir::VirtualDispatchRole& role)
    -> mir::VirtualSlot;

// The object a dispatch on `slot` is made on, as the class that introduced the
// behavior, which is the class whose table, or whose row among the interface
// classes the object's class implements, numbers the behavior. The object is
// the same whichever class it is seen as (LRM 8.15, 8.26.5), so only its type
// changes. A receiver already of that class is left as it is.
[[nodiscard]] auto AsIntroducer(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId receiver,
    const mir::VirtualSlot& slot) -> mir::ExprId;

// Which kind of callable of a class a body is, as far as what it takes ahead of
// the formals the source wrote depends on it: an instance member runs on an
// object (LRM 8.6), a type-associated one has none (LRM 8.10), and a
// constructor runs on the object it is building (LRM 8.7).
enum class CallableForm : std::uint8_t {
  kInstanceMember,
  kTypeAssociated,
  kConstructor,
};

// The instance of the structural scope that declares a class (LRM 6.22), as a
// class this unit declares keeps it: the type a construction or a
// type-associated body of the class is handed it as, and the member each object
// records it in for the object's methods to read -- none for an interface
// class, which has no object (LRM 8.26). It is held as the base every scope
// extends because the class is published, and what it publishes may name no
// class the scope keeps to itself.
struct DeclaringInstance {
  mir::TypeId type;
  std::optional<mir::FieldId> member;
};

// The code one body compiles for a class its unit declares, by each callable
// the class took, its construction among them. It is held until where it lands
// is known -- as the class's own definitions, or, in a unit whose instances are
// a tree, as the methods of the body realizing the scope the class's instance
// is, which every place a generate block is built at compiles anew (LRM 27.3)
// while the class itself is stated once. Who declares each callable and the
// part it takes in dispatch the unit recorded when the class took it.
struct CompiledClassCode {
  std::map<mir::CallableId, mir::CallableCode> callables;
};

// The structural portion of a class declaration: the facts a peer needs to read
// about a class while its own body is being lowered. What is carried over to
// `mir::Class` keeps the same semantics under the same name there; the
// executable parts -- the constructor, the callable bodies -- are not
// represented here.
//
// None of this outlives the lowering. A finished unit holds one fully composed
// class per identity, so nothing downstream pairs two views of one class.
struct ClassShape {
  support::ClassName identity;
  std::optional<mir::ClassRef> base;
  // Interface class contracts (LRM 8.26) this class commits to satisfying.
  // Populated from the source `implements` clause of a regular class or the
  // `extends` clause of an interface class; both source keywords name the same
  // object-model relation -- aggregate these interface classes' pure virtual
  // method contracts. Multiple entries are legal; the concrete-base
  // single-value rule stays on `base`, and interface conformance introduces no
  // instance storage.
  std::vector<mir::ClassId> implements;
  mir::TypeId self_pointer_type;
  // The instance this class belongs to, present exactly where the class's
  // declaration says it takes one (LRM 6.22): what each body of the class is
  // handed follows from it. An interface class belongs to one and keeps it in
  // no member. Absent for a scope of the design hierarchy, which is a type of
  // no instance.
  std::optional<DeclaringInstance> declaring_instance;
  std::optional<mir::ScopeDeclarations> scope_declarations;
  base::Arena<mir::ParamDecl, mir::ParamId> ctor_prefix_params;
  base::Arena<mir::FieldDecl, mir::FieldId> fields;
  std::vector<mir::NamedField> named_fields;
  base::Arena<mir::StaticPropertyDecl, mir::StaticPropertyId> static_properties;
  std::vector<mir::NamedStaticProperty> named_static_properties;
  // The bodies the runtime library calls on an object of the class, each under
  // the callback it answers.
  std::map<support::LibraryCallback, mir::CallableId> library_callbacks;
  // Where each static property's cell was placed. It is a home rather than a
  // slot because which pool holds it follows from what replicates the class
  // declaration: a class a namespace unit declares owns its cells, and one a
  // structural scope declares is replicated with that scope, so its cells are
  // fields of the instance (LRM 6.22, 8.9). Either pool also takes what the
  // class's bodies keep for the whole class -- a static-lifetime variable, and
  // what a `disable` naming a block of a method invalidates -- so a property's
  // position in it is not its position in the class.
  base::Translation<hir::StaticPropertyId, StaticStorageHome>
      static_property_translation;
  std::vector<mir::ClassId> contained;
  // Whether the class is final (LRM 8.13). A scope's own class is; the class
  // its unit published of it is extended by that one, and an SV class carries
  // the source-declared value.
  bool is_final = false;
  // Whether this class is an `interface class` declaration (LRM 8.26): its body
  // carries only pure virtual method contracts, no instance storage and no
  // constructor. A class implementing it places nothing of it in its objects
  // (LRM 8.26); what answers its methods is a table of the implementing class.
  bool is_interface = false;

  // Storage the source declared, which the class answers by the identifier the
  // source wrote, and storage the lowering keeps for its own use, which nothing
  // answers. Two calls rather than one with an optional, so a site says which
  // it is building at the moment it builds it.
  auto AddNamedField(std::string name, mir::TypeId type) -> mir::FieldId;
  auto AddField(mir::TypeId type) -> mir::FieldId;

  // The class this declaration becomes: everything settled here carried over
  // verbatim. What is left absent is exactly what the body stage produces --
  // each taken callable's body, the constructor, the static init, what the
  // class conforms to, and what it tells the library about itself. The
  // callables it declares are the unit's, which the class took before any body
  // lowers: taking a callable before its body exists is what lets a call
  // resolve before the callee's body exists (LRM 13.7).
  [[nodiscard]] auto OpenClass() const -> mir::Class;
};

}  // namespace lyra::lowering::hir_to_mir
