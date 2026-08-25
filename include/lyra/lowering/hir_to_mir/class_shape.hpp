#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The declaration-facing view of a callable: the facts a peer's body lowering
// needs to know about it while its own body is being lowered. The callable's
// body itself is not here; the finished `mir::CallableDecl` on the class
// carries the body once every body composes. `virtual_dispatch` is here so a
// peer that calls this callable picks between direct and virtual invocation
// from a stated fact, with no dependency on which class's body lowered first.
struct CallableSignature {
  std::optional<mir::VirtualDispatchRole> virtual_dispatch;
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
  std::string name;
  std::optional<mir::ClassRef> base;
  // Interface class contracts (LRM 8.26) this class commits to satisfying.
  // Populated from the source `implements` clause of a regular class or the
  // `extends` clause of an interface class; both source keywords name the same
  // object-model relation -- aggregate these interface classes' pure virtual
  // method contracts. Multiple entries are legal; the concrete-base
  // single-value rule stays on `base`, and interface conformance introduces no
  // instance storage.
  std::vector<mir::ClassRef> implements;
  mir::TypeId self_pointer_type;
  TimeResolution time_resolution;
  base::Arena<mir::ParamDecl, mir::ParamId> ctor_prefix_params;
  base::Arena<mir::FieldDecl, mir::FieldId> fields;
  base::Arena<mir::StaticPropertyDecl, mir::StaticPropertyId> static_properties;
  // Which pass owns the callable identity space depends on the entity: an SV
  // class's methods are named by another class's declaration (an override
  // states the slot its base declared, LRM 8.20), so they are taken before any
  // declaration settles; a structural scope's callables are named by no
  // declaration, so the scope settles them itself.
  base::Translation<mir::CallableId, CallableSignature> callable_signatures;
  // Where each of the class's HIR declarations landed in the pools above. The
  // two id spaces are separate: the MIR field pool also takes the
  // static-lifetime storage the class's bodies declare (LRM 13.3.1), so a
  // property's position in it is not its HIR position.
  base::Translation<hir::FieldId, mir::FieldId> field_translation;
  base::Translation<hir::StaticPropertyId, mir::StaticPropertyId>
      static_property_translation;
  std::vector<mir::ClassId> contained;
  // Whether the class is final (LRM 8.13). A structural class always is; an SV
  // class carries the source-declared value.
  bool is_final = false;
  // Whether this class is an `interface class` declaration (LRM 8.26): its body
  // carries only pure virtual method contracts, no instance storage and no
  // constructor. Consumers read the bit to render the class as an abstract
  // target-language type and to route inheritance through the multi-base
  // mechanism `implements` names.
  bool is_interface_class = false;

  // The class this declaration becomes: everything settled here carried over
  // verbatim, and one reserved callable identity per signature. What is left
  // absent is exactly what the body stage produces -- each reserved callable's
  // body, the constructor, the adapters, the static init.
  //
  // The reservation is what lets a call resolve before the callee's body exists
  // (LRM 13.7): the declaration hands out the identity, the body stage fills
  // it.
  [[nodiscard]] auto OpenClass() const -> mir::Class;
};

}  // namespace lyra::lowering::hir_to_mir
