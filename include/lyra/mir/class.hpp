#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_alias.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct Class {
  std::string name;
  // The base this object extends, or absent for a plain object that is not a
  // tree node. A backend realizes the base, the registering constructor, and
  // the tree-node overrides only when a base is present; a baseless object is a
  // plain struct.
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
  base::Arena<MemberDecl, MemberId> members;
  // Construction logic, run when the object is allocated. The backend's C++
  // constructor is the allocation shell that kicks this off; an empty body
  // needs no kickoff.
  Block constructor_block;
  base::Arena<Class, ClassId> nested_classes;
  base::Arena<MethodDecl, MethodId> methods;
  std::vector<TypeAliasDecl> type_aliases;

  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
