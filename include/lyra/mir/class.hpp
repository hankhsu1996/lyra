#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_alias.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The base class a class extends. A class is a generic object type; whether it
// is a node in the runtime object tree, and which runtime base it inherits, is
// stated here rather than inferred by a backend. `kInstance` is a top-level
// module instance, `kGenScope` a nested generate scope (both runtime Scope
// subclasses). A class with no base is a plain object -- members only, not a
// tree node -- constructed and held directly rather than registered.
enum class RuntimeBaseClass : std::uint8_t {
  kInstance,
  kGenScope,
};

struct Class {
  std::string name;
  // The runtime base this class extends, or absent for a plain object. A
  // backend realizes the base, the registering constructor, and the tree-node
  // overrides only when a base is present; a baseless class is a plain struct.
  std::optional<RuntimeBaseClass> base;
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
  // Cross-instance binding (a `ref` port aliases the child's reference member
  // to the connected variable, LRM 23.3.3.2), run after the whole tree is
  // built. Absent when the scope binds no cross-instance reference.
  std::optional<MethodDecl> resolve;
  // Variable initializers (LRM 6.8), run after resolution so they observe bound
  // values. Absent when the scope has no value initializers.
  std::optional<MethodDecl> initialize;
  // Process activation (LRM 9.2): the scope's startup and shutdown lifecycle
  // registrations for its `initial` / `final` / `always*` / continuous-assign
  // bodies, run after initialization. The bodies are ordinary callables; this
  // body holds their registrations. Absent when the scope has no process.
  std::optional<MethodDecl> activate;
  base::Arena<Class, ClassId> nested_classes;
  base::Arena<MethodDecl, MethodId> methods;
  std::vector<TypeAliasDecl> type_aliases;

  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
