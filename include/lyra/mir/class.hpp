#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
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

// A fixed runtime-library base class. These are object types the runtime
// library provides, not classes of any compilation unit: `kInstance` is a
// module / interface / program instance and `kGenScope` a named generate scope,
// both nodes in the runtime object tree.
enum class RuntimeClassKind : std::uint8_t {
  kInstance,
  kGenScope,
};

// A reference to a runtime-library base class.
struct RuntimeLibraryClassRef {
  RuntimeClassKind kind;

  auto operator==(const RuntimeLibraryClassRef&) const -> bool = default;
};

// A reference to the object type an object extends. A consumer resolves it
// through one path rather than switching a closed classification; a base is a
// runtime-library class.
using ClassRef = std::variant<RuntimeLibraryClassRef>;

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
