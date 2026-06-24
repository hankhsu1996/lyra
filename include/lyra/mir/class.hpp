#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/time.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_alias.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct Class {
  std::string name;
  TypeId self_pointer_type;
  // The class's resolved time unit and precision (LRM 3.14.2). The emitted
  // class exposes the precision so the engine can take the design-global
  // minimum (LRM 3.14.3) and so delays scale to it.
  TimeResolution time_resolution;
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
  base::Arena<Process, ProcessId> processes;
  base::Arena<Class, ClassId> nested_classes;
  base::Arena<MethodDecl, MethodId> methods;
  std::vector<TypeAliasDecl> type_aliases;

  void AddTypeAlias(TypeAliasDecl decl) {
    type_aliases.push_back(std::move(decl));
  }
};

}  // namespace lyra::mir
