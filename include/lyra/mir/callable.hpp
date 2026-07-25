#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/foreign_linkage.hpp"

namespace lyra::mir {

// Whether a callable is part of the object's externally callable surface or an
// internal mechanism. A class instance method (LRM 8.6) is callable through a
// handle, so it is public; a scope's processes, lifecycle hooks, and helper
// subroutines are reached only by the owning runtime or by the scope's own
// bodies, so they are internal. A backend reads which access a callable has
// from here rather than inferring it from the object's base or the callable's
// role.
enum class CallableVisibility : std::uint8_t { kPublic, kInternal };

// A named callable a class or a unit namespace owns. Every SystemVerilog
// function and task, every process body, every synthesized lifecycle body, and
// both directions of the DPI-C boundary are this one concept. What varies among
// them is stated by independent structure, never by a kind:
//
//   - `code` always carries the signature; its body is present exactly when
//     this declaration also defines the callable. A pure virtual method (LRM
//     8.21) and a DPI-C import (LRM 35.4) are the two that do not.
//   - `foreign`, when present, is the C linkage the callable is reached under.
//     It is orthogonal to the body: bodyless plus foreign is an import the
//     user's C defines, bodied plus foreign is the entry point of an export the
//     user's C calls.
//   - `virtual_dispatch`, when present, states this callable's role in the
//     class's dispatch table (LRM 8.20) -- introducing a new slot or overriding
//     an ancestor's -- so a backend renders the marker off stated structure,
//     never re-deriving virtualness by name. It is absent for a direct-only
//     callable: every static callable and every foreign one.
//   - The receiver is not a kind either: an instance method carries `self` as
//     its first parameter and a static callable omits it, which the signature
//     already says.
//
// A pure virtual prototype is therefore the combination of no body and a
// dispatch role, and needs no third fact to say so.
struct CallableDecl {
  std::string name;
  CallableCode code;
  std::optional<ForeignLinkage> foreign;
  std::optional<VirtualDispatchRole> virtual_dispatch;
  CallableVisibility visibility;

  // The name the emitted symbol is reached by. A foreign callable's linkage
  // name is program-global and independent of the SV name it was declared under
  // (LRM 35.4); every other callable is reached by its declared name.
  [[nodiscard]] auto LinkedName() const -> const std::string& {
    return foreign.has_value() ? foreign->foreign_name : name;
  }
};

// A named class-owned callable whose identity is a plain function pointer the
// runtime library holds and calls back through -- the shape a
// `void (*)(RuntimeScopeBase*)` lifecycle hook requires. Structurally a
// distinct callable species from `CallableDecl`: its receiver is an explicit
// parameter (never bound implicitly), it participates in no dispatch table, and
// it is reached only as a code address, never through a `CallExpr`. A backend
// renders it in the target language's function-pointer-compatible form, which
// is not the form an instance method takes.
struct AbiAdapter {
  std::string name;
  CallableCode code;
};

}  // namespace lyra::mir
