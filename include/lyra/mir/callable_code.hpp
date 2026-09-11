#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The code of a callable: a signature, and a body where this declaration also
// defines it. The binding arena is the callable's, not a per-block one:
// `locals` holds every activation local and parameter of the whole body, and a
// `LocalRef` resolves against it directly, with no hops. `params` is a prefix
// of `locals` in signature order; for an instance callable `params[0]` is the
// receiver, absent for a static one. `result_type` is the call protocol -- a
// coroutine type for a time-consuming task or process, a value or void type for
// a zero-time function -- carrying the completion payload.
//
// An absent `body` is a declaration this program does not define: a DPI-C
// import, whose definition the user's C provides (LRM 35.4), or a pure virtual
// method, which the deriving class supplies (LRM 8.21). It is distinct from a
// present but empty body, which is a legal definition that does nothing (LRM
// 8.21 note), and the distinction is the presence of the body itself -- no tag
// restates it.
//
// A closure's invoke body has no `params[0]` receiver in its signature; instead
// its `locals[0]` is the closure receiver, a read-only borrow of the closure,
// and a captured binding is read as a field access over it. A directly invoked
// callable receives every parameter from the caller; a closure binds its
// captured fields at construction and supplies the per-invocation `params` at
// each call. A backend reads each binding's name and type from `locals`.
struct CallableCode {
  std::vector<LocalId> params;
  TypeId result_type;
  base::Arena<LocalDecl, LocalId> locals;
  // What the source called the locals it declared. A local the lowering added
  // for its own working takes no part, so what a reader sees named is what the
  // design wrote.
  std::vector<NamedLocal> named_locals;
  std::optional<Block> body;

  // A code skeleton for a callable this program defines: the body starts
  // present and empty, ready for a builder to append to. A bodyless form leaves
  // `body` default-constructed instead, so the safe default is the one whose
  // mistake is loud -- a builder that forgets this trips over the missing body
  // at once, where a builder that forgot to clear an always-present body would
  // silently publish an empty definition.
  [[nodiscard]] static auto Defined() -> CallableCode {
    CallableCode code{};
    code.body.emplace();
    return code;
  }

  // A local the source declared, which the body answers by the identifier the
  // source wrote, and a local the lowering keeps for its own working, which
  // nothing answers. Two calls rather than one with an optional, so a site says
  // which it is building at the moment it builds it.
  auto AddNamedLocal(std::string name, TypeId type) -> LocalId {
    const LocalId id = locals.Add(LocalDecl{.type = type});
    named_locals.push_back(NamedLocal{.name = std::move(name), .local = id});
    return id;
  }

  auto AddLocal(TypeId type) -> LocalId {
    return locals.Add(LocalDecl{.type = type});
  }

  // Whether the signature declares a receiver: `params[0]`, if present, is
  // typed as the enclosing class's self-pointer. The single structural check
  // that both the code-declaration render and the call-site render read to
  // pick between the instance form (with receiver) and the static form
  // (without), without any side flag restating what the params list already
  // fixes.
  [[nodiscard]] auto HasReceiver(TypeId self_pointer_type) const -> bool {
    return !params.empty() && locals.Get(params[0]).type == self_pointer_type;
  }

  // The body of a callable this program defines. A builder that is filling a
  // body in has already established that there is one, so reaching a missing
  // body here is a lowering that lost track of which form it was building.
  [[nodiscard]] auto Body() -> Block& {
    if (!body.has_value()) {
      throw InternalError("CallableCode::Body: this callable has no body");
    }
    return *body;
  }

  [[nodiscard]] auto Body() const -> const Block& {
    if (!body.has_value()) {
      throw InternalError("CallableCode::Body: this callable has no body");
    }
    return *body;
  }
};

}  // namespace lyra::mir
