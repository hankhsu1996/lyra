#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/foreign_linkage.hpp"

namespace lyra::mir {

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
//     user's C calls. A foreign name is program-global and belongs to no class
//     (LRM 35.4, 35.7), so only a unit's own callables ever carry one.
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
//
// Access is not stated here. A scope draws no access boundary at all --
// anything lexically inside a module reaches its subroutines, and a
// hierarchical name reaches them from outside it (LRM 23.8) -- and a class's
// own `local` / `protected` (LRM 8.9) is a source-declared, three-valued fact
// over members of every kind, which is a different thing than a callable-only
// flag.
// A callable carries no name. Its identity is the position its declaration
// sits at, which every callable has and which is already what an intra-unit
// reference names; being reachable by a name is a separate relation, held by
// whatever answers that name (`NamedCallable` below). A body the source never
// wrote simply does not take part in it, rather than taking part with nothing
// in the slot -- and it needs no name of the compiler's own, which is as well,
// since a SystemVerilog identifier admits every printable character but white
// space (LRM 5.6.1) and so leaves no spelling reserved to mint one from.
struct CallableDecl {
  CallableCode code;
  std::optional<ForeignLinkage> foreign;
  std::optional<VirtualDispatchRole> virtual_dispatch;
};

// One entry of the relation between a name space and the bodies it answers: the
// identifier written in the source, and the body it reaches. A class's methods
// and a namespace's subroutines are each such a relation, held by the class or
// the unit rather than by the bodies, so the bodies nothing names carry
// nothing.
struct NamedCallable {
  std::string name;
  CallableId body;
};

// The identifier `body` answers to among `named`, or nothing where nothing
// names it. Answering nothing is the answer, not a case to work around.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedCallable> named, CallableId body)
    -> std::optional<std::string_view> {
  for (const NamedCallable& entry : named) {
    if (entry.body == body) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

// The lifecycle entries the runtime drives are reached through the definition
// itself and answer to no name.
struct UnpublishedEntry {
  auto operator==(const UnpublishedEntry&) const -> bool = default;
};

// The entry answers to the SV identifier its subroutine was declared under, so
// a hierarchical name that reaches it is resolved against the scope while the
// design elaborates (LRM 23.6, 23.8.1). Every subroutine a scope declares
// answers this way, because what reaches one is a name the declaring unit never
// promised and so never knew to expect.
struct SubroutineEntry {
  std::string name;

  auto operator==(const SubroutineEntry&) const -> bool = default;
};

// Which name, if any, the runtime holds this entry under beside the scope's own
// lifecycle. A DPI-C export's name is one program-global symbol (LRM 35.4)
// while its subroutine is compiled once per specialization of the declaring
// scope; an SV subroutine's is the identifier a hierarchical name spells. The
// two are separate namespaces and one declaration may answer in both.
using AbiAdapterPublication =
    std::variant<UnpublishedEntry, ForeignLinkage, SubroutineEntry>;

// A class-owned callable whose identity is a plain function pointer the runtime
// library holds and calls back through -- the shape a lifecycle hook taking the
// scope it runs on requires, and the shape a name answered at elaboration hands
// back. Structurally a distinct callable species from `CallableDecl`: its
// receiver is an explicit parameter (never bound implicitly), it participates
// in no dispatch table, and it is never named as a callee, only reached as a
// code address. A backend renders it in the target language's
// function-pointer-compatible form, which is not the form an instance method
// takes.
//
// Its position in the class's own arena is the whole of its identity; what it
// is spelled as in a target language is that target's to mint. The name it
// answers to elsewhere -- a hierarchical name, or the DPI-C name space -- is
// `published`, which says which name space holds it rather than what it is
// called here.
struct AbiAdapter {
  CallableCode code;
  AbiAdapterPublication published;
};

}  // namespace lyra::mir
