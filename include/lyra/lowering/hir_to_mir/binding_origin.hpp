#pragma once

#include <cstdint>

#include "lyra/hir/procedural_var.hpp"

namespace lyra::lowering::hir_to_mir {

// The cross-body identity of a capturable binding: the stable key under which a
// binding is deduplicated in a body and forwarded across callable boundaries.
// It is independent of any one body's physical slot (a `LocalId` or a
// closure-record field), so the same origin matches in the declaring body and
// in every closure that captures it. The receiver is not a distinct kind of
// binding -- it is the receiver-role parameter, identified by `Receiver()`. The
// runtime services handle is the same shape for a receiver-less callable (a
// package function or task, LRM 26.3): a role-tagged leading parameter, the
// ambient engine handle a body reaches when an instance callable would reach it
// through `self`, identified by `Services()`.
struct BindingOriginId {
  enum class Kind : std::uint8_t {
    kReceiver,
    kServices,
    kSourceProcedural,
    kIterator,
    kSynthesized,
  };

  Kind kind{};
  // SourceProcedural: the HIR procedural-var id. Iterator: the clause id.
  // Synthesized: the deterministic owner site (e.g. the HIR statement that
  // introduced the carrier).
  std::uint32_t key = 0;
  // Iterator: the role (element vs index). Synthesized: an ordinal within the
  // owner site. Unused otherwise.
  std::uint32_t subkey = 0;

  auto operator<=>(const BindingOriginId&) const = default;

  static auto Receiver() -> BindingOriginId {
    return {.kind = Kind::kReceiver};
  }
  static auto Services() -> BindingOriginId {
    return {.kind = Kind::kServices};
  }
  static auto Procedural(hir::ProceduralVarId var) -> BindingOriginId {
    return {.kind = Kind::kSourceProcedural, .key = var.value};
  }
  static auto Iterator(std::uint32_t clause, std::uint32_t role)
      -> BindingOriginId {
    return {.kind = Kind::kIterator, .key = clause, .subkey = role};
  }
  static auto Synthesized(std::uint32_t owner_site, std::uint32_t ordinal)
      -> BindingOriginId {
    return {.kind = Kind::kSynthesized, .key = owner_site, .subkey = ordinal};
  }
};

}  // namespace lyra::lowering::hir_to_mir
