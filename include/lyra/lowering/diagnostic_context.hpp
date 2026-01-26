#pragma once

#include <concepts>
#include <functional>
#include <optional>
#include <string>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_span.hpp"

namespace lyra::lowering {

// Concept for types that can resolve OriginId to SourceSpan.
// Used to abstract the origin map lookup mechanism.
template <typename T>
concept OriginLookup = requires(const T& lookup, common::OriginId id) {
  {
    lookup.ResolveToSpan(id)
  } -> std::convertible_to<std::optional<SourceSpan>>;
};

// Context for creating diagnostics with resolved source spans.
// Stores a type-erased lookup function so callers don't need to know the
// concrete OriginMap implementation details.
//
// Thread-safety: not thread-safe. Each thread should have its own instance.
class DiagnosticContext {
 public:
  // Construct from any type satisfying OriginLookup concept.
  template <OriginLookup L>
  explicit DiagnosticContext(const L& lookup)
      : resolve_fn_([&lookup](common::OriginId id) {
          return lookup.ResolveToSpan(id);
        }) {
  }

  // Create an Unsupported diagnostic with resolved span.
  // If the origin cannot be resolved, returns UnknownSpan (recoverable for UX
  // but indicates a compiler bug).
  [[nodiscard]] auto MakeUnsupported(
      common::OriginId origin, std::string msg, UnsupportedCategory cat) const
      -> Diagnostic;

  // Create an Error diagnostic with resolved span.
  [[nodiscard]] auto MakeError(common::OriginId origin, std::string msg) const
      -> Diagnostic;

 private:
  std::function<std::optional<SourceSpan>(common::OriginId)> resolve_fn_;
};

}  // namespace lyra::lowering
