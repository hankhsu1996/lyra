#pragma once

#include "lyra/common/origin_id.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// RAII guard: sets origin on construction, restores on destruction.
// KEY INVARIANT: If origin is Invalid, do nothing (preserve outer origin).
// This ensures function-level origin is preserved when instruction origin is
// Invalid.
class [[nodiscard]] OriginScope {
 public:
  OriginScope(Context& ctx, common::OriginId origin);
  ~OriginScope();

  OriginScope(const OriginScope&) = delete;
  auto operator=(const OriginScope&) -> OriginScope& = delete;
  OriginScope(OriginScope&&) = delete;
  auto operator=(OriginScope&&) -> OriginScope& = delete;

 private:
  Context& ctx_;
  common::OriginId saved_origin_;
  bool pushed_;
};

// RAII guard for statement-scoped cleanup of owned string temps.
// Destructor emits LyraStringRelease calls for all registered temps.
class StatementScope {
 public:
  explicit StatementScope(Context& ctx);
  ~StatementScope();

  StatementScope(const StatementScope&) = delete;
  auto operator=(const StatementScope&) -> StatementScope& = delete;
  StatementScope(StatementScope&&) = delete;
  auto operator=(StatementScope&&) -> StatementScope& = delete;

 private:
  Context& ctx_;
};

}  // namespace lyra::lowering::mir_to_llvm
