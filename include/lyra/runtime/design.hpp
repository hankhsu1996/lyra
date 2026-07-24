#pragma once

#include <memory>
#include <utility>

#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

// The elaborated scope tree, owned as a single value. Constructed by the
// generated `$root` builder and handed to the runtime through `BindDesign`;
// the runtime attaches to it for its lifetime and drives elaboration walks
// and Observed-region drain preorder against it.
//
// A distinct type separates "the whole design" from "an arbitrary scope in
// it" -- `BindDesign` cannot accidentally take a mid-tree scope, and
// tree-global traversal helpers land here rather than accumulating on
// `Scope`.
class Design {
 public:
  explicit Design(std::unique_ptr<Scope> root) : root_(std::move(root)) {
  }

  Design(const Design&) = delete;
  auto operator=(const Design&) -> Design& = delete;
  Design(Design&&) = delete;
  auto operator=(Design&&) -> Design& = delete;
  ~Design() = default;

  [[nodiscard]] auto Root() -> Scope& {
    return *root_;
  }
  [[nodiscard]] auto Root() const -> const Scope& {
    return *root_;
  }

  // Preorder walk from `Root()` -- each scope visited before its children,
  // in attach order. Consumers are elaboration walks, Observed region drain,
  // dump paths.
  template <typename Fn>
  void ForEachScope(Fn&& fn) {
    auto&& f = std::forward<Fn>(fn);
    WalkPreOrder(*root_, f);
  }

 private:
  template <typename Fn>
  static void WalkPreOrder(Scope& scope, Fn& fn) {
    fn(scope);
    scope.ForEachChild([&fn](Scope& child) { WalkPreOrder(child, fn); });
  }

  std::unique_ptr<Scope> root_;
};

}  // namespace lyra::runtime
