#pragma once

#include <utility>

#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

namespace detail {

template <typename Fn>
void WalkScopePreOrderImpl(Scope& root, Fn& fn) {
  fn(root);
  root.ForEachChild([&fn](Scope& child) { WalkScopePreOrderImpl(child, fn); });
}

}  // namespace detail

template <typename Fn>
void WalkScopePreOrder(Scope& root, Fn&& fn) {
  auto&& f = std::forward<Fn>(fn);
  detail::WalkScopePreOrderImpl(root, f);
}

}  // namespace lyra::runtime
