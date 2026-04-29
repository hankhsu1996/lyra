#pragma once

#include <utility>

#include "lyra/runtime/runtime_scope.hpp"

namespace lyra::runtime {

namespace detail {

template <typename Fn>
void WalkScopePreOrderImpl(RuntimeScope& root, Fn& fn) {
  fn(root);
  root.ForEachChild(
      [&fn](RuntimeScope& child) { WalkScopePreOrderImpl(child, fn); });
}

template <typename Fn>
void WalkScopePreOrderImpl(const RuntimeScope& root, Fn& fn) {
  fn(root);
  root.ForEachChild(
      [&fn](const RuntimeScope& child) { WalkScopePreOrderImpl(child, fn); });
}

}  // namespace detail

template <typename Fn>
void WalkScopePreOrder(RuntimeScope& root, Fn&& fn) {
  auto&& f = std::forward<Fn>(fn);
  detail::WalkScopePreOrderImpl(root, f);
}

template <typename Fn>
void WalkScopePreOrder(const RuntimeScope& root, Fn&& fn) {
  auto&& f = std::forward<Fn>(fn);
  detail::WalkScopePreOrderImpl(root, f);
}

}  // namespace lyra::runtime
