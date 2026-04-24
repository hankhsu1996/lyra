#pragma once

namespace lyra::support {

// Visitor helper for std::visit with multiple lambdas.
//
// Usage:
//   std::visit(Overloaded{
//       [](const TypeA& a) { ... },
//       [](const TypeB& b) { ... },
//   }, variant);
//
// Each lambda names its alternative's exact type. If the variant gains a
// new alternative, std::visit fails to find a matching overload at the
// call site, giving a compile-time exhaustiveness check.
template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

}  // namespace lyra::support
