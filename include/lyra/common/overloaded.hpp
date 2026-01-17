#pragma once

namespace lyra {

// Visitor helper for std::visit with multiple lambdas.
//
// Usage:
//   std::visit(Overloaded{
//       [](const TypeA& a) { ... },
//       [](const TypeB& b) { ... },
//   }, variant);
//
// This replaces the if-constexpr chain pattern and makes it harder to
// forget a case when adding new variant alternatives.

template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

// Deduction guide (required in C++17, optional in C++20)
template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

}  // namespace lyra
