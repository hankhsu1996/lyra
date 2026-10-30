#pragma once

namespace conformance::framework {

// One visitor made of several lambdas, each naming its alternative's exact
// type, so a variant that gains an alternative fails to compile at every visit
// that does not say what the new one means.
template <class... Ts>
struct Overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

}  // namespace conformance::framework
