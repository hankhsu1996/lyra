#pragma once

#include <array>
#include <concepts>
#include <cstddef>
#include <utility>

#include "lyra/value/clear_above_widths.hpp"

namespace lyra::value {

namespace detail {

// One component of a product, told apart from every other by its position, so
// two components of one type are still two bases.
template <std::size_t I, typename T>
struct TupleComponent {
  T value{};
};

// Every component of a product, each a base of its own. A component is then
// reached by converting to the one base its position names, and nothing about
// the product is stated as an expression over all of its components -- which a
// compiler bounds the length of, where a design does not bound how many members
// a structure declares (LRM 7.2).
template <typename Positions, typename... Ts>
struct TupleComponents;

template <std::size_t... Is, typename... Ts>
struct TupleComponents<std::index_sequence<Is...>, Ts...>
    : TupleComponent<Is, Ts>... {
  TupleComponents() = default;

  template <typename... Us>
    requires(sizeof...(Us) == sizeof...(Ts) && sizeof...(Ts) > 0)
  explicit TupleComponents(Us&&... values)
      : TupleComponent<Is, Ts>{std::forward<Us>(values)}... {
  }
};

template <std::size_t I, typename T>
[[nodiscard]] auto ComponentAt(TupleComponent<I, T>& component) -> T& {
  return component.value;
}
template <std::size_t I, typename T>
[[nodiscard]] auto ComponentAt(const TupleComponent<I, T>& component)
    -> const T& {
  return component.value;
}
template <std::size_t I, typename T>
[[nodiscard]] auto ComponentAt(TupleComponent<I, T>&& component) -> T&& {
  return std::move(component).value;
}

}  // namespace detail

// A heterogeneous product value: a positional, fixed list of component value
// types, each reached by its declaration-order index. It holds the components
// and copies, moves and ends them with itself; a component that owns
// variable-size storage carries its own copy semantics, so a Tuple copy is a
// shallow copy of its components.
//
// A product a lowering composes for itself -- a task's output pack, an
// associative entry's (key, value) pair -- is this and nothing more, since
// nothing asks a whole-value operation of one. A structure the source declares
// (LRM 7.2) is a type of its declaring unit built on this one, whose members
// are the methods its declaration states for those operations.
template <typename... Ts>
class Tuple {
 public:
  Tuple() = default;

  // Each component is built where it lies, copied or moved from what it is
  // handed, so a large one is never first made a copy of its own on the way
  // in. A product of no components is a value like any other and its only form
  // is the default one, so the constructor that takes components is declared
  // only where there are components to take.
  template <typename... Us>
    requires(
        sizeof...(Us) == sizeof...(Ts) && sizeof...(Ts) > 0 &&
        (std::constructible_from<Ts, Us &&> && ...))
  explicit Tuple(Us&&... values) : data_(std::forward<Us>(values)...) {
  }

  // Component `I`, by declaration-order index, in the product's own value
  // category: one to read through a product that may not be changed, one to
  // write or reach further through, and one to move out of an expiring
  // product. Every component of a product is live at once, so reaching one
  // settles nothing about the others, and reading and writing it are the one
  // access.
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> decltype(auto) {
    return detail::ComponentAt<I>(data_);
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() & -> decltype(auto) {
    return detail::ComponentAt<I>(data_);
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() && -> decltype(auto) {
    return detail::ComponentAt<I>(std::move(data_));
  }

  // Each component's positions above its widths made clear, through a table
  // for the reason the components are bases.
  void ClearAboveWidths() {
    using Clear = void (*)(Tuple&);
    static constexpr auto kClears =
        []<std::size_t... I>(std::index_sequence<I...>) {
          return std::array<Clear, sizeof...(I)>{+[](Tuple& product) {
            lyra::value::ClearAboveWidths(product.template Component<I>());
          }...};
        }(std::index_sequence_for<Ts...>{});
    for (const Clear clear : kClears) {
      clear(*this);
    }
  }

 private:
  detail::TupleComponents<std::index_sequence_for<Ts...>, Ts...> data_;
};

}  // namespace lyra::value
