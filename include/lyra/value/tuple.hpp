#pragma once

#include <algorithm>
#include <array>
#include <concepts>
#include <cstddef>
#include <type_traits>
#include <utility>

#include "lyra/value/clear_above_widths.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/pack_element.hpp"

namespace lyra::value {

namespace detail {

// One component of a product, told apart from every other by its position, so
// two components of one type are still two bases.
template <std::size_t I, typename T>
struct TupleComponent : Positioned<I, T> {
  T value{};
};

// Every component of a product, each a base of its own, so a product of any
// length is a list of bases, as libc++ lays out its tuple. libstdc++'s is a
// chain of bases each holding the rest, which nests as deep as the product is
// long.
template <typename Indices, typename... Ts>
struct TupleComponents;

template <std::size_t... Is, typename... Ts>
struct TupleComponents<std::index_sequence<Is...>, Ts...>
    : TupleComponent<Is, Ts>... {
  TupleComponents() = default;

  template <typename... Us>
  explicit TupleComponents(Us&&... values)
      : TupleComponent<Is, Ts>{{}, std::forward<Us>(values)}... {
  }
};

// Whether the values handed to a product's constructor are one product of that
// very type, which is then copied or moved rather than taken as its one
// component's value.
template <typename Product, typename... Us>
constexpr bool kIsTheProductItself = false;
template <typename Product, typename U>
constexpr bool kIsTheProductItself<Product, U> =
    std::same_as<std::remove_cvref_t<U>, Product>;

}  // namespace detail

// A heterogeneous product value: a positional, fixed list of component value
// types, each reached by its declaration-order index. It holds the components
// and copies, moves and ends them with itself; a component that owns
// variable-size storage carries its own copy semantics, so a Tuple copy is a
// shallow copy of its components.
//
// A product a lowering composes for itself -- a task's output pack, an
// associative entry's (key, value) pair, what a tagged union's `void` member
// holds (LRM 7.3.2) -- is this and nothing more, and answers each operation on
// its whole value as its components do, one component after another, so the
// product of no components answers each as a value with nothing to compare
// does. Each operation is instantiated only where it is asked for, so a
// product of components lacking one still holds them. A structure the source
// declares (LRM 7.2) is a type of its declaring unit built on this one, whose
// members are the methods its declaration states for those operations.
template <typename... Ts>
class Tuple {
  using Components =
      detail::TupleComponents<std::index_sequence_for<Ts...>, Ts...>;

 public:
  template <std::size_t I>
  using Element = detail::PackElement<I, Components>;

  Tuple() = default;

  // Each component is built where it lies, copied or moved from what it is
  // handed, so a large one is never first made a copy of its own on the way
  // in. A product of no components is a value like any other and its only form
  // is the default one, so the constructor that takes components is declared
  // only where there are components to take. Whether each component can be
  // built from its value is left to where it is built, since a condition over
  // every component is an expression as deep as the product is long.
  template <typename... Us>
    requires(
        sizeof...(Us) == sizeof...(Ts) && sizeof...(Ts) > 0 &&
        !detail::kIsTheProductItself<Tuple, Us...>)
  explicit Tuple(Us&&... values) : data_(std::forward<Us>(values)...) {
  }

  // Component `I`, by declaration-order index, in the product's own value
  // category: one to read through a product that may not be changed, one to
  // write or reach further through, and one to move out of an expiring
  // product. Every component of a product is live at once, so reaching one
  // settles nothing about the others, and reading and writing it are the one
  // access.
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> const Element<I>& {
    return static_cast<const Stored<I>&>(data_).value;
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() & -> Element<I>& {
    return static_cast<Stored<I>&>(data_).value;
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() && -> Element<I>&& {
    return std::move(static_cast<Stored<I>&>(data_).value);
  }

  // LRM 11.4.5: equal where every pair of components is, and unknown where no
  // pair is unequal and some pair is unknown, in the state domain a component's
  // comparison can answer in.
  [[nodiscard]] auto operator==(const Tuple& other) const {
    using Compare = FourStateBit (*)(const Tuple&, const Tuple&);
    static constexpr auto kCompares =
        PerComponent<Compare>([]<std::size_t I>() -> Compare {
          return +[](const Tuple& lhs, const Tuple& rhs) {
            return AnswerScalar(
                lhs.template Component<I>() == rhs.template Component<I>());
          };
        });
    FourStateBit equal = FourStateBit::kOne;
    for (const Compare compare : kCompares) {
      equal = LogicalAnd(equal, compare(*this, other));
    }
    return OneBit<EqualityDomain()>::Filled(equal);
  }
  [[nodiscard]] auto operator!=(const Tuple& other) const {
    return !(*this == other);
  }

  // LRM 11.4.5 `===`, a known bit: every pair of components identical in the
  // same sense.
  [[nodiscard]] auto CaseEqual(const Tuple& other) const -> Bit {
    using Compare = bool (*)(const Tuple&, const Tuple&);
    static constexpr auto kCompares =
        PerComponent<Compare>([]<std::size_t I>() -> Compare {
          return +[](const Tuple& lhs, const Tuple& rhs) {
            return lhs.template Component<I>()
                .CaseEqual(rhs.template Component<I>())
                .IsTruthy();
          };
        });
    return Bit::FromBool(std::ranges::all_of(kCompares, [&](Compare compare) {
      return compare(*this, other);
    }));
  }

  // LRM 9.4.2: whether two products are the same bits, component by component.
  [[nodiscard]] auto IsBitIdentical(const Tuple& other) const -> bool {
    using Compare = bool (*)(const Tuple&, const Tuple&);
    static constexpr auto kCompares =
        PerComponent<Compare>([]<std::size_t I>() -> Compare {
          return +[](const Tuple& lhs, const Tuple& rhs) {
            return lhs.template Component<I>().IsBitIdentical(
                rhs.template Component<I>());
          };
        });
    return std::ranges::all_of(
        kCompares, [&](Compare compare) { return compare(*this, other); });
  }

  // LRM 20.9: whether any component holds an x or a z.
  [[nodiscard]] auto HasUnknown() const -> bool {
    using Ask = bool (*)(const Tuple&);
    static constexpr auto kAsks = PerComponent<Ask>([]<std::size_t I>() -> Ask {
      return +[](const Tuple& product) {
        return product.template Component<I>().HasUnknown();
      };
    });
    return std::ranges::any_of(kAsks, [&](Ask ask) { return ask(*this); });
  }

  // Each component's positions above its widths made clear, through a table
  // of one entry per component rather than one expression over all of them.
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
  template <std::size_t I>
  using Stored = detail::TupleComponent<I, Element<I>>;

  // A table of one entry per component, entry `I` what `entry.template
  // operator()<I>()` makes, so a question asked of every component is a loop
  // over the table rather than one expression over all of them.
  template <typename Entry, typename MakeEntry>
  [[nodiscard]] static constexpr auto PerComponent(MakeEntry entry) {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return std::array<Entry, sizeof...(I)> {
        entry.template operator()<I>()...
      };
    }(std::index_sequence_for<Ts...>{});
  }

  // The state domain comparing two products answers in: four-state where
  // comparing some pair of components can answer unknown.
  [[nodiscard]] static constexpr auto EqualityDomain() -> StateDomain {
    constexpr std::array<StateDomain, sizeof...(Ts)> kComponents{
        decltype(std::declval<const Ts&>() == std::declval<const Ts&>())::
            kDomain...};
    StateDomain domain = StateDomain::kTwoState;
    for (const StateDomain component : kComponents) {
      domain = detail::CombinedDomain(domain, component);
    }
    return domain;
  }

  Components data_;
};

}  // namespace lyra::value
