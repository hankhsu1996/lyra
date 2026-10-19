#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

namespace lyra::value::detail {

// A type at a position of a pack. The types of a pack laid out by position are
// each a base of one class, told apart by its position, so a pack of any length
// is a list of bases and never a recursion over its types, which a compiler
// bounds and a design does not (LRM 7.2). A class laying out values by position
// derives each from the position it names, so the one lookup below serves it as
// well.
template <std::size_t I, typename T>
struct Positioned {};

template <typename Indices, typename... Ts>
struct PackPositionsAt;

template <std::size_t... Is, typename... Ts>
struct PackPositionsAt<std::index_sequence<Is...>, Ts...>
    : Positioned<Is, Ts>... {};

template <typename... Ts>
using PackPositions = PackPositionsAt<std::index_sequence_for<Ts...>, Ts...>;

template <std::size_t I, typename T>
auto TypeAt(const Positioned<I, T>& position) -> std::type_identity<T>;

// The type at position `I` of `Positions`, a class deriving from its pack's
// positions, found by converting to the base `I` names. A class over a pack
// names its positions once and asks them, since naming them afresh at each
// lookup spells the whole pack out again each time, which costs a compiler as
// much as the pack is long at every one.
template <std::size_t I, typename Positions>
using PackElement =
    typename decltype(TypeAt<I>(std::declval<const Positions&>()))::type;

}  // namespace lyra::value::detail
