#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// How many words one member of an enumeration takes in its table: the words of
// its value plane and then, over a base that has one, those of its unknown
// plane.
[[nodiscard]] constexpr auto EnumerationMemberWords(
    std::uint64_t width, bool four_state) -> std::size_t {
  return WordCountForBits(width) * (four_state ? 2U : 1U);
}

// The words a table of `members` is laid out as: member after member in
// declared order, each as wide as the base type states. A member that states
// no unknown plane over a base that has one holds no x or z.
[[nodiscard]] auto EnumerationPlanes(
    std::uint64_t width, bool four_state, std::span<const ConstPlanes> members)
    -> std::vector<std::uint64_t>;

// The members an enumeration declares, in declared order, and the questions
// the language asks of a value against them (LRM 6.19.5, 6.24.2). Each member
// is its whole value at the base type -- a 4-state base admits members holding
// x or z bits, and a base may be wider than a machine word -- so a value is a
// member when it is bit-identical to one, unknown bits included.
//
// The members are constant data of the enumeration, which whoever compiled it
// states once and this only reads: the base type's width and whether it has an
// unknown plane, the members' planes laid out as that says, and the members'
// names in the same order. It is laid out as C lays a structure out, because a
// unit states one as data. A value asked about is of the base type.
struct Enumeration {
  const std::uint64_t* planes;
  const char* const* names;
  std::uint64_t members;
  std::uint64_t width;
  bool four_state;

  // LRM 6.24.2: where a value lies among the declared members, and nothing for
  // one that is no member.
  [[nodiscard]] auto PositionOf(ConstPlanes value) const
      -> std::optional<std::size_t>;
  // LRM 6.19.5.6: the member's name, or the empty string for a value that is
  // no member.
  [[nodiscard]] auto NameOf(ConstPlanes value) const -> String;
  // LRM 6.19.5.3 / 6.19.5.4: the member `count` places after or before the
  // value in declared order, wrapping at either end, and nothing for a value
  // that is no member. `count` is the `int unsigned` the source passed, read
  // as a machine integer.
  [[nodiscard]] auto MemberAfter(ConstPlanes value, std::int64_t count) const
      -> std::optional<ConstPlanes>;
  [[nodiscard]] auto MemberBefore(ConstPlanes value, std::int64_t count) const
      -> std::optional<ConstPlanes>;

 private:
  [[nodiscard]] auto MemberAt(std::size_t position) const -> ConstPlanes;

  // The member `steps` places after the one `value` is, wrapping.
  [[nodiscard]] auto Stepped(ConstPlanes value, std::size_t steps) const
      -> std::optional<ConstPlanes>;
};

// An enumeration's members as a unit compiled with the base type states them:
// the planes and the names held in the constant itself, and the same questions
// asked of a value of the base type.
template <std::uint64_t kWidth, bool kFourState, std::size_t kMembers>
struct EnumerationMembers {
  std::array<
      std::uint64_t, kMembers * EnumerationMemberWords(kWidth, kFourState)>
      planes;
  std::array<const char*, kMembers> names;

  template <IntegralValue T>
  [[nodiscard]] auto Has(const T& value) const -> bool {
    return Read().PositionOf(value.Load().Read()).has_value();
  }

  template <IntegralValue T>
  [[nodiscard]] auto Name(const T& value) const -> String {
    return Read().NameOf(value.Load().Read());
  }

  // A value that is no member steps to the base type's default (LRM Table
  // 6-7).
  template <IntegralValue T>
  [[nodiscard]] auto Next(const T& value, std::int64_t count) const -> T {
    return MemberOr<T>(Read().MemberAfter(value.Load().Read(), count));
  }
  template <IntegralValue T>
  [[nodiscard]] auto Prev(const T& value, std::int64_t count) const -> T {
    return MemberOr<T>(Read().MemberBefore(value.Load().Read(), count));
  }

 private:
  [[nodiscard]] constexpr auto Read() const -> Enumeration {
    return Enumeration{
        .planes = planes.data(),
        .names = names.data(),
        .members = kMembers,
        .width = kWidth,
        .four_state = kFourState};
  }

  template <IntegralValue T>
  [[nodiscard]] static auto MemberOr(std::optional<ConstPlanes> member) -> T {
    if (!member) {
      return T{};
    }
    typename T::Words words;
    std::ranges::copy(member->value, words.value.begin());
    std::ranges::copy(member->unknown, words.unknown.begin());
    return T::FromWords(words);
  }
};

}  // namespace lyra::value
