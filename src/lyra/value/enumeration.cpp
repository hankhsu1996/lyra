#include "lyra/value/enumeration.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

auto EnumerationPlanes(
    std::uint64_t width, bool four_state, std::span<const ConstPlanes> members)
    -> std::vector<std::uint64_t> {
  const std::size_t words = WordCountForBits(width);
  std::vector<std::uint64_t> planes;
  planes.reserve(members.size() * EnumerationMemberWords(width, four_state));
  for (const ConstPlanes& member : members) {
    for (std::size_t i = 0; i < words; ++i) {
      planes.push_back(WordOrZero(member.value, i));
    }
    for (std::size_t i = 0; four_state && i < words; ++i) {
      planes.push_back(WordOrZero(member.unknown, i));
    }
  }
  return planes;
}

auto Enumeration::MemberAt(std::size_t position) const -> ConstIntegralView {
  const std::size_t stride = EnumerationMemberWords(width, four_state);
  return ViewOfWords(
      std::span(planes, members * stride).subspan(position * stride).data(),
      IntegralShape{
          .width = width,
          .signedness = Signedness::kUnsigned,
          .domain =
              four_state ? StateDomain::kFourState : StateDomain::kTwoState});
}

auto Enumeration::PositionOf(const ConstIntegralView& value) const
    -> std::optional<std::size_t> {
  for (std::size_t i = 0; i < members; ++i) {
    if (CaseEqual(MemberAt(i), value)) {
      return i;
    }
  }
  return std::nullopt;
}

auto Enumeration::NameOf(const ConstIntegralView& value) const -> String {
  const std::optional<std::size_t> at = PositionOf(value);
  return at ? String(std::span(names, members)[*at]) : String();
}

auto Enumeration::Stepped(const ConstIntegralView& value, std::size_t steps)
    const -> std::optional<ConstIntegralView> {
  const std::optional<std::size_t> at = PositionOf(value);
  if (!at) {
    return std::nullopt;
  }
  return MemberAt((*at + steps) % members);
}

auto Enumeration::MemberAfter(
    const ConstIntegralView& value, std::int64_t count) const
    -> std::optional<ConstIntegralView> {
  return Stepped(
      value,
      static_cast<std::size_t>(static_cast<std::uint64_t>(count) % members));
}

// Stepping back `count` places lands where stepping on by the rest of a whole
// turn through the members does.
auto Enumeration::MemberBefore(
    const ConstIntegralView& value, std::int64_t count) const
    -> std::optional<ConstIntegralView> {
  return Stepped(
      value, members - static_cast<std::size_t>(
                           static_cast<std::uint64_t>(count) % members));
}

}  // namespace lyra::value
