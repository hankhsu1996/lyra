#include "lyra/value/enumeration.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

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
      planes.push_back(WordAt(member.value, i));
    }
    for (std::size_t i = 0; four_state && i < words; ++i) {
      planes.push_back(WordAt(member.unknown, i));
    }
  }
  return planes;
}

auto Enumeration::MemberAt(std::size_t position) const -> ConstPlanes {
  const std::size_t words = WordCountForBits(width);
  const std::size_t stride = EnumerationMemberWords(width, four_state);
  const std::span<const std::uint64_t> member =
      std::span(planes, members * stride).subspan(position * stride, stride);
  return ConstPlanes{
      .value = member.first(words), .unknown = member.subspan(words)};
}

auto Enumeration::PositionOf(ConstPlanes value) const
    -> std::optional<std::size_t> {
  for (std::size_t i = 0; i < members; ++i) {
    if (CaseEqual(MemberAt(i), value)) {
      return i;
    }
  }
  return std::nullopt;
}

auto Enumeration::NameOf(ConstPlanes value) const -> String {
  const std::optional<std::size_t> at = PositionOf(value);
  return at ? String(std::span(names, members)[*at]) : String();
}

auto Enumeration::Stepped(ConstPlanes value, std::size_t steps) const
    -> std::optional<ConstPlanes> {
  const std::optional<std::size_t> at = PositionOf(value);
  if (!at) {
    return std::nullopt;
  }
  return MemberAt((*at + steps) % members);
}

auto Enumeration::MemberAfter(ConstPlanes value, std::int64_t count) const
    -> std::optional<ConstPlanes> {
  return Stepped(
      value,
      static_cast<std::size_t>(static_cast<std::uint64_t>(count) % members));
}

// Stepping back `count` places lands where stepping on by the rest of a whole
// turn through the members does.
auto Enumeration::MemberBefore(ConstPlanes value, std::int64_t count) const
    -> std::optional<ConstPlanes> {
  return Stepped(
      value, members - static_cast<std::size_t>(
                           static_cast<std::uint64_t>(count) % members));
}

}  // namespace lyra::value
