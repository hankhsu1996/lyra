#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

namespace lyra::runtime {

// The path from a variable to one part of it: one step for each member or
// element selected on the way (LRM 7.2, 7.4), each step being the member's
// index or the element's ordinal. The empty path is the whole variable. A wait
// holds the path of the part it watches and a write the path of the part it
// lands in, and the two are about the same storage exactly where one path is a
// prefix of the other.
//
// A path holds a bounded number of steps, each of bounded size. Where the way
// to a part cannot be followed any further -- past either bound, or by a
// select no step stands for, as into an entry of an associative array (LRM
// 7.8) -- the path stops there and is inexact: the part meant is the one the
// path reaches or something inside it. So an inexact path only ever stands for
// more storage than the source wrote, never for less.
//
// Every member function but the default constructor is defined in the
// library: a unit writing a part of a variable builds and extends these, and a
// definition written here would be compiled again by each such unit. Copying
// one is copying its bytes.
class PartPath {
 public:
  static constexpr std::size_t kMaxSteps = 6;

  // The whole variable. Written here, and not in the library's own source,
  // because every write into part of a value and every reference begins with
  // one, whether or not anything waits on the value.
  constexpr PartPath() noexcept = default;

  // The path of `steps`, in order.
  explicit PartPath(std::span<const std::int64_t> steps);

  // The path of one step, to the child `step` stands for.
  [[nodiscard]] static auto Child(std::uint64_t step) -> PartPath;

  // The path to the element at `ordinal`, inexact where the position names no
  // element.
  [[nodiscard]] static auto Element(std::optional<std::int64_t> ordinal)
      -> PartPath;

  // The path of a select no step stands for.
  [[nodiscard]] static auto Inexact() -> PartPath;

  // This path followed by `further`.
  [[nodiscard]] auto Join(const PartPath& further) const -> PartPath;

  [[nodiscard]] auto Steps() const -> std::span<const std::uint32_t>;

  [[nodiscard]] auto IsInexact() const -> bool;

  // Whether this is the path of the whole variable and nothing less.
  [[nodiscard]] auto IsWhole() const -> bool;

  // Whether the part this path reaches is the one `other` reaches or one that
  // part lies in.
  [[nodiscard]] auto IsPrefixOf(const PartPath& other) const -> bool;

  // This path with `prefix` taken off its front: the same part, reached from
  // the part `prefix` reaches.
  [[nodiscard]] auto StripPrefix(const PartPath& prefix) const -> PartPath;

  [[nodiscard]] auto operator==(const PartPath& other) const -> bool;

 private:
  void Append(std::uint64_t step);

  std::array<std::uint32_t, kMaxSteps> steps_{};
  std::uint8_t depth_ = 0;
  bool inexact_ = false;
};

}  // namespace lyra::runtime
