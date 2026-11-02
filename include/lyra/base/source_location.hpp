#pragma once

#include <compare>
#include <cstdint>

namespace lyra::base {

// Where in the design's source an operation was written, so a failure the
// library raises for the design is reported there (LRM 20.10 asks this of a
// severity task's own report, and a run-time error of the design is reported
// the same way). It is one word, the address of a record the program states as
// a constant once per place, as `std::source_location` is, and it is copied and
// handed on as a value. The default value is no place.
class SourceLocation {
 public:
  // What a program states of a place: the file and the line `__FILE__ and
  // `__LINE__ expand to there (LRM 22.13) and the column.
  struct Impl {
    const char* file = nullptr;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
  };

  constexpr SourceLocation() = default;
  explicit constexpr SourceLocation(const Impl* impl) : impl_(impl) {
  }

  [[nodiscard]] constexpr auto IsValid() const -> bool {
    return impl_ != nullptr;
  }
  [[nodiscard]] constexpr auto File() const -> const char* {
    return impl_->file;
  }
  [[nodiscard]] constexpr auto Line() const -> std::uint32_t {
    return impl_->line;
  }
  [[nodiscard]] constexpr auto Column() const -> std::uint32_t {
    return impl_->column;
  }

  // Which constant it names, which a unit states once per place, so what is
  // counted by place is keyed without reading the record.
  [[nodiscard]] constexpr auto Identity() const -> const Impl* {
    return impl_;
  }

  constexpr auto operator<=>(const SourceLocation&) const = default;

 private:
  const Impl* impl_ = nullptr;
};

// Stated by a raise or a report the run makes for no operation of the design,
// so it reads no place.
struct NoSourceLocation {};
inline constexpr NoSourceLocation kNoSourceLocation{};

// Binds `at` for the extent of the scope, as the place whatever fails inside it
// is reported at, and restores the place bound before it. Handed no place it
// changes nothing, so the place already bound stands: a function a table calls
// is handed none, and what it fails at is the operation that reached the table.
// The library's door opens one, so nothing between the door and a report
// carries the place. Like folly's `RequestContextScopeGuard` it sets on
// construction and restores on destruction. A door's extent never spans a
// suspension, so nothing has to clear it.
class SourceLocationScope {
 public:
  explicit SourceLocationScope(SourceLocation at) noexcept : previous_(bound_) {
    bound_ = at.IsValid() ? at : previous_;
  }
  SourceLocationScope(const SourceLocationScope&) = delete;
  auto operator=(const SourceLocationScope&) -> SourceLocationScope& = delete;
  SourceLocationScope(SourceLocationScope&&) = delete;
  auto operator=(SourceLocationScope&&) -> SourceLocationScope& = delete;
  ~SourceLocationScope() {
    bound_ = previous_;
  }

  friend auto CurrentSourceLocation() noexcept -> SourceLocation;

 private:
  static constinit inline thread_local SourceLocation bound_{};

  SourceLocation previous_;
};

// The place bound now, which is no place outside every door.
[[nodiscard]] inline auto CurrentSourceLocation() noexcept -> SourceLocation {
  return SourceLocationScope::bound_;
}

}  // namespace lyra::base
