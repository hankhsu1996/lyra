#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>

namespace lyra::backend::cpp {

// Target text under construction. Everything that goes into an emitted artifact
// is written here, in the order it will be read, so what a byte costs is the
// one write that puts it in place.
//
// Two things a contributor would otherwise have to be told belong to the text
// instead. What column a line opens at is a property of the text rather than of
// whoever adds to it. And a blank line setting one section apart from the next
// is owed rather than written, so a section with nothing in it costs nothing --
// separator included -- and nobody has to answer in advance whether their
// section has anything.
class TargetText {
 public:
  auto operator+=(std::string_view text) -> TargetText&;

  // A line is written by opening it, writing what it holds, and ending it with
  // a newline. Opening it is what puts it at the current depth.
  void OpenLine();

  void Indent();
  void Outdent();

  // A body is written at its own depth, whatever the text stood at where the
  // body was reached, so a lambda inside a deeply nested statement opens where
  // any other body does. The depth in force before it is restored when this
  // ends.
  class BodyDepth {
   public:
    BodyDepth(TargetText& out, std::size_t depth)
        : out_(out), outer_depth_(out.depth_) {
      out.depth_ = depth;
    }
    ~BodyDepth() {
      out_.depth_ = outer_depth_;
    }
    BodyDepth(const BodyDepth&) = delete;
    BodyDepth(BodyDepth&&) = delete;
    auto operator=(const BodyDepth&) -> BodyDepth& = delete;
    auto operator=(BodyDepth&&) -> BodyDepth& = delete;

   private:
    TargetText& out_;
    std::size_t outer_depth_;
  };

  // A section of an artifact, set apart from the one before it by a blank line.
  // The separator is owed while the section is open and paid by the first byte
  // that lands, so a section that turns out to hold nothing contributes nothing
  // at all -- separator included -- and nobody has to answer in advance whether
  // theirs has anything. A section opened inside another owes one of its own,
  // which is how the outer separator and the inner one both stand.
  class Section {
   public:
    explicit Section(TargetText& out)
        : out_(out), length_at_open_(out.text_.size()) {
      ++out.owed_blank_lines_;
    }
    ~Section() {
      if (out_.text_.size() == length_at_open_) {
        --out_.owed_blank_lines_;
      }
    }
    Section(const Section&) = delete;
    Section(Section&&) = delete;
    auto operator=(const Section&) -> Section& = delete;
    auto operator=(Section&&) -> Section& = delete;

   private:
    TargetText& out_;
    std::size_t length_at_open_;
  };

  [[nodiscard]] auto View() const -> std::string_view {
    return text_;
  }
  [[nodiscard]] auto Take() && -> std::string {
    return std::move(text_);
  }

 private:
  void PayWhatIsOwed();

  std::string text_;
  std::size_t depth_ = 0;
  std::size_t owed_blank_lines_ = 0;
};

// A whole number, in the base asked for. Turning a number into text is work
// rather than placement, so it is spelled once and lands straight in the text.
void WriteNumber(TargetText& out, std::uint64_t value, int base);
void WriteNumber(TargetText& out, std::int64_t value, int base);

// What may stand as a piece beside target syntax without naming any part of the
// program. A character is not one: written alone it would be a number, which is
// never what a piece of syntax means.
template <typename T>
concept WholeNumber =
    std::integral<T> && !std::same_as<std::remove_cv_t<T>, bool> &&
    !std::same_as<std::remove_cv_t<T>, char>;

inline void WriteOne(TargetText& out, std::string_view text) {
  out += text;
}

template <WholeNumber Number>
void WriteOne(TargetText& out, Number value) {
  if constexpr (std::signed_integral<Number>) {
    WriteNumber(out, static_cast<std::int64_t>(value), 10);
  } else {
    WriteNumber(out, static_cast<std::uint64_t>(value), 10);
  }
}

// The pieces of one contribution, in the order they are read. A comma fold runs
// them left to right, so what a piece writes lands where the piece is written.
template <typename... Pieces>
void Write(TargetText& out, const Pieces&... pieces) {
  (WriteOne(out, pieces), ...);
}

// Text that is a value rather than a contribution: characters a reader other
// than the artifact needs, such as the name of the file a class is written in.
// Anything the artifact reads belongs in the artifact instead, written where it
// goes.
template <typename... Pieces>
[[nodiscard]] auto TextOf(const Pieces&... pieces) -> std::string {
  TargetText text;
  Write(text, pieces...);
  return std::move(text).Take();
}

}  // namespace lyra::backend::cpp
