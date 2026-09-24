#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>

namespace lyra::backend::cpp {

// An output file, written front to back. It also keeps two things writers
// would otherwise have to pass around: the current indentation, which
// `OpenLine` applies, and the blank lines between sections, which are written
// only once the next section actually writes something.
class TargetText {
 public:
  auto operator+=(std::string_view text) -> TargetText&;

  // Starts a line at the current indentation; the writer ends it with a
  // newline.
  void OpenLine();

  void Indent();
  void Outdent();

  // Sets the indentation to `depth` until this is destroyed, then restores
  // it. A function body is written at depth 1 wherever it appears, a
  // lambda inside a deeply nested statement included.
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

  // A section of the file, set apart from what comes before by a blank line.
  // The blank line is written just before the section's first byte, so an
  // empty section writes nothing at all. A section inside another adds a blank
  // line of its own.
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

// Writes an integer in the given base.
void WriteNumber(TargetText& out, std::uint64_t value, int base);
void WriteNumber(TargetText& out, std::int64_t value, int base);

// An integer `Write` writes as decimal digits. `char` and `bool` are left out:
// a piece of syntax never means either one as a number.
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

// Writes the pieces left to right: text as is, integers as digits, and names
// and types the way their own types say to.
template <typename... Pieces>
void Write(TargetText& out, const Pieces&... pieces) {
  (WriteOne(out, pieces), ...);
}

// A list: each item written by `write_item`, with the separator between each
// pair of them. An empty list writes nothing.
template <typename Items, typename WriteItem>
void WriteSeparated(
    TargetText& out, const Items& items, std::string_view separator,
    WriteItem write_item) {
  bool first = true;
  for (const auto& item : items) {
    if (!first) out += separator;
    write_item(item);
    first = false;
  }
}

// A function body in braces: `write_contents` writes its lines at the depth a
// body opens at, and nothing follows the closing brace.
template <typename WriteContents>
void WriteBody(TargetText& out, WriteContents write_contents) {
  out += "{\n";
  {
    const TargetText::BodyDepth body(out, 1);
    write_contents();
  }
  out += "}";
}

// The pieces as a string, for text that is not output, such as a file name.
template <typename... Pieces>
[[nodiscard]] auto TextOf(const Pieces&... pieces) -> std::string {
  TargetText text;
  Write(text, pieces...);
  return std::move(text).Take();
}

}  // namespace lyra::backend::cpp
