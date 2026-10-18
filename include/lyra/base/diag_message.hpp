#pragma once

#include <array>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace lyra::base {

// A rule of the form every message to a user takes, as the literal it is
// formatted from is checked against it.
enum class MessageRule : std::uint8_t {
  kSaysSomething,
  kStartsLowercase,
  kEndsWithoutPunctuation,
  kHoldsNoColonBeforeASpace,
  kIsOneSentence,
  kIsOneLine,
  kQuotesANameInSingleQuotes,
  kHoldsNoContraction,
  kCitesNoClauseOfTheStandard,
  kUsesNoWordForTheCompilersInsides,
  kSaysAGapIsNotYetSupported,
};

// Declared and never defined, each named for a rule, so a literal that breaks
// one fails to build at the call that formats it and the error names the rule.
void AMessageSaysSomething();
void AMessageStartsLowercase();
void AMessageEndsWithoutPunctuation();
void AMessageHoldsNoColonBeforeASpace();
void AMessageIsOneSentence();
void AMessageIsOneLine();
void AMessageQuotesANameInSingleQuotes();
void AMessageHoldsNoContraction();
void AMessageCitesNoClauseOfTheStandard();
void AMessageUsesNoWordForTheCompilersInsides();
void AMessageSaysAGapIsNotYetSupported();

namespace detail {

constexpr auto IsLower(char c) -> bool {
  return c >= 'a' && c <= 'z';
}

constexpr auto IsUpper(char c) -> bool {
  return c >= 'A' && c <= 'Z';
}

constexpr auto IsAlnum(char c) -> bool {
  return IsLower(c) || IsUpper(c) || (c >= '0' && c <= '9');
}

constexpr auto ToLower(char c) -> char {
  return IsUpper(c) ? static_cast<char>(c - 'A' + 'a') : c;
}

// An acronym keeps its capitals at the head of a message (DPI-C, PATH).
constexpr auto StartsWithAcronym(std::string_view text) -> bool {
  std::size_t capitals = 0;
  for (const char c : text) {
    if (c == ' ') {
      break;
    }
    if (IsLower(c)) {
      return false;
    }
    if (IsUpper(c)) {
      ++capitals;
    }
  }
  return capitals >= 2;
}

// Whether `word` stands at `at` as a whole word, ignoring case.
constexpr auto WordAt(
    std::string_view text, std::size_t at, std::string_view word) -> bool {
  if (at + word.size() > text.size()) {
    return false;
  }
  for (std::size_t i = 0; i < word.size(); ++i) {
    if (ToLower(text[at + i]) != word[i]) {
      return false;
    }
  }
  const bool open = at == 0 || !IsAlnum(text[at - 1]);
  const bool close =
      at + word.size() == text.size() || !IsAlnum(text[at + word.size()]);
  return open && close;
}

// Words for how Lyra works inside, which a designer reading a message has no
// reason to know.
inline constexpr std::array<std::string_view, 16> kInsideWords = {
    "lowered",      "lowering",       "hir",       "mir",      "lir",
    "codegen",      "code generator", "front end", "frontend", "closure",
    "coroutine",    "arena",          "thunk",     "receiver", "materialize",
    "materialized",
};

// Every spelling of a gap but "not yet supported".
inline constexpr std::array<std::string_view, 4> kGapSpellings = {
    "unsupported",
    "not supported",
    "unimplemented",
    "not implemented",
};

inline constexpr std::array<std::string_view, 6> kContractions = {
    "n't", "'re", "'ll", "'ve", "'m", "'d",
};

// A single quote opens a name where no letter or digit stands before it, and
// closes one where none stands after it; between letters it is a possessive.
constexpr auto OpensQuote(std::string_view text, std::size_t at) -> bool {
  return text[at] == '\'' && (at == 0 || !IsAlnum(text[at - 1]));
}

}  // namespace detail

// The first rule `text` breaks, where it breaks one.
constexpr auto BrokenMessageRule(std::string_view text)
    -> std::optional<MessageRule> {
  using detail::IsAlnum;
  using detail::WordAt;
  if (text.empty()) {
    return MessageRule::kSaysSomething;
  }
  if (detail::IsUpper(text.front()) && !detail::StartsWithAcronym(text)) {
    return MessageRule::kStartsLowercase;
  }
  const char last = text.back();
  if (last == '!' || last == ',' || last == ';' || last == ':' || last == ' ' ||
      (last == '.' && !text.ends_with("..."))) {
    return MessageRule::kEndsWithoutPunctuation;
  }
  bool quoted = false;
  for (std::size_t i = 0; i < text.size(); ++i) {
    const char c = text[i];
    if (c == '\'') {
      quoted = quoted ? i + 1 < text.size() && IsAlnum(text[i + 1])
                      : detail::OpensQuote(text, i);
    }
    if (c == '\n' || c == '\t') {
      return MessageRule::kIsOneLine;
    }
    if (c == '`' || c == '"') {
      return MessageRule::kQuotesANameInSingleQuotes;
    }
    if (quoted) {
      continue;
    }
    const auto rest = text.substr(i);
    if (rest.starts_with(": ")) {
      return MessageRule::kHoldsNoColonBeforeASpace;
    }
    if ((rest.starts_with(". ") || rest.starts_with("? ")) &&
        (i < 2 || text.substr(i - 2, 3) != "...")) {
      return MessageRule::kIsOneSentence;
    }
    for (const std::string_view contraction : detail::kContractions) {
      if (rest.starts_with(contraction)) {
        return MessageRule::kHoldsNoContraction;
      }
    }
    if (WordAt(text, i, "lrm") || WordAt(text, i, "ieee") ||
        WordAt(text, i, "annex")) {
      return MessageRule::kCitesNoClauseOfTheStandard;
    }
    for (const std::string_view word : detail::kInsideWords) {
      if (WordAt(text, i, word)) {
        return MessageRule::kUsesNoWordForTheCompilersInsides;
      }
    }
    for (const std::string_view gap : detail::kGapSpellings) {
      if (WordAt(text, i, gap)) {
        return MessageRule::kSaysAGapIsNotYetSupported;
      }
    }
  }
  return std::nullopt;
}

namespace detail {

consteval auto RequireMessageForm(std::string_view text) -> void {
  const std::optional<MessageRule> broken = BrokenMessageRule(text);
  if (!broken) {
    return;
  }
  switch (*broken) {
    case MessageRule::kSaysSomething:
      AMessageSaysSomething();
      return;
    case MessageRule::kStartsLowercase:
      AMessageStartsLowercase();
      return;
    case MessageRule::kEndsWithoutPunctuation:
      AMessageEndsWithoutPunctuation();
      return;
    case MessageRule::kHoldsNoColonBeforeASpace:
      AMessageHoldsNoColonBeforeASpace();
      return;
    case MessageRule::kIsOneSentence:
      AMessageIsOneSentence();
      return;
    case MessageRule::kIsOneLine:
      AMessageIsOneLine();
      return;
    case MessageRule::kQuotesANameInSingleQuotes:
      AMessageQuotesANameInSingleQuotes();
      return;
    case MessageRule::kHoldsNoContraction:
      AMessageHoldsNoContraction();
      return;
    case MessageRule::kCitesNoClauseOfTheStandard:
      AMessageCitesNoClauseOfTheStandard();
      return;
    case MessageRule::kUsesNoWordForTheCompilersInsides:
      AMessageUsesNoWordForTheCompilersInsides();
      return;
    case MessageRule::kSaysAGapIsNotYetSupported:
      AMessageSaysAGapIsNotYetSupported();
      return;
  }
}

}  // namespace detail

// The literal a message to a user is formatted from, checked against the form
// every such message takes when Lyra is built. Only the literal is checked: a
// name or a system's own words formatted into it are not, which is why the
// literal is where the quotes and the parentheses around them are written.
template <typename... Args>
class DiagFormat {
 public:
  template <typename Text>
    requires std::convertible_to<const Text&, std::string_view>
  explicit(false) consteval DiagFormat(const Text& text) : format_(text) {
    detail::RequireMessageForm(std::string_view{text});
  }

  [[nodiscard]] constexpr auto Get() const -> std::format_string<Args...> {
    return format_;
  }

 private:
  std::format_string<Args...> format_;
};

template <typename... Args>
using DiagFormatFor = DiagFormat<std::type_identity_t<Args>...>;

// A message to a user, formatted from a checked literal. Built only that way,
// so a message that reaches a user has passed the check whatever path carried
// it there.
class DiagMessage {
 public:
  template <typename... Args>
  explicit DiagMessage(DiagFormatFor<Args...> format, Args&&... args)
      : text_(std::format(format.Get(), std::forward<Args>(args)...)) {
  }

  [[nodiscard]] auto Text() const -> std::string_view {
    return text_;
  }

  [[nodiscard]] auto TakeText() && -> std::string {
    return std::move(text_);
  }

 private:
  std::string text_;
};

}  // namespace lyra::base
