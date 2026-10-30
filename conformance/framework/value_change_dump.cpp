#include "conformance/framework/value_change_dump.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

namespace conformance::framework {
namespace {

auto Tokens(std::string_view text) -> std::vector<std::string_view> {
  std::vector<std::string_view> tokens;
  std::size_t at = 0;
  while (at < text.size()) {
    while (at < text.size() &&
           std::isspace(static_cast<unsigned char>(text[at])) != 0) {
      ++at;
    }
    const std::size_t start = at;
    while (at < text.size() &&
           std::isspace(static_cast<unsigned char>(text[at])) == 0) {
      ++at;
    }
    if (at > start) {
      tokens.push_back(text.substr(start, at - start));
    }
  }
  return tokens;
}

auto ReadUnsigned(std::string_view digits) -> std::optional<std::uint64_t> {
  std::uint64_t value = 0;
  const auto [end, failed] =
      std::from_chars(digits.data(), digits.data() + digits.size(), value);
  if (digits.empty() || failed != std::errc{} ||
      end != digits.data() + digits.size()) {
    return std::nullopt;
  }
  return value;
}

// How many femtoseconds one unit of a dump's time stands for, read off its
// $timescale: 1, 10 or 100 of s, ms, us, ns, ps or fs, the number and the
// unit together or apart (LRM 21.7.2.1 `time_number`, `time_unit`).
auto FemtosecondsPerUnit(std::string_view spelled)
    -> std::optional<std::uint64_t> {
  constexpr std::array<std::pair<std::string_view, std::uint64_t>, 6> kUnits{{
      {"fs", 1},
      {"ps", 1'000},
      {"ns", 1'000'000},
      {"us", 1'000'000'000},
      {"ms", 1'000'000'000'000},
      {"s", 1'000'000'000'000'000},
  }};
  for (const auto& [unit, femtoseconds] : kUnits) {
    if (!spelled.ends_with(unit)) {
      continue;
    }
    const std::optional<std::uint64_t> number =
        ReadUnsigned(spelled.substr(0, spelled.size() - unit.size()));
    if (!number || (*number != 1 && *number != 10 && *number != 100)) {
      return std::nullopt;
    }
    return *number * femtoseconds;
  }
  return std::nullopt;
}

// A declaration's kind, and its size, which counts the bits in the variable
// (LRM 21.7.2.3). A real or an event has no bits to count and the type mapping
// gives a real no size (LRM Table 21-11), so for those the size is the tool's.
struct Declared {
  std::string kind;
  std::string size;

  auto operator==(const Declared& other) const -> bool {
    const bool sized = kind != "real" && kind != "realtime" && kind != "event";
    return kind == other.kind && (!sized || size == other.size);
  }
};

// A checkpoint section: its keyword and the value it records of each name.
struct Section {
  std::string keyword;
  std::map<std::string, std::string> values;

  auto operator<=>(const Section&) const = default;
};

// What one simulated time records: the sections it opens, which nothing
// orders, and the changes outside them.
struct Moment {
  std::vector<Section> sections;
  std::map<std::string, std::string> changes;
};

// What a dump records, keyed by the hierarchical names it declares rather
// than by the codes it happened to give them, and by simulated time in
// femtoseconds rather than in the unit it happened to count in.
struct Dump {
  std::set<std::string> scopes;
  std::map<std::string, Declared> declared;
  std::map<std::uint64_t, Moment> moments;
};

// A vector's value as it stands at its declared width: a shortened one is
// extended to the left with 0 where its leftmost digit is 0 or 1, and with
// that digit where it is x or z (LRM 21.7.2.2, Table 21-8).
auto Widened(std::string_view digits, std::string_view size) -> std::string {
  std::string lowered;
  for (const char digit : digits) {
    lowered.push_back(
        static_cast<char>(std::tolower(static_cast<unsigned char>(digit))));
  }
  const std::optional<std::uint64_t> width = ReadUnsigned(size);
  if (!width || lowered.empty() || lowered.size() >= *width) {
    return lowered;
  }
  const char fill =
      lowered.front() == 'x' || lowered.front() == 'z' ? lowered.front() : '0';
  return std::string(*width - lowered.size(), fill) + lowered;
}

// Whether `size` is one a declaration may give: a number of bits, or for a
// port its index range or 1 (LRM 21.7.2.1, 21.7.4.2).
auto IsSize(std::string_view kind, std::string_view size) -> bool {
  if (ReadUnsigned(size)) {
    return true;
  }
  if (kind != "port" || !size.starts_with('[') || !size.ends_with(']')) {
    return false;
  }
  const std::string_view range = size.substr(1, size.size() - 2);
  const std::size_t colon = range.find(':');
  return colon != std::string_view::npos &&
         ReadUnsigned(range.substr(0, colon)) &&
         ReadUnsigned(range.substr(colon + 1));
}

auto Joined(const std::vector<std::string>& path, std::string_view name)
    -> std::string {
  std::string full;
  for (const std::string& part : path) {
    full += part;
    full += '.';
  }
  full += name;
  return full;
}

auto Parse(std::string_view text) -> std::expected<Dump, std::string> {
  const std::vector<std::string_view> tokens = Tokens(text);
  Dump dump;
  std::vector<std::string> path;
  std::map<std::string, std::vector<std::string>, std::less<>> named;
  std::optional<std::uint64_t> unit;
  bool defined = false;
  Section* section = nullptr;
  std::uint64_t now = 0;

  // Moves `at` to the $end closing the command at `at`, which every command
  // has whatever its content.
  const auto to_end = [&](std::size_t& at) -> bool {
    while (at < tokens.size() && tokens[at] != "$end") {
      ++at;
    }
    return at < tokens.size();
  };
  const auto record = [&](std::string_view code,
                          std::string value) -> std::optional<std::string> {
    const auto found = named.find(code);
    if (found == named.end()) {
      return std::format(
          "a value is written under '{}', which nothing declares", code);
    }
    for (const std::string& name : found->second) {
      // An event holds no value for a checkpoint section to list (LRM
      // 21.7.2.3); its triggering is what a change of it records.
      const bool event = dump.declared[name].kind == "event";
      if (section != nullptr) {
        if (!event) {
          section->values[name] = value;
        }
      } else {
        dump.moments[now].changes[name] = event ? "triggered" : value;
      }
    }
    return std::nullopt;
  };

  for (std::size_t at = 0; at < tokens.size(); ++at) {
    const std::string_view token = tokens[at];
    if (!defined) {
      if (token == "$scope") {
        if (at + 3 >= tokens.size() || tokens[at + 3] != "$end") {
          return std::unexpected("a $scope is not its kind, a name and $end");
        }
        dump.scopes.insert(
            std::format("{} {}", tokens[at + 1], Joined(path, tokens[at + 2])));
        path.emplace_back(tokens[at + 2]);
        at += 3;
      } else if (token == "$upscope") {
        if (path.empty() || at + 1 >= tokens.size() ||
            tokens[at + 1] != "$end") {
          return std::unexpected("an $upscope closes no scope");
        }
        path.pop_back();
        ++at;
      } else if (token == "$var") {
        if (at + 5 >= tokens.size()) {
          return std::unexpected("a $var lacks its kind, size, code or name");
        }
        const std::string_view kind = tokens[at + 1];
        const std::string_view size = tokens[at + 2];
        if (!IsSize(kind, size)) {
          return std::unexpected(
              std::format("'{}' is no size of a $var", size));
        }
        const std::string name = Joined(path, tokens[at + 4]);
        dump.declared[name] =
            Declared{.kind = std::string(kind), .size = std::string(size)};
        named[std::string(tokens[at + 3])].push_back(name);
        at += 4;
        if (!to_end(at)) {
          return std::unexpected("a $var is never ended");
        }
      } else if (token == "$timescale") {
        std::string spelled;
        for (++at; at < tokens.size() && tokens[at] != "$end"; ++at) {
          spelled += tokens[at];
        }
        unit = FemtosecondsPerUnit(spelled);
        if (!unit || at >= tokens.size()) {
          return std::unexpected(std::format("'{}' is no time unit", spelled));
        }
      } else if (token == "$enddefinitions") {
        if (at + 1 >= tokens.size() || tokens[at + 1] != "$end") {
          return std::unexpected("$enddefinitions is not ended");
        }
        defined = true;
        ++at;
      } else if (token.starts_with('$') && token != "$end") {
        if (!to_end(at)) {
          return std::unexpected(std::format("'{}' is never ended", token));
        }
      } else {
        return std::unexpected(
            std::format("'{}' stands among the definitions", token));
      }
      continue;
    }
    if (token == "$end") {
      if (section == nullptr) {
        return std::unexpected("an $end closes no section");
      }
      section = nullptr;
    } else if (token == "$comment") {
      if (!to_end(at)) {
        return std::unexpected("a $comment is never ended");
      }
    } else if (token == "$vcdclose") {
      // The time the dump was closed at, and nothing else (LRM 21.7.4.1).
      if (at + 2 >= tokens.size() || !tokens[at + 1].starts_with('#') ||
          !ReadUnsigned(tokens[at + 1].substr(1)) || tokens[at + 2] != "$end") {
        return std::unexpected("a $vcdclose is not a time and $end");
      }
      at += 2;
    } else if (token.starts_with('$')) {
      if (section != nullptr) {
        return std::unexpected(
            std::format("'{}' opens inside another section", token));
      }
      dump.moments[now].sections.push_back(
          Section{.keyword = std::string(token), .values = {}});
      section = &dump.moments[now].sections.back();
    } else if (token.starts_with('#')) {
      const std::optional<std::uint64_t> time = ReadUnsigned(token.substr(1));
      if (!time) {
        return std::unexpected(std::format("'{}' is no time", token));
      }
      if (!unit) {
        return std::unexpected(
            "the dump states no $timescale, so its times say no time");
      }
      now = *time * *unit;
    } else if (token[0] == 'b' || token[0] == 'B') {
      if (at + 1 >= tokens.size()) {
        return std::unexpected("a vector value names no code");
      }
      const std::string_view code = tokens[++at];
      const auto found = named.find(code);
      const std::string size = found == named.end() || found->second.empty()
                                   ? std::string{}
                                   : dump.declared[found->second.front()].size;
      if (auto failed = record(code, "b" + Widened(token.substr(1), size))) {
        return std::unexpected(*failed);
      }
    } else if (token[0] == 'r' || token[0] == 'R') {
      if (at + 1 >= tokens.size()) {
        return std::unexpected("a real value names no code");
      }
      const std::string_view digits = token.substr(1);
      double number = 0;
      const auto [end, failed] =
          std::from_chars(digits.data(), digits.data() + digits.size(), number);
      if (failed != std::errc{} || end != digits.data() + digits.size()) {
        return std::unexpected(std::format("'{}' is no real value", token));
      }
      if (auto missing = record(tokens[++at], std::format("r{}", number))) {
        return std::unexpected(*missing);
      }
    } else if (token[0] == 'p') {
      // A port: its state characters, then its two strengths, then its code
      // (LRM 21.7.4.3).
      if (at + 3 >= tokens.size()) {
        return std::unexpected("a port value lacks its strengths or code");
      }
      std::string value =
          std::format("{} {} {}", token, tokens[at + 1], tokens[at + 2]);
      if (auto missing = record(tokens[at + 3], std::move(value))) {
        return std::unexpected(*missing);
      }
      at += 3;
    } else if (
        std::string_view{"01xXzZ"}.find(token[0]) != std::string_view::npos &&
        token.size() > 1) {
      std::string value(
          1, static_cast<char>(
                 std::tolower(static_cast<unsigned char>(token[0]))));
      if (auto missing = record(token.substr(1), std::move(value))) {
        return std::unexpected(*missing);
      }
    } else {
      return std::unexpected(std::format("'{}' is no value change", token));
    }
  }
  if (!defined) {
    return std::unexpected("the definitions are never ended");
  }
  if (section != nullptr) {
    return std::unexpected("a section is never ended");
  }
  // A scope declaring nothing, directly or below, records nothing.
  std::erase_if(dump.scopes, [&](const std::string& scope) {
    const std::string prefix = scope.substr(scope.find(' ') + 1) + ".";
    return std::ranges::none_of(dump.declared, [&](const auto& declared) {
      return declared.first.starts_with(prefix);
    });
  });
  // The sections of one time are compared as a set, since nothing fixes the
  // order a tool writes them in.
  for (auto& [time, moment] : dump.moments) {
    std::ranges::sort(moment.sections);
  }
  return dump;
}

template <typename Map>
auto KeysOf(const Map& a, const Map& b) -> std::set<typename Map::key_type> {
  std::set<typename Map::key_type> keys;
  for (const auto& [key, value] : a) {
    keys.insert(key);
  }
  for (const auto& [key, value] : b) {
    keys.insert(key);
  }
  return keys;
}

auto Spelled(const std::set<std::string>& items) -> std::string {
  std::string text;
  for (const std::string& item : items) {
    text += text.empty() ? "" : ", ";
    text += item;
  }
  return text.empty() ? "none" : text;
}

auto Spelled(const std::vector<Section>& sections) -> std::string {
  std::string text;
  for (const Section& section : sections) {
    text += text.empty() ? "" : "; ";
    text += section.keyword;
    for (const auto& [name, value] : section.values) {
      text += std::format(" {}={}", name, value);
    }
  }
  return text.empty() ? "none" : text;
}

auto ValuesDiffer(
    std::uint64_t time, const std::map<std::string, std::string>& want_values,
    const std::map<std::string, std::string>& got_values)
    -> std::optional<std::string> {
  for (const std::string& name : KeysOf(want_values, got_values)) {
    const auto want = want_values.find(name);
    const auto got = got_values.find(name);
    if (got == got_values.end()) {
      return std::format(
          "at {} fs no change of '{}' is recorded, where {} is expected", time,
          name, want->second);
    }
    if (want == want_values.end()) {
      return std::format(
          "at {} fs '{}' changes to {}, where no change of it is expected",
          time, name, got->second);
    }
    if (want->second != got->second) {
      return std::format(
          "at {} fs '{}' changes to {}, where {} is expected", time, name,
          got->second, want->second);
    }
  }
  return std::nullopt;
}

auto Difference(const Dump& expected, const Dump& written)
    -> std::optional<std::string> {
  if (expected.scopes != written.scopes) {
    return std::format(
        "the scopes are {}, where {} are expected", Spelled(written.scopes),
        Spelled(expected.scopes));
  }
  for (const std::string& name : KeysOf(expected.declared, written.declared)) {
    const auto want = expected.declared.find(name);
    const auto got = written.declared.find(name);
    if (got == written.declared.end()) {
      return std::format("'{}' is not declared", name);
    }
    if (want == expected.declared.end()) {
      return std::format(
          "'{}' is declared, and no declaration of it is expected", name);
    }
    if (want->second != got->second) {
      return std::format(
          "'{}' is declared as {} {}, where {} {} is expected", name,
          got->second.kind, got->second.size, want->second.kind,
          want->second.size);
    }
  }
  static const Moment kNothing;
  for (const std::uint64_t time : KeysOf(expected.moments, written.moments)) {
    const Moment& want =
        expected.moments.contains(time) ? expected.moments.at(time) : kNothing;
    const Moment& got =
        written.moments.contains(time) ? written.moments.at(time) : kNothing;
    if (want.sections != got.sections) {
      return std::format(
          "at {} fs the sections are {}, where {} are expected", time,
          Spelled(got.sections), Spelled(want.sections));
    }
    if (auto differ = ValuesDiffer(time, want.changes, got.changes)) {
      return differ;
    }
  }
  return std::nullopt;
}

}  // namespace

auto DumpUnreadable(std::string_view text) -> std::optional<std::string> {
  const std::expected<Dump, std::string> read = Parse(text);
  if (!read) {
    return read.error();
  }
  return std::nullopt;
}

auto DumpDifference(std::string_view expected, std::string_view written)
    -> std::optional<std::string> {
  const std::expected<Dump, std::string> want = Parse(expected);
  if (!want) {
    return std::format("the expected dump does not parse: {}", want.error());
  }
  const std::expected<Dump, std::string> got = Parse(written);
  if (!got) {
    return std::format("the dump does not parse: {}", got.error());
  }
  return Difference(*want, *got);
}

}  // namespace conformance::framework
