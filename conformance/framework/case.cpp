#include "conformance/framework/case.hpp"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/overloaded.hpp"
#include "conformance/framework/value_change_dump.hpp"

namespace conformance::framework {
namespace {

auto ReadFile(const std::filesystem::path& path) -> std::string {
  std::ifstream in(path, std::ios::binary);
  if (!in) {
    throw std::runtime_error(
        std::format("cannot open '{}' for read", path.string()));
  }
  std::ostringstream out;
  out << in.rdbuf();
  return out.str();
}

auto Trim(std::string_view text) -> std::string_view {
  const auto is_space = [](unsigned char c) { return std::isspace(c) != 0; };
  while (!text.empty() && is_space(text.front())) {
    text.remove_prefix(1);
  }
  while (!text.empty() && is_space(text.back())) {
    text.remove_suffix(1);
  }
  return text;
}

auto SplitOnWhitespace(std::string_view text) -> std::vector<std::string> {
  std::vector<std::string> out;
  std::istringstream in{std::string(text)};
  std::string token;
  while (in >> token) {
    out.push_back(token);
  }
  return out;
}

struct Directive {
  std::string key;
  std::string value;
};

// The directives at the top of a source, in the order they appear. The header
// is the comment and blank lines before the first line of code, so prose
// may sit beside the directives and is passed over rather than parsed.
auto ReadDirectives(std::string_view source, const std::filesystem::path& path)
    -> std::vector<Directive> {
  std::vector<Directive> out;
  std::istringstream lines{std::string(source)};
  std::string raw;
  while (std::getline(lines, raw)) {
    const std::string_view line = Trim(raw);
    if (line.empty()) {
      continue;
    }
    if (!line.starts_with("//")) {
      break;
    }
    const std::string_view body = Trim(line.substr(2));
    if (!body.starts_with("@")) {
      continue;
    }
    const auto colon = body.find(':');
    if (colon == std::string_view::npos) {
      throw std::runtime_error(
          std::format(
              "{}: directive '{}' needs a ':' between its key and its value",
              path.string(), body));
    }
    out.push_back(
        Directive{
            .key = std::string(Trim(body.substr(1, colon - 1))),
            .value = std::string(Trim(body.substr(colon + 1)))});
  }
  return out;
}

auto ReportKindNamed(std::string_view word) -> std::optional<ReportKind> {
  for (const ReportKind kind :
       {ReportKind::kInfo, ReportKind::kWarning, ReportKind::kError,
        ReportKind::kFatal, ReportKind::kViolation, ReportKind::kFinish,
        ReportKind::kStop}) {
    if (SpellingOf(kind) == word) {
      return kind;
    }
  }
  return std::nullopt;
}

// A claim of `count=N` asks for N reports of the same description, each
// answered by one of its own.
struct ReadPattern {
  ReportPattern pattern;
  std::uint64_t count = 1;
};

// The fields of a report a claim names, as `key=value` after the kind, a value
// holding spaces written in double quotes. Nothing when the text is not in that
// form.
auto ReadReportPattern(std::string_view text) -> std::optional<ReadPattern> {
  text = Trim(text);
  const std::size_t kind_end = text.find(' ');
  const std::optional<ReportKind> kind =
      ReportKindNamed(text.substr(0, kind_end));
  if (!kind) {
    return std::nullopt;
  }
  ReadPattern read;
  ReportPattern& pattern = read.pattern;
  pattern.kind = *kind;
  std::string_view rest =
      kind_end == std::string_view::npos ? "" : text.substr(kind_end);
  while (!(rest = Trim(rest)).empty()) {
    const std::size_t equals = rest.find('=');
    if (equals == std::string_view::npos) {
      return std::nullopt;
    }
    const std::string_view key = rest.substr(0, equals);
    rest.remove_prefix(equals + 1);
    std::string_view value;
    if (rest.starts_with('"')) {
      const std::size_t close = rest.find('"', 1);
      if (close == std::string_view::npos) {
        return std::nullopt;
      }
      value = rest.substr(1, close - 1);
      rest.remove_prefix(close + 1);
    } else {
      const std::size_t end = rest.find(' ');
      value = rest.substr(0, end);
      rest.remove_prefix(end == std::string_view::npos ? rest.size() : end);
    }
    if (key == "line" || key == "time" || key == "count") {
      const std::optional<std::uint64_t> value_read = ReadDecimal(value);
      if (!value_read || (key == "count" && *value_read == 0)) {
        return std::nullopt;
      }
      if (key == "count") {
        read.count = *value_read;
      } else {
        (key == "line" ? pattern.line : pattern.time) = value_read;
      }
    } else if (key == "scope") {
      pattern.scope = std::string(value);
    } else if (key == "message") {
      pattern.message = std::string(value);
    } else {
      return std::nullopt;
    }
  }
  return read;
}

// A case sits inside the clause directory it tests, so its id names a clause
// and a subject within it. A case directly under the corpus root names no
// clause, which leaves it outside the one enumeration coverage is measured
// against.
void CheckFiledUnderClause(
    const std::string& id, const std::filesystem::path& directory) {
  if (id.find('/') == std::string::npos) {
    throw std::runtime_error(
        std::format(
            "{}: a case sits in the directory of the LRM clause it tests, and "
            "'{}' names no clause",
            directory.string(), id));
  }
}

void CheckNameAlphabet(
    const std::string& id, const std::filesystem::path& directory) {
  // A reader who has a failing case's name wants the directory it came from,
  // and a reader who has the directory wants the name to run it by. Holding the
  // two to one alphabet keeps that a substitution of the separator and nothing
  // else.
  for (const char ch : id) {
    const bool allowed = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
                         (ch >= '0' && ch <= '9') || ch == '_' || ch == '/';
    if (!allowed) {
      throw std::runtime_error(
          std::format(
              "{}: a case is named with letters, digits and underscores, so "
              "'{}' cannot be spelled as a name to run it by",
              directory.string(), id));
    }
  }
}

// The sources beside the entry one, the native sources built with them, and
// the library map saying which library each source belongs to. A
// case's directory holds exactly what the case needs, so this is a listing
// rather than a lookup of anything the case had to declare.
void CollectCompanions(const std::filesystem::path& directory, Case& c) {
  for (const auto& entry : std::filesystem::directory_iterator(directory)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    const std::filesystem::path& path = entry.path();
    const std::string extension = path.extension().string();
    if (path.filename() == kCaseLibraryMap) {
      c.library_map = path;
    } else if (extension == ".sv") {
      if (path.filename() != kCaseEntrySource) {
        c.supporting_sources.push_back(path);
      }
    } else if (extension == ".c" || extension == ".cpp") {
      c.link_sources.push_back(path);
    }
  }
  std::ranges::sort(c.supporting_sources);
  std::ranges::sort(c.link_sources);

  // A dump is the one kind of file a run is held to, since it is the one a
  // tool writes by itself of what the program did (LRM 21.7).
  const std::filesystem::path expected = directory / kCaseExpectedOutputs;
  if (!std::filesystem::is_directory(expected)) {
    return;
  }
  for (const auto& entry : std::filesystem::directory_iterator(expected)) {
    const std::filesystem::path& path = entry.path();
    const std::string extension = path.extension().string();
    if (!entry.is_regular_file() ||
        (extension != ".vcd" && extension != ".evcd")) {
      throw std::runtime_error(
          std::format(
              "{}: a case's expected file is a dump, named .vcd or .evcd",
              path.string()));
    }
    std::string text = ReadFile(path);
    if (const std::optional<std::string> unreadable = DumpUnreadable(text)) {
      throw std::runtime_error(
          std::format("{}: not a dump: {}", path.string(), *unreadable));
    }
    c.expected_outputs.push_back(
        ExpectedOutput{
            .name = path.filename().string(), .text = std::move(text)});
  }
  std::ranges::sort(c.expected_outputs, {}, &ExpectedOutput::name);
}

auto ParseCase(
    const std::filesystem::path& corpus_root,
    const std::filesystem::path& harness,
    const std::filesystem::path& directory) -> Case {
  Case c;
  c.directory = directory;
  c.id = directory.lexically_relative(corpus_root).generic_string();
  CheckNameAlphabet(c.id, directory);
  CheckFiledUnderClause(c.id, directory);
  CollectCompanions(directory, c);

  const std::filesystem::path entry = directory / kCaseEntrySource;
  c.entry = entry;
  std::unordered_set<std::string> seen;
  std::optional<std::string> claimed_by;
  const auto claim = [&](const Directive& d, Claim stated) {
    if (claimed_by) {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' and '@{}' each state what the run is held to, and a "
              "case states one requirement",
              entry.string(), *claimed_by, d.key));
    }
    claimed_by = d.key;
    c.claim = std::move(stated);
  };
  const auto take_no_value = [&](const Directive& d) {
    if (!d.value.empty()) {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' takes no value, and was given '{}'", entry.string(),
              d.key, d.value));
    }
  };
  // Each report a case requires is a line of its own, one report answering
  // each, so that line is the one directive given more than once.
  IssuesReports reported;
  for (const Directive& d : ReadDirectives(ReadFile(entry), entry)) {
    if (d.key != "reports" && !seen.insert(d.key).second) {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' is given twice; a directive states its whole value "
              "once",
              entry.string(), d.key));
    }
    if (d.key == "rejected") {
      take_no_value(d);
      claim(d, RejectedBeforeRunning{});
    } else if (d.key == "reports-none") {
      IssuesNoReportOf forbidden;
      for (const std::string& word : SplitOnWhitespace(d.value)) {
        const std::optional<ReportKind> kind = ReportKindNamed(word);
        if (!kind) {
          throw std::runtime_error(
              std::format(
                  "{}: '@reports-none' names the kinds of report the run must "
                  "not issue, and '{}' is not one",
                  entry.string(), word));
        }
        forbidden.kinds.push_back(*kind);
      }
      if (forbidden.kinds.empty()) {
        throw std::runtime_error(
            std::format(
                "{}: '@reports-none' names the kinds of report the run must "
                "not issue, and names none",
                entry.string()));
      }
      claim(d, std::move(forbidden));
    } else if (d.key == "compilation-units") {
      if (d.value == "single") {
        c.compilation_units = CompilationUnits::kSingle;
      } else if (d.value == "separate") {
        c.compilation_units = CompilationUnits::kSeparate;
      } else {
        throw std::runtime_error(
            std::format(
                "{}: '@compilation-units' is 'single' or 'separate', and was "
                "given '{}'",
                entry.string(), d.value));
      }
    } else if (d.key == "reports") {
      std::optional<ReadPattern> pattern = ReadReportPattern(d.value);
      if (!pattern) {
        throw std::runtime_error(
            std::format(
                "{}: '@reports' names a kind of report and what the standard "
                "fixes of it, as 'error line=12 time=5 scope=Top "
                "message=\"...\" count=2', and was given '{}'",
                entry.string(), d.value));
      }
      if (reported.reports.empty()) {
        claim(d, IssuesReports{});
      }
      reported.reports.insert(
          reported.reports.end(), pattern->count, pattern->pattern);
    } else if (d.key == "includes") {
      // The corpus's own helpers a case is written against, compiled ahead of
      // everything the case holds, as test262's harness includes are.
      std::vector<std::filesystem::path> included;
      for (const std::string& name : SplitOnWhitespace(d.value)) {
        const std::filesystem::path helper = harness / name;
        if (!std::filesystem::exists(helper)) {
          throw std::runtime_error(
              std::format(
                  "{}: '@includes' names '{}', which the corpus's harness "
                  "does not hold",
                  entry.string(), name));
        }
        included.push_back(helper);
      }
      c.supporting_sources.insert(
          c.supporting_sources.begin(), included.begin(), included.end());
    } else if (d.key == "top") {
      c.tops = SplitOnWhitespace(d.value);
    } else if (d.key == "argv") {
      c.program_args = SplitOnWhitespace(d.value);
    } else {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' is not a directive this corpus defines",
              entry.string(), d.key));
    }
  }

  if (!reported.reports.empty()) {
    c.claim = std::move(reported);
  }
  if (c.tops.empty()) {
    c.tops.emplace_back("Top");
  }
  return c;
}

}  // namespace

auto ReadDecimal(std::string_view text) -> std::optional<std::uint64_t> {
  std::uint64_t number = 0;
  const auto [end, error] =
      std::from_chars(text.data(), text.data() + text.size(), number);
  if (error != std::errc{} || end != text.data() + text.size()) {
    return std::nullopt;
  }
  return number;
}

namespace {

// A kind of report as a reader is told of it, one of them or several.
auto ReportNamed(ReportKind kind, std::size_t count) -> std::string {
  const auto named = [&](std::string_view one, std::string_view many) {
    return count == 1 ? std::string(one) : std::format("{} {}", count, many);
  };
  switch (kind) {
    case ReportKind::kInfo:
      return named("an info report", "info reports");
    case ReportKind::kWarning:
      return named("a warning", "warnings");
    case ReportKind::kError:
      return named("an error", "errors");
    case ReportKind::kFatal:
      return named("a fatal error", "fatal errors");
    case ReportKind::kViolation:
      return named("a violation report", "violation reports");
    case ReportKind::kFinish:
      return named("the message $finish prints", "messages of $finish");
    case ReportKind::kStop:
      return named("the message $stop prints", "messages of $stop");
  }
  return named("a report", "reports");
}

}  // namespace

auto DescribeReport(const ReportPattern& report, std::size_t count)
    -> std::string {
  std::string text = ReportNamed(report.kind, count);
  if (report.line) {
    text += std::format(" from line {}", *report.line);
  }
  if (report.scope) {
    text += std::format(" in {}", *report.scope);
  }
  if (report.time) {
    text += std::format(" at time {}", *report.time);
  }
  if (report.message) {
    text += std::format(" with the message \"{}\"", *report.message);
  }
  return text;
}

namespace {

auto Joined(const std::vector<std::string>& parts, std::string_view last)
    -> std::string {
  std::string text;
  for (std::size_t i = 0; i < parts.size(); ++i) {
    if (i > 0) {
      text += i + 1 == parts.size() ? std::format(" {} ", last) : ", ";
    }
    text += parts[i];
  }
  return text;
}

}  // namespace

auto Describe(const Claim& claim) -> std::string {
  return std::visit(
      Overloaded{
          [](const RunsToItsChecks&) -> std::string {
            return "The program must run and pass every check it makes.";
          },
          [](const RejectedBeforeRunning&) -> std::string {
            return "A tool must refuse this program before it runs.";
          },
          [](const IssuesReports& required) -> std::string {
            // Equal reports are named once with how many, in the order the
            // case first asks for each.
            std::vector<std::string> reports;
            for (auto it = required.reports.begin();
                 it != required.reports.end(); ++it) {
              if (std::ranges::find(required.reports.begin(), it, *it) != it) {
                continue;
              }
              reports.push_back(DescribeReport(
                  *it, static_cast<std::size_t>(
                           std::ranges::count(required.reports, *it))));
            }
            return std::format(
                "The program must issue {}.", Joined(reports, "and"));
          },
          [](const IssuesNoReportOf& forbidden) -> std::string {
            std::vector<std::string> kinds;
            for (const ReportKind kind : forbidden.kinds) {
              kinds.push_back(ReportNamed(kind, 1));
            }
            return std::format(
                "The program must pass every check without issuing {}.",
                Joined(kinds, "or"));
          }},
      claim);
}

auto SpellingOf(ReportKind kind) -> std::string_view {
  switch (kind) {
    case ReportKind::kInfo:
      return "info";
    case ReportKind::kWarning:
      return "warning";
    case ReportKind::kError:
      return "error";
    case ReportKind::kFatal:
      return "fatal";
    case ReportKind::kViolation:
      return "violation";
    case ReportKind::kFinish:
      return "finish";
    case ReportKind::kStop:
      return "stop";
  }
  return "info";
}

auto MayEndTheRun(ReportKind kind) -> bool {
  switch (kind) {
    case ReportKind::kError:
    case ReportKind::kFatal:
    case ReportKind::kViolation:
    case ReportKind::kStop:
      return true;
    case ReportKind::kInfo:
    case ReportKind::kWarning:
    case ReportKind::kFinish:
      return false;
  }
  return true;
}

auto LoadCases(
    const std::filesystem::path& corpus_root,
    const std::filesystem::path& harness) -> std::vector<Case> {
  std::vector<Case> cases;
  if (!std::filesystem::exists(corpus_root)) {
    return cases;
  }
  // A case's id is where the walk found it, subtracted textually from the root
  // it was found under. A corpus assembled for a test run links each file in
  // rather than copying the tree, so asking the filesystem where a source
  // really lives would answer from outside that root and yield no id at all.
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(corpus_root)) {
    if (entry.is_directory() &&
        std::filesystem::exists(entry.path() / kCaseEntrySource)) {
      cases.push_back(ParseCase(corpus_root, harness, entry.path()));
    }
  }
  std::ranges::sort(cases, {}, &Case::id);
  return cases;
}

auto LoadSuite(const std::filesystem::path& directory) -> Suite {
  const std::filesystem::path harness = directory / "harness";
  return Suite{
      .cases = LoadCases(directory / "cases", harness),
      .helper_checks = LoadCases(directory / "selftest", harness)};
}

}  // namespace conformance::framework
