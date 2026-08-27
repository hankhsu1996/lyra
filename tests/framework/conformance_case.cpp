#include "conformance_case.hpp"

#include <algorithm>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

namespace lyra::test {
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
// is the run of comment and blank lines before the first line of code, so prose
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

// The sources beside the entry one, and the native sources built with them. A
// case's directory holds exactly what the case needs, so this is a listing
// rather than a lookup of anything the case had to declare.
void CollectCompanions(
    const std::filesystem::path& directory, ConformanceCase& c) {
  for (const auto& entry : std::filesystem::directory_iterator(directory)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    const std::filesystem::path& path = entry.path();
    const std::string extension = path.extension().string();
    if (extension == ".sv") {
      if (path.filename() != kCaseEntrySource) {
        c.supporting_sources.push_back(path);
      }
    } else if (extension == ".c" || extension == ".cpp") {
      c.link_sources.push_back(path);
    }
  }
  std::ranges::sort(c.supporting_sources);
  std::ranges::sort(c.link_sources);
}

auto ParseCase(
    const std::filesystem::path& corpus_root,
    const std::filesystem::path& directory, std::string_view entry_name)
    -> ConformanceCase {
  ConformanceCase c;
  c.directory = directory;
  c.id = directory.lexically_relative(corpus_root).generic_string();
  CheckNameAlphabet(c.id, directory);
  CheckFiledUnderClause(c.id, directory);
  CollectCompanions(directory, c);

  const std::filesystem::path entry = directory / entry_name;
  c.entry = entry;
  std::unordered_set<std::string> seen;
  for (const Directive& d : ReadDirectives(ReadFile(entry), entry)) {
    if (!seen.insert(d.key).second) {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' is given twice; a directive states its whole value "
              "once",
              entry.string(), d.key));
    }
    if (d.key == "top") {
      c.tops = SplitOnWhitespace(d.value);
    } else if (d.key == "args") {
      c.front_end_args = SplitOnWhitespace(d.value);
    } else if (d.key == "argv") {
      c.program_args = SplitOnWhitespace(d.value);
    } else if (d.key == "error") {
      c.required_error = d.value;
    } else {
      throw std::runtime_error(
          std::format(
              "{}: '@{}' is not a directive this corpus defines",
              entry.string(), d.key));
    }
  }

  if (c.tops.empty()) {
    c.tops.emplace_back("Top");
  }
  return c;
}

}  // namespace

namespace {

auto LoadEntered(
    const std::filesystem::path& corpus_root, std::string_view entry_name)
    -> std::vector<ConformanceCase> {
  std::vector<ConformanceCase> cases;
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
        std::filesystem::exists(entry.path() / entry_name)) {
      cases.push_back(ParseCase(corpus_root, entry.path(), entry_name));
    }
  }
  std::ranges::sort(cases, {}, &ConformanceCase::id);
  return cases;
}

}  // namespace

auto LoadConformanceCases(const std::filesystem::path& corpus_root)
    -> std::vector<ConformanceCase> {
  return LoadEntered(corpus_root, kCaseEntrySource);
}

auto LoadParkedCases(const std::filesystem::path& corpus_root)
    -> std::vector<ConformanceCase> {
  return LoadEntered(corpus_root, kParkedCaseEntry);
}

}  // namespace lyra::test
