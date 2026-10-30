// The initializers of static variables are all run before any procedure starts,
// and in no order the standard fixes among themselves (LRM 6.8, 10.5). One that
// reads another static variable may therefore read it before or after that
// variable's own initializer has run, and a case whose expected answer leans on
// one of the two is not a statement of the standard. This finds every such read
// in the corpus -- directly, or through a subroutine the initializer calls --
// so that it is looked at; it says nothing of any tool, and an automatic
// variable's initializer, which runs as its block is entered, is no part of it.

#include <algorithm>
#include <array>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <unistd.h>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/driver/Driver.h>
#include <slang/text/SourceManager.h>

#include "conformance/adapters/lyra/reader.hpp"
#include "conformance/framework/case.hpp"
#include "lyra/compiler/compile.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

using conformance::adapters::lyra::DesignArguments;
using conformance::framework::Case;
using conformance::framework::LoadCases;

namespace {

auto IsStatic(const slang::ast::Symbol& symbol) -> bool {
  const auto* variable = symbol.as_if<slang::ast::VariableSymbol>();
  return variable != nullptr &&
         symbol.kind != slang::ast::SymbolKind::FormalArgument &&
         variable->lifetime == slang::ast::VariableLifetime::Static;
}

// Whether `symbol` is declared inside `scope`, at any depth.
auto DeclaredWithin(
    const slang::ast::Symbol& symbol, const slang::ast::Scope& scope) -> bool {
  for (const slang::ast::Scope* at = symbol.getParentScope(); at != nullptr;
       at = at->asSymbol().getParentScope()) {
    if (at == &scope) return true;
  }
  return false;
}

// The system functions that ask about an operand's type, whose operand is
// therefore not read (LRM 20.6, 20.7).
constexpr std::array<std::string_view, 11> kTypeQueries = {
    "$bits",      "$typename", "$dimensions", "$unpacked_dimensions",
    "$left",      "$right",    "$low",        "$high",
    "$increment", "$size",     "$isunbounded"};

// The static variables an initializer reads, other than its own, and those
// the subroutines it calls read and write that they do not declare
// themselves.
struct StaticAccesses
    : slang::ast::ASTVisitor<StaticAccesses, slang::ast::VisitFlags::AllGood> {
  const slang::ast::Symbol* own = nullptr;
  const slang::ast::Scope* inside = nullptr;
  bool writing = false;
  std::unordered_set<const slang::ast::SubroutineSymbol*> entered;
  std::set<const slang::ast::Symbol*> read;
  std::set<const slang::ast::Symbol*> written;

  void Note(const slang::ast::Symbol& symbol) {
    if (&symbol == own || !IsStatic(symbol)) return;
    if (inside != nullptr && DeclaredWithin(symbol, *inside)) return;
    (writing ? written : read).insert(&symbol);
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    Note(e.symbol);
    visitDefault(e);
  }

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Note(e.symbol);
    visitDefault(e);
  }

  void handle(const slang::ast::AssignmentExpression& e) {
    const bool outer = writing;
    writing = inside != nullptr;
    e.left().visit(*this);
    writing = outer;
    e.right().visit(*this);
  }

  void handle(const slang::ast::CallExpression& call) {
    if (call.isSystemCall()) {
      if (std::ranges::find(kTypeQueries, call.getSubroutineName()) ==
          kTypeQueries.end()) {
        visitDefault(call);
      }
      return;
    }
    visitDefault(call);
    const auto* subroutine =
        std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
    if (subroutine == nullptr || !entered.insert(subroutine).second) return;
    const slang::ast::Scope* outer = inside;
    inside = subroutine;
    subroutine->getBody().visit(*this);
    inside = outer;
  }
};

// One static variable with an initializer, and what that initializer reaches.
struct Initialized {
  const slang::ast::VariableSymbol* variable;
  StaticAccesses accesses;
};

// Every static variable with an initializer the design declares.
struct EveryInitializer
    : slang::ast::ASTVisitor<
          EveryInitializer, slang::ast::VisitFlags::AllGood> {
  std::vector<Initialized> found;

  template <typename T>
    requires std::is_base_of_v<slang::ast::VariableSymbol, T>
  void handle(const T& variable) {
    if (IsStatic(variable)) {
      if (const slang::ast::Expression* initializer =
              variable.getInitializer()) {
        Initialized entry{.variable = &variable, .accesses = {}};
        entry.accesses.own = &variable;
        initializer->visit(entry.accesses);
        found.push_back(std::move(entry));
      }
    }
    visitDefault(variable);
  }
};

// The reads whose value depends on the order initializers run in: of a
// variable that has an initializer of its own, or that another initializer
// writes through a subroutine it calls. A variable no other initializer gives a
// value holds the same value whichever runs first.
auto OrderDependentReads(
    const std::vector<Initialized>& initialized,
    const slang::SourceManager& sources) -> std::vector<std::string> {
  std::map<const slang::ast::Symbol*, std::set<const Initialized*>> givers;
  for (const Initialized& entry : initialized) {
    givers[entry.variable].insert(&entry);
    for (const slang::ast::Symbol* written : entry.accesses.written) {
      givers[written].insert(&entry);
    }
  }
  std::vector<std::string> found;
  for (const Initialized& entry : initialized) {
    std::set<std::string> names;
    for (const slang::ast::Symbol* read : entry.accesses.read) {
      const auto given = givers.find(read);
      if (given == givers.end()) continue;
      const bool by_another = std::ranges::any_of(
          given->second,
          [&](const Initialized* giver) { return giver != &entry; });
      if (by_another) names.insert(std::string(read->name));
    }
    if (names.empty()) continue;
    std::string spelled;
    for (const std::string& name : names) {
      spelled += spelled.empty() ? "" : ", ";
      spelled += name;
    }
    found.push_back(
        fmt::format(
            "line {}: {} reads {}",
            sources.getLineNumber(entry.variable->location),
            entry.variable->name, spelled));
  }
  return found;
}

// The cases whose reads were looked at and found to accept the value every
// order gives, each with what makes it so.
const std::map<std::string, std::string, std::less<>> kReviewed = {
    {"26_packages/variable_initializer_reads_another_package",
     "accepts both the other package's value and its type's default"},
    {"08_classes/specialization_adds_only_its_own_class",
     "accepts the two counts in either order, by their sum"},
};

// What the initializers of a design read, or nothing where it does not
// elaborate -- a case the standard requires a tool to reject runs nothing.
auto ReadsOfStaticInitializers(const Case& test_case)
    -> std::optional<std::vector<std::string>> {
  slang::driver::Driver driver;
  driver.addStandardArgs();
  std::vector<std::string> args{"lyra"};
  const std::vector<std::string> design = DesignArguments(test_case);
  args.insert(args.end(), design.begin(), design.end());
  std::vector<const char*> argv;
  argv.reserve(args.size());
  for (const std::string& arg : args) {
    argv.push_back(arg.c_str());
  }
  if (!driver.parseCommandLine(static_cast<int>(argv.size()), argv.data())) {
    return std::nullopt;
  }
  const compiler::FrontEndResult front = compiler::RunFrontEnd(driver);
  if (!front.elaborated) return std::nullopt;
  EveryInitializer every;
  front.elaborated->compilation->getRoot().visit(every);
  return OrderDependentReads(every.found, driver.sourceManager);
}

auto DesignOf(const std::filesystem::path& dir, std::string_view text) -> Case {
  std::filesystem::create_directories(dir);
  const std::filesystem::path path = dir / "main.sv";
  std::ofstream(path) << text;
  Case c;
  c.id = dir.filename().string();
  c.directory = dir;
  c.entry = path;
  c.tops = {"Top"};
  return c;
}

auto FoundIn(std::string_view name, std::string_view text)
    -> std::vector<std::string> {
  const std::filesystem::path dir =
      std::filesystem::temp_directory_path() /
      fmt::format("lyra-initializers-{}-{}", ::getpid(), name);
  const auto found = ReadsOfStaticInitializers(DesignOf(dir, text));
  std::filesystem::remove_all(dir);
  return found.value_or(std::vector<std::string>{"did not elaborate"});
}

// Held both ways, since a check that never finds anything passes every corpus.
TEST(StaticInitializerOrderTest, AReadOfAnotherStaticIsFoundAndNoOtherIs) {
  EXPECT_EQ(
      FoundIn(
          "module_static", "module Top; int a = 5; int b = a + 1; endmodule"),
      std::vector<std::string>{"line 1: b reads a"});
  EXPECT_EQ(
      FoundIn(
          "block_static",
          "module Top; int a = 5;\n"
          "initial begin static int b = a; end endmodule"),
      std::vector<std::string>{"line 2: b reads a"});
  EXPECT_EQ(
      FoundIn(
          "through_a_call",
          "module Top; int a = 5;\n"
          "function automatic int get(); return a; "
          "endfunction\n"
          "int b = get(); endmodule"),
      std::vector<std::string>{"line 3: b reads a"});
  EXPECT_TRUE(FoundIn(
                  "automatic_locals",
                  "module Top; task automatic run(); int a = 5; int b = a + 1; "
                  "endtask initial run(); endmodule")
                  .empty());
  EXPECT_TRUE(FoundIn(
                  "a_constant",
                  "module Top; localparam int P = 5; int b = P + 1; endmodule")
                  .empty());
  EXPECT_TRUE(
      FoundIn(
          "a_call_reading_its_own",
          "module Top; function automatic int get(); int x = 3; return x; "
          "endfunction int b = get(); endmodule")
          .empty());
  // A variable no initializer gives a value to holds its default from the
  // start, whatever runs first, unless another initializer writes it.
  EXPECT_TRUE(FoundIn(
                  "no_initializer_of_its_own",
                  "module Top; event a; event b = a; endmodule")
                  .empty());
  EXPECT_TRUE(
      FoundIn(
          "read_and_written_by_one",
          "module Top; int n;\n"
          "function automatic int bump(); n = n + 1; return n; endfunction\n"
          "int once = bump(); endmodule")
          .empty());
  EXPECT_EQ(
      FoundIn(
          "written_by_another",
          "module Top; int a;\n"
          "function automatic int set(); a = 4; return 1; endfunction\n"
          "int done = set();\n"
          "int b = a; endmodule"),
      std::vector<std::string>{"line 4: b reads a"});
  // A query of a type reads no value.
  EXPECT_TRUE(FoundIn(
                  "a_query",
                  "module Top; logic [7:0] v = 1; int w = $bits(v); "
                  "endmodule")
                  .empty());
}

// The corpus states only what the standard requires, so no case leans on the
// order its static initializers run in. One whose reads were reviewed and
// accept the value every order gives is listed with why; one that pins an
// order is a choice of a tool's, and lives with the choices.
TEST(StaticInitializerOrderTest, NoCaseLeansOnTheOrderOfItsInitializers) {
  std::string error;
  const std::unique_ptr<bazel::tools::cpp::runfiles::Runfiles> runfiles(
      bazel::tools::cpp::runfiles::Runfiles::CreateForTest(&error));
  ASSERT_NE(runfiles, nullptr) << error;
  const std::vector<Case> corpus = LoadCases(
      runfiles->Rlocation("_main/conformance/cases"),
      runfiles->Rlocation("_main/conformance/harness"));
  ASSERT_FALSE(corpus.empty());
  std::string report;
  for (const Case& c : corpus) {
    if (kReviewed.contains(c.id)) continue;
    const auto found = ReadsOfStaticInitializers(c);
    if (!found) continue;
    for (const std::string& line : *found) {
      report += fmt::format("{}: {}\n", c.id, line);
    }
  }
  EXPECT_TRUE(report.empty())
      << "static initializers reading another static variable, whose value "
         "the standard leaves to the order initializers run in:\n"
      << report;
}

}  // namespace
}  // namespace lyra::test
