#include <algorithm>
#include <array>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <regex>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/driver/project_layout.hpp"
#include "lyra/support/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

// A design stating the constructs an emitted unit reaches the runtime through:
// variables of the value types a design does not shape and of one it does, a
// class handle, a net and its driver, a named event, sampled history, a
// function, a task, a fork, the event controls, a delay, a level wait, a
// nonblocking assignment and a display.
constexpr std::string_view kDesignSource = R"sv(
module Test;
  typedef struct { logic [3:0] x; logic [3:0] y; } pair_t;

  class Counter;
    int count;
    function void bump();
      count = count + 1;
    endfunction
  endclass

  logic clk = 0;
  logic [7:0] a = 0;
  logic [7:0] b;
  logic [7:0] q;
  string s = "x";
  real r = 1.5;
  wire [7:0] w;
  event e;
  pair_t p;
  Counter h;
  logic [3:0] px;

  assign w = a + 8'd1;

  always_comb b = a ^ 8'hff;

  always_comb px = p.x;

  always @(posedge clk) begin
    q <= a;
    if ($past(a) == 8'd3) s = "seen";
  end

  function automatic logic [7:0] inc(input logic [7:0] v);
    return v + 8'd1;
  endfunction

  task automatic step(input logic [7:0] v);
    #1 a = v;
  endtask

  initial begin
    h = new;
    h.bump();
    fork
      step(8'd3);
      #2 -> e;
      @(e);
    join
    wait (a == 8'd3);
    p.x = 4'd1;
    a = inc(a);
    repeat (4) #1 clk = ~clk;
    r = r * 2.0;
    $display("%0d %0d %0d %s %f %0d %0d", b, q, w, s, r, h.count, px);
    $finish;
  end
endmodule
)sv";

// Whether a definition an emitted unit holds is one the unit's own design
// decided. That is so where it names something of a unit's -- its scopes, the
// lambdas its bodies write -- or where a template of the runtime is
// instantiated over a shape the design chose: a fork's branch count, or a value
// type composed from the design's declarations. A unit's own names sit in the
// namespace its file is named for, and are looked for in both spellings,
// because the names a compiler gives lambdas do not always demangle.
auto IsTheDesignsOwn(
    std::string_view mangled, std::string_view demangled,
    std::span<const std::string> unit_namespaces) -> bool {
  const bool names_a_unit =
      std::ranges::any_of(unit_namespaces, [&](const std::string& unit) {
        return demangled.contains(unit + "::") ||
               mangled.contains(std::format("{}{}", unit.size(), unit));
      });
  if (names_a_unit) {
    return true;
  }
  static constexpr std::array<std::string_view, 7> kDesignShaped = {
      "lyra::value::Tuple<lyra",        "lyra::value::UnpackedArray<",
      "lyra::value::DynamicArray<",     "lyra::value::Queue<",
      "lyra::value::AssociativeArray<", "lyra::value::Union<",
      "lyra::value::TaggedUnion<"};
  if (std::ranges::any_of(kDesignShaped, [&](std::string_view shape) {
        return demangled.contains(shape);
      })) {
    return true;
  }
  static const std::regex kBranchCount{R"(<\d+ul>)"};
  return std::regex_search(demangled.begin(), demangled.end(), kBranchCount);
}

// What the runtime writes in its headers on purpose: the write path asks it on
// every store, and the library's own writes fold it. A unit writing a cell of a
// type its design shaped therefore carries these; each is a single comparison.
// The list is what makes that a decision rather than a drift: an entry is added
// only with the reason it has to be read where it is used.
constexpr std::array<std::string_view, 2> kFoldedByTheWritePath = {
    "lyra::runtime::Observable::HasWaiter() const",
    "lyra::runtime::RegistrationList::Empty() const"};

// Whether a symbol is an entity of the runtime namespace itself, or something
// local to one, rather than a standard-library template that merely names a
// runtime type among its arguments -- which a unit instantiates for its own
// uses and which follows from whatever runtime entity it reaches anyway.
auto IsRuntimeEntity(std::string_view mangled) -> bool {
  static constexpr std::array<std::string_view, 4> kPrefixes = {
      "_ZN4lyra7runtime", "_ZNK4lyra7runtime", "_ZZN4lyra7runtime",
      "_ZZNK4lyra7runtime"};
  return std::ranges::any_of(kPrefixes, [&](std::string_view prefix) {
    return mangled.starts_with(prefix);
  });
}

// The name field of each line `nm` prints, in order.
auto SymbolNames(std::string_view listing) -> std::vector<std::string> {
  std::vector<std::string> names;
  std::size_t at = 0;
  while (at < listing.size()) {
    const std::size_t end = std::min(listing.find('\n', at), listing.size());
    const std::string_view line = listing.substr(at, end - at);
    at = end + 1;
    // An address, a one-letter kind, and the name, each separated by a space.
    const std::size_t kind = line.find(' ');
    if (kind == std::string_view::npos || kind + 3 > line.size()) {
      continue;
    }
    names.emplace_back(line.substr(kind + 3));
  }
  return names;
}

}  // namespace

// A translation unit pays for what it contains. A unit of an emitted project
// contains one class per scope its design describes, and the operations a scope
// offers are the shipped library's to define -- so what this unit hands the
// linker is its own code and calls into that library, never a copy of what the
// library already holds.
//
// Every way that stops being true is silent, which is why this reads the
// objects rather than the headers. A function written in a header is compiled
// again by every unit that reaches it, along with everything its body reaches.
// A polymorphic class with no virtual function defined in the library has no
// unit of its own, so every unit that builds one copies its dispatch table. A
// constructor or destructor defaulted where it is declared, or left implicit,
// is defined by each unit that uses it, and a family stated as already
// compiled does not withhold it. Nothing in a header looks wrong in any of
// these, and no warning names one; the object names every one of them.
TEST(RuntimeSurface, AUnitCarriesNoneOfWhatTheLibraryDefines) {
  auto cxx_or = lyra::test::FindDefaultCxx();
  if (!cxx_or) {
    GTEST_SKIP() << "measuring an object requires the compiler a project's "
                    "build recipe defaults to";
  }
  auto nm_or = lyra::support::FindOnPath("nm");
  if (!nm_or) {
    GTEST_SKIP() << "reading what an object defines requires nm";
  }
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto dir_or = lyra::test::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();

  const auto design = *dir_or / "design.sv";
  const auto project = *dir_or / "project";
  {
    std::ofstream out(design);
    out << kDesignSource;
  }
  const std::vector<std::string> emit_args = {
      "emit", "cpp", "--top", "Test", "-o", project.string(), design.string()};
  auto emit_or = lyra::support::RunProcessCaptured(lyra_exe, emit_args);
  ASSERT_TRUE(emit_or) << emit_or.error();
  ASSERT_EQ(emit_or->exit_code, 0) << emit_or->stderr_text;

  std::vector<lyra::support::ProcessRequest> compiles;
  std::vector<std::filesystem::path> objects;
  std::vector<std::string> unit_namespaces;
  for (const auto& entry : std::filesystem::directory_iterator(project)) {
    if (entry.path().extension() != ".cpp") {
      continue;
    }
    unit_namespaces.push_back(entry.path().stem().string());
    std::filesystem::path object = entry.path();
    object.replace_extension(".o");
    compiles.push_back(
        lyra::support::ProcessRequest{
            .exe = *cxx_or,
            .args = {
                std::string{lyra::driver::kCxxStandardFlag},
                std::string{lyra::driver::OptimizationFlag(
                    lyra::driver::Optimization::kIterate)},
                "-I", (project / "runtime" / "include").string(), "-I",
                project.string(), "-c", entry.path().string(), "-o",
                object.string()}});
    objects.push_back(std::move(object));
  }
  ASSERT_FALSE(objects.empty()) << "the design emitted no translation unit";
  auto compiled_or = lyra::support::RunProcessesCaptured(compiles, 4);
  ASSERT_TRUE(compiled_or) << compiled_or.error();
  for (const auto& compiled : *compiled_or) {
    ASSERT_EQ(compiled.exit_code, 0) << "an emitted unit did not compile:\n"
                                     << compiled.stderr_text;
  }

  std::string carried;
  for (const auto& object : objects) {
    const std::vector<std::string> mangled_args = {
        "--defined-only", object.string()};
    const std::vector<std::string> demangled_args = {
        "--defined-only", "--demangle", object.string()};
    auto mangled_or = lyra::support::RunProcessCaptured(*nm_or, mangled_args);
    auto demangled_or =
        lyra::support::RunProcessCaptured(*nm_or, demangled_args);
    ASSERT_TRUE(mangled_or) << mangled_or.error();
    ASSERT_TRUE(demangled_or) << demangled_or.error();
    const std::vector<std::string> mangled =
        SymbolNames(mangled_or->stdout_text);
    const std::vector<std::string> demangled =
        SymbolNames(demangled_or->stdout_text);
    ASSERT_EQ(mangled.size(), demangled.size());
    for (std::size_t at = 0; at < mangled.size(); ++at) {
      if (IsRuntimeEntity(mangled[at]) &&
          !IsTheDesignsOwn(mangled[at], demangled[at], unit_namespaces) &&
          !std::ranges::contains(kFoldedByTheWritePath, demangled[at])) {
        carried += std::format(
            "  {}: {}\n", object.filename().string(), demangled[at]);
      }
    }
  }

  EXPECT_TRUE(carried.empty()) << std::format(
      "emitted units define what the runtime owns:\n{}Each is compiled again "
      "by every unit that reaches it. A function the library alone decides is "
      "defined in the library's own source; a class template over a value "
      "type the design does not shape is stated as already compiled, with its "
      "constructor and destructor defaulted outside the class so that the "
      "statement reaches them too.",
      carried);
}
