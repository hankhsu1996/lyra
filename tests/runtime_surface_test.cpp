#include <algorithm>
#include <array>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <optional>
#include <ranges>
#include <regex>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/driver/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

// A design stating the constructs an emitted unit reaches the runtime through:
// variables of the value types a design does not shape and of ones it does, a
// class handle, a net and its driver, a named event, sampled history, a
// function, a task, a fork, the event controls, a delay, a level wait, a
// nonblocking assignment, a write to some bits of a value, a value wider than
// a machine word, and a display.
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
  logic [99:0] wide = '0;

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
    q[3:0] = 4'd2;
    wide = wide + 100'd1;
    wide[70] = 1'b1;
    repeat (4) #1 clk = ~clk;
    r = r * 2.0;
    $display("%0d %0d %0d %s %f %0d %0d", b, q, w, s, r, h.count, px);
    $finish;
  end
endmodule
)sv";

// A design stating a procedural continuous assignment on each kind of value no
// design shapes that the language lets one name (a `chandle` it does not, LRM
// 6.14), through its variable, and through a port standing for what drives it
// on each kind a port may hold.
constexpr std::string_view kTakeoverDesignSource = R"sv(
interface Bus;
endinterface

class C;
endclass

module Port (input string s, input real r, input shortreal sr, input C c);
endmodule

module Test;
  Bus bus ();
  virtual Bus v;
  virtual Bus w;
  string s = "x";
  real r = 1.5;
  shortreal sr = 1.5;
  C h;
  C other;

  Port port (s, r, sr, h);

  initial begin
    w = bus;
    force v = w;
    release v;
    force s = "y";
    force r = 2.5;
    force sr = 2.5;
    force h = other;
    release s;
    release r;
    release sr;
    release h;
    assign s = "z";
    assign r = 3.5;
    deassign s;
    deassign r;
    force port.s = "y";
    force port.r = 2.5;
    force port.sr = 2.5;
    force port.c = other;
    release port.s;
    release port.r;
    release port.sr;
    release port.c;
    $finish;
  end
endmodule
)sv";

// A design whose one module is compiled to two bodies: its two instances build
// different blocks of a conditional generate, whose processes write different
// bits and wait on different controls, so each body builds closures of its own
// under the same names; the module declares a structure whose member
// initializers its methods read, and exports a function to C under one global
// name (LRM 35.4), which every body states alike.
constexpr std::string_view kTwoBodiesDesignSource = R"sv(
module Stage #(parameter int R = 0) (input logic clk, input logic d);
  typedef struct { int x = 5; int y = 9; } pair_t;
  pair_t p;
  function int twice(int x);
    return 2 * x;
  endfunction
  export "DPI-C" function twice;
  logic [2:1] s = '0;
  if (R == 1) begin : low
    always_ff @(posedge clk) s[1] <= d;
  end else begin : high
    always_ff @(posedge clk or negedge d) s[2] <= d;
  end
endmodule

module Test;
  logic clk = 0;
  logic d = 1;
  Stage #(.R(0)) u0 (.clk, .d);
  Stage #(.R(1)) u1 (.clk, .d);
  initial begin
    #1 clk = 1;
    #1 $display("%b %b %0d", u0.s, u1.s, u1.p.x);
    $finish;
  end
endmodule
)sv";

// Whether a definition an emitted unit holds is one the unit's own design
// decided. That is so where it names something of a unit's -- its scopes, the
// lambdas its bodies write -- or where a template of the runtime is
// instantiated over a shape the design chose: a fork's branch count, a packed
// value of the width the design declared, or a value type composed from the
// design's declarations. A unit's own names sit in the namespace its file is
// named for, and are looked for in both spellings, because the names a
// compiler gives lambdas, and templates constrained by a concept, do not always
// demangle.
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
  static constexpr std::array<std::string_view, 8> kDesignShaped = {
      "lyra::value::Integral<",      "lyra::value::Tuple<lyra",
      "lyra::value::UnpackedArray<", "lyra::value::DynamicArray<",
      "lyra::value::Queue<",         "lyra::value::AssociativeArray<",
      "lyra::value::Union<",         "lyra::value::TaggedUnion<"};
  if (std::ranges::any_of(kDesignShaped, [&](std::string_view shape) {
        return demangled.contains(shape);
      })) {
    return true;
  }
  if (mangled.contains("8IntegralILm")) {
    return true;
  }
  static const std::regex kBranchCount{R"(<\d+ul>)"};
  return std::regex_search(demangled.begin(), demangled.end(), kBranchCount);
}

// What the runtime writes in its headers on purpose: the write path asks it on
// every store, and the library's own writes fold it. A unit writing a cell of a
// type its design shaped therefore carries these; each is a single comparison
// or a few stores.
// The list is what makes that a decision rather than a drift: an entry is added
// only with the reason it has to be read where it is used.
constexpr std::array<std::string_view, 8> kFoldedByTheWritePath = {
    "lyra::runtime::Observable::HasMembers() const",
    "lyra::runtime::IntrusiveList<lyra::runtime::WaitMembership>::Empty() "
    "const",
    "lyra::runtime::VariableCell::OverridesWrites() const",
    "lyra::runtime::VariableCell::Watched() const",
    "lyra::runtime::VariableCell::Rare() const",
    "lyra::runtime::ErasedReference::OverridesWrites() const",
    "lyra::runtime::ErasedReference::Watched() const",
    "lyra::runtime::PartPath::PartPath()"};

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

// The lines a tool printed, in order, empty ones left out: for `nm
// --format=just-symbols`, the names it lists.
auto LinesOf(std::string_view listing) -> std::vector<std::string> {
  std::vector<std::string> lines;
  for (const auto line : std::views::split(listing, '\n')) {
    if (!line.empty()) {
      lines.emplace_back(std::string_view(line));
    }
  }
  return lines;
}

// Emits `source` into `project`.
void EmitDesign(
    const std::filesystem::path& lyra_exe, std::string_view source,
    const std::filesystem::path& project) {
  std::filesystem::create_directories(project);
  const auto design = project.parent_path() / "design.sv";
  {
    std::ofstream out(design);
    out << source;
  }
  const std::vector<std::string> emit_args = {
      "emit", "cpp", "--top", "Test", "-o", project.string(), design.string()};
  auto emit_or = lyra::driver::RunProcessCaptured(lyra_exe, emit_args);
  ASSERT_TRUE(emit_or) << emit_or.error().Text();
  ASSERT_EQ(emit_or->exit_code, 0) << emit_or->stderr_text;
}

// Compiles every unit of `project` with `compiler`, as an unoptimized build of
// the design does, and `added` beside its flags, and adds the object it leaves
// to `objects`.
void CompileUnits(
    const std::filesystem::path& compiler, const std::filesystem::path& project,
    std::span<const std::string_view> added,
    std::vector<std::filesystem::path>& objects) {
  std::vector<lyra::driver::ProcessRequest> compiles;
  for (const auto& entry : std::filesystem::directory_iterator(project)) {
    if (entry.path().extension() != ".cpp") {
      continue;
    }
    std::filesystem::path object = entry.path();
    object.replace_extension(".o");
    std::vector<std::string> args = lyra::driver::DesignCompileFlags(
        lyra::driver::OptimizationLevel::kO0, lyra::support::Sanitizer::kNone);
    args.insert(args.end(), added.begin(), added.end());
    args.insert(
        args.end(),
        {"-I", (project / "runtime" / "include").string(), "-I",
         project.string(), "-c", entry.path().string(), "-o", object.string()});
    compiles.push_back(
        lyra::driver::ProcessRequest{.exe = compiler, .args = std::move(args)});
    objects.push_back(std::move(object));
  }
  ASSERT_FALSE(objects.empty()) << "the design emitted no translation unit";
  auto compiled_or = lyra::driver::RunProcessesCaptured(compiles, 4);
  ASSERT_TRUE(compiled_or) << compiled_or.error().Text();
  for (const auto& compiled : *compiled_or) {
    ASSERT_EQ(compiled.exit_code, 0) << "an emitted unit did not compile:\n"
                                     << compiled.stderr_text;
  }
}

// The one of `names` a section holds, where a compile given a section per
// definition named the section after it, `.<kind>.<symbol>` with a kind that
// may itself hold dots, `.data.rel.ro`; nothing for a section holding none of
// them, such as the pool of a file's string literals.
auto SymbolOfSection(
    std::string_view section, const std::set<std::string>& names)
    -> std::optional<std::string> {
  for (std::size_t at = section.find('.'); at != std::string_view::npos;
       at = section.find('.', at + 1)) {
    const std::string candidate(section.substr(at + 1));
    if (names.contains(candidate)) {
      return candidate;
    }
  }
  return std::nullopt;
}

// What a relocation refers to, as `llvm-objdump -r` prints it -- a symbol, or
// the section holding one, with an addend after it -- where it is one of
// `names`.
auto RelocationTarget(
    std::string_view value, const std::set<std::string>& names)
    -> std::optional<std::string> {
  const std::size_t addend = value.find_first_of("+-", 1);
  const std::string target(
      addend == std::string_view::npos ? value : value.substr(0, addend));
  if (names.contains(target)) {
    return target;
  }
  return SymbolOfSection(target, names);
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
  const auto clangxx = lyra::test::ResolveClangxx();
  ASSERT_FALSE(clangxx.empty());
  const auto nm = lyra::test::ResolveLlvmNm();
  ASSERT_FALSE(nm.empty());
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto dir_or = conformance::framework::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();

  const auto project = *dir_or / "project";
  ASSERT_NO_FATAL_FAILURE(EmitDesign(lyra_exe, kDesignSource, project));
  std::vector<std::filesystem::path> objects;
  ASSERT_NO_FATAL_FAILURE(CompileUnits(clangxx, project, {}, objects));

  std::vector<std::string> unit_namespaces;
  for (const auto& object : objects) {
    unit_namespaces.push_back(object.stem().string());
  }

  std::string carried;
  for (const auto& object : objects) {
    const std::vector<std::string> mangled_args = {
        "--defined-only", "--format=just-symbols", object.string()};
    const std::vector<std::string> demangled_args = {
        "--defined-only", "--format=just-symbols", "--demangle",
        object.string()};
    auto mangled_or = lyra::driver::RunProcessCaptured(nm, mangled_args);
    auto demangled_or = lyra::driver::RunProcessCaptured(nm, demangled_args);
    ASSERT_TRUE(mangled_or) << mangled_or.error().Text();
    ASSERT_TRUE(demangled_or) << demangled_or.error().Text();
    const std::vector<std::string> mangled = LinesOf(mangled_or->stdout_text);
    const std::vector<std::string> demangled =
        LinesOf(demangled_or->stdout_text);
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

// The library is compiled by one compiler and a unit of a design by whichever
// the user has, so what a unit calls in it is a name the two have to spell
// alike. Nothing shows it where they do not: a unit compiled by the library's
// own compiler finds every name, and the headers read the same to both. The
// names a unit asks for are read off its object, under the compiler the
// library was not compiled by, without building or linking the program.
TEST(RuntimeSurface, AUnitOfAnotherCompilerFindsWhatItCallsInTheLibrary) {
  const auto gcc = lyra::test::ResolveHostGcc();
  ASSERT_FALSE(gcc.empty());
  const auto nm = lyra::test::ResolveLlvmNm();
  ASSERT_FALSE(nm.empty());
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto dir_or = conformance::framework::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();
  auto runtime_or = lyra::driver::ResolveRuntimeLocation(lyra_exe.string());
  ASSERT_TRUE(runtime_or) << runtime_or.error().primary.message;

  const auto symbols = [&](std::string_view which,
                           const std::filesystem::path& file) {
    const std::vector<std::string> args = {
        std::string(which), "--format=just-symbols", file.string()};
    auto listed_or = lyra::driver::RunProcessCaptured(nm, args);
    if (!listed_or) {
      ADD_FAILURE() << listed_or.error().Text();
      return std::vector<std::string>{};
    }
    EXPECT_EQ(listed_or->exit_code, 0) << listed_or->stderr_text;
    return LinesOf(listed_or->stdout_text);
  };

  const std::vector<std::string> library =
      symbols("--defined-only", runtime_or->lib);
  ASSERT_FALSE(library.empty()) << "the library was listed as defining nothing";

  std::string missing;
  for (const auto [at, source] : std::views::enumerate(
           std::array{kDesignSource, kTakeoverDesignSource})) {
    const auto project = *dir_or / std::format("design{}", at) / "project";
    ASSERT_NO_FATAL_FAILURE(EmitDesign(lyra_exe, source, project));
    std::vector<std::filesystem::path> objects;
    ASSERT_NO_FATAL_FAILURE(CompileUnits(gcc, project, {}, objects));

    // Each design is a program of its own, so what one defines answers nothing
    // another asks.
    std::set<std::string> defined(library.begin(), library.end());
    for (const auto& object : objects) {
      std::ranges::copy(
          symbols("--defined-only", object),
          std::inserter(defined, defined.end()));
    }

    std::size_t asked = 0;
    for (const auto& object : objects) {
      for (const std::string& name : symbols("--undefined-only", object)) {
        if (!name.contains("4lyra")) {
          continue;
        }
        ++asked;
        if (!defined.contains(name)) {
          missing +=
              std::format("  {}: {}\n", object.filename().string(), name);
        }
      }
    }
    EXPECT_GT(asked, 0U) << "no unit was listed as asking the library for "
                            "anything, so this measured nothing";
  }

  EXPECT_TRUE(missing.empty()) << std::format(
      "units GCC compiled ask for names the library does not define:\n{}A "
      "unit calls a member of a family stated as already compiled by the name "
      "its own compiler gives it, and a constraint on the member is part of "
      "that name to some compilers and not to others.",
      missing);
}

// A unit compiled to two bodies is two translation units sharing the unit's
// names, and a definition of vague linkage -- one every file stating it
// defines and the link keeps one of -- stands in for the others in each. So it
// has to mean the same in both, and one referring to a definition only its own
// file has cannot: the other file's copy refers to the other file's, and which
// copy runs is the linker's choice. Nothing reports it, since the program links
// and runs whichever was kept, so this reads it off each object: what every
// section holding a definition of vague linkage refers to, against which
// definitions the object keeps to itself.
TEST(RuntimeSurface, ADefinitionAnotherBodyMayStandInForNamesNothingOfItsOwn) {
  const auto clangxx = lyra::test::ResolveClangxx();
  ASSERT_FALSE(clangxx.empty());
  const auto nm = lyra::test::ResolveLlvmNm();
  ASSERT_FALSE(nm.empty());
  const auto objdump = lyra::test::ResolveLlvmObjdump();
  ASSERT_FALSE(objdump.empty());
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto dir_or = conformance::framework::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();

  const auto project = *dir_or / "project";
  ASSERT_NO_FATAL_FAILURE(
      EmitDesign(lyra_exe, kTwoBodiesDesignSource, project));
  // A section per definition, so each relocation is read against the one
  // definition whose text carries it.
  constexpr std::array<std::string_view, 2> kSectionPerDefinition = {
      "-ffunction-sections", "-fdata-sections"};
  std::vector<std::filesystem::path> objects;
  ASSERT_NO_FATAL_FAILURE(
      CompileUnits(clangxx, project, kSectionPerDefinition, objects));

  std::string misread;
  std::size_t vague_read = 0;
  for (const auto& object : objects) {
    const std::vector<std::string> listed_args = {
        "--defined-only", "--format=posix", object.string()};
    auto listed_or = lyra::driver::RunProcessCaptured(nm, listed_args);
    ASSERT_TRUE(listed_or) << listed_or.error().Text();
    ASSERT_EQ(listed_or->exit_code, 0) << listed_or->stderr_text;
    // `name type value size`: a lowercase type is a definition the object
    // keeps to itself, and `V`, `W` and `u` are the vague ones -- a weak
    // function C code calls by its own name among them. Only an entity of the
    // program is read: an assembler's local label, a string literal's above
    // all, has no identity a program may rely on (C++ [lex.string]).
    std::set<std::string> own;
    std::set<std::string> vague;
    for (const std::string& line : LinesOf(listed_or->stdout_text)) {
      const std::size_t space = line.find(' ');
      if (space == std::string::npos || space + 1 >= line.size()) {
        continue;
      }
      const std::string name = line.substr(0, space);
      const char type = line[space + 1];
      if (name.starts_with('.')) {
        continue;
      }
      if (type == 'V' || type == 'W' || type == 'u') {
        vague.insert(name);
      } else if (type >= 'a' && type <= 'z' && type != 'v' && type != 'w') {
        own.insert(name);
      }
    }

    const std::vector<std::string> dumped_args = {"-r", object.string()};
    auto dumped_or = lyra::driver::RunProcessCaptured(objdump, dumped_args);
    ASSERT_TRUE(dumped_or) << dumped_or.error().Text();
    ASSERT_EQ(dumped_or->exit_code, 0) << dumped_or->stderr_text;
    // `RELOCATION RECORDS FOR [section]:`, then `offset type value` per
    // relocation the section carries.
    std::optional<std::string> holder;
    for (const std::string& line : LinesOf(dumped_or->stdout_text)) {
      constexpr std::string_view kRecords = "RELOCATION RECORDS FOR [";
      if (line.starts_with(kRecords)) {
        const std::string section =
            line.substr(kRecords.size(), line.find(']') - kRecords.size());
        holder = SymbolOfSection(section, vague);
        if (holder.has_value()) {
          ++vague_read;
        }
        continue;
      }
      if (!holder.has_value()) {
        continue;
      }
      const std::size_t value_at = line.find_last_of(' ');
      if (value_at == std::string::npos) {
        continue;
      }
      // An entity local to the holder's own body -- a lambda it writes -- is
      // part of its definition, so whichever copy is kept reaches its own.
      const std::optional<std::string> target =
          RelocationTarget(line.substr(value_at + 1), own);
      if (!target.has_value()) {
        continue;
      }
      const bool of_the_holder =
          holder->starts_with("_Z") &&
          target->starts_with(std::format("_ZZ{}", holder->substr(2)));
      if (!of_the_holder) {
        misread += std::format(
            "  {}: {} refers to {}\n", object.filename().string(), *holder,
            *target);
      }
    }
  }

  EXPECT_GT(vague_read, 0U) << "no object was read as holding a definition of "
                               "vague linkage, so this measured nothing";
  EXPECT_TRUE(misread.empty()) << std::format(
      "definitions every body states alike refer to what one body keeps to "
      "itself:\n{}What only a body names is written in the namespace with no "
      "name, and a definition every body states alike writes a constant it "
      "names in place.",
      misread);
}

// The runtime writes its descriptors itself and names no standard stream of
// the C++ library, which is what lets an ending hand the design's output over
// from a signal handler, and what keeps a program it links from needing the
// library's stream initializer.
TEST(RuntimeSurface, TheLibraryNamesNoStandardStream) {
  const auto nm = lyra::test::ResolveLlvmNm();
  ASSERT_FALSE(nm.empty());
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto runtime_or = lyra::driver::ResolveRuntimeLocation(lyra_exe.string());
  ASSERT_TRUE(runtime_or) << runtime_or.error().primary.message;

  const std::vector<std::string> args = {
      "--undefined-only", "--format=just-symbols", runtime_or->lib.string()};
  auto listed_or = lyra::driver::RunProcessCaptured(nm, args);
  ASSERT_TRUE(listed_or) << listed_or.error().Text();
  ASSERT_EQ(listed_or->exit_code, 0) << listed_or->stderr_text;
  const std::vector<std::string> asked = LinesOf(listed_or->stdout_text);

  EXPECT_FALSE(std::ranges::contains(asked, "_ZSt4cout"))
      << "the library writes standard output through std::cout";
  EXPECT_FALSE(std::ranges::contains(asked, "_ZSt4cerr"))
      << "the library writes the error stream through std::cerr";
  EXPECT_FALSE(std::ranges::contains(asked, "_ZSt3cin"))
      << "the library reads standard input through std::cin";
}
