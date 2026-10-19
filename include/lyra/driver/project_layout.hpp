#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::driver {

// How hard the design's own code is optimized, whichever backend compiles it:
// the levels clang's `-O` names, each the pipeline LLVM runs at that level. A
// lower level pays less compile on every edit and a higher one less run time.
// Without link-time optimization the runtime is the prebuilt, optimized library
// at every level; with it, the runtime is optimized at this level too.
enum class OptimizationLevel : std::uint8_t { kO0, kO1, kO2, kO3 };

// Whether the design is optimized together with the runtime it calls, at the
// link, as clang names its kinds of link-time optimization. It is a choice of
// its own, apart from the level: what it buys depends on the design, and what
// it costs is a whole optimization again at the link.
enum class LinkTimeOptimization : std::uint8_t { kNone, kThin };

// A precompiled header and the translation unit including it must be compiled
// alike, so every level names a flag: a compiler's default is not something
// two command lines can be checked against.
[[nodiscard]] constexpr auto OptimizationFlag(OptimizationLevel optimization)
    -> std::string_view {
  switch (optimization) {
    case OptimizationLevel::kO0:
      return "-O0";
    case OptimizationLevel::kO1:
      return "-O1";
    case OptimizationLevel::kO2:
      return "-O2";
    case OptimizationLevel::kO3:
      return "-O3";
  }
  std::unreachable();
}

// Whether to run the emitted C++ through a formatter before writing it. Named
// rather than a bare bool so a call site says which it means.
enum class SourceFormatting : std::uint8_t { kOff, kOn };

// Relative layout and build recipe of a self-contained emitted C++ project.
// Shared by the `build.sh` the project ships and the build `build` and `run`
// drive themselves, so the two never drift.
inline constexpr std::string_view kRuntimeIncludeDir = "runtime/include";
inline constexpr std::string_view kRuntimeLibDir = "runtime/lib";
inline constexpr std::string_view kRuntimeLibFile = "libcpp_runtime.a";
inline constexpr std::string_view kRuntimeCacheDir = "runtime/cache";
// Where a project optimized at the link keeps the runtime's own sources, which
// its compiler builds beside the design so that it optimizes the two as one
// program, with a list of those a build compiles and the include directories
// and macros every such compile is told.
inline constexpr std::string_view kRuntimeSourceDir = "runtime/src";
inline constexpr std::string_view kRuntimeSourceList = "sources.txt";
inline constexpr std::string_view kRuntimeIncludeDirList = "include_dirs.txt";
inline constexpr std::string_view kRuntimeDefineList = "defines.txt";
// Where a compiled object lands. Every source is compiled on its own so that a
// build may work on several at once, which needs somewhere for a finished one
// to wait until the link reads them all. A foreign source's object sits under
// this directory too, in a subdirectory of its own, because the design's own
// names and the user's are two name spaces and nothing forbids a collision
// between them.
inline constexpr std::string_view kObjectDir = "obj";
inline constexpr std::string_view kProgramName = "program";
inline constexpr std::string_view kCxxStandardFlag = "-std=c++23";
// The design's code is read without type-based aliasing. A member of an
// untagged union is written through one type and read through another (LRM
// 7.3.2), which C++ gives a meaning only where the access names the union, and
// a member is reached through a reference to it; the execution backend states
// no aliasing by type either, so both read the program alike.
inline constexpr std::string_view kNoTypeBasedAliasingFlag =
    "-fno-strict-aliasing";
// Whose layout a directory a link keeps its per-module compiles in has: LLVM's
// ThinLTO cache, which Lyra's own link and a host clang's linker both write
// under keys carrying the LLVM release, or GCC's incremental one.
enum class LinkTimeCache : std::uint8_t { kLlvm, kGcc };

// What a link is told, followed directly by a directory, to keep what it
// compiled per module there, so that a link after an edit compiles only the
// modules whose code or imports changed.
struct LinkTimeCacheFlag {
  std::string_view spelling;
  LinkTimeCache layout;
};

// One way a host compiler and the linker it drives can be told to optimize a
// program whole at the link: what every compile is told, which the link is
// told as well, and how the link keeps its cache where it can.
struct LinkTimeForm {
  std::string_view compile;
  std::optional<LinkTimeCacheFlag> cache;
};

// Every such way, in the order a build tries them; the first a compiler and
// its linker accept is the one taken. The compiler's driver names no cache, so
// each linker's own spelling is a form of its own: the gold plugin's, which
// bfd and mold read too, then lld's, which refuses the plugin's. GCC refuses
// ThinLTO's spelling, so one list serves either compiler without asking which
// it is. Both spell a link that runs its partitions in parallel.
inline constexpr std::array<LinkTimeForm, 5> kLinkTimeForms{{
    {.compile = "-flto=thin",
     .cache =
         LinkTimeCacheFlag{
             .spelling = "-Wl,-plugin-opt,cache-dir=",
             .layout = LinkTimeCache::kLlvm}},
    {.compile = "-flto=thin",
     .cache =
         LinkTimeCacheFlag{
             .spelling = "-Wl,--thinlto-cache-dir=",
             .layout = LinkTimeCache::kLlvm}},
    {.compile = "-flto=thin", .cache = std::nullopt},
    {.compile = "-flto=auto",
     .cache =
         LinkTimeCacheFlag{
             .spelling = "-flto-incremental=", .layout = LinkTimeCache::kGcc}},
    {.compile = "-flto=auto", .cache = std::nullopt},
}};

// The program a build compiles and links with each form to learn whether the
// compiler and its linker accept it. Its object is bitcode alone, so a link
// that cannot optimize at the link cannot link it at all. It holds no single
// quote, since the project's recipe writes it out quoted so.
inline constexpr std::string_view kLinkTimeProbe = "int main() { return 0; }\n";
// How every compile of the design's own code reads it.
inline constexpr std::array<std::string_view, 2> kDesignCompileFlags{
    kCxxStandardFlag, kNoTypeBasedAliasingFlag};

// What every compile of the design's own code is told, at `optimization`.
[[nodiscard]] inline auto DesignCompileFlags(OptimizationLevel optimization)
    -> std::vector<std::string> {
  std::vector<std::string> flags(
      kDesignCompileFlags.begin(), kDesignCompileFlags.end());
  flags.emplace_back(OptimizationFlag(optimization));
  return flags;
}
// A precompiled header stays usable only while the headers it was built from
// are unchanged, and the compiler decides that by timestamp unless asked to
// read the bytes. Not every one of those headers is ours to leave alone -- a
// checkout restores one byte for byte under a new timestamp -- so the question
// has to be asked of the content. The answer is recorded when the header is
// prepared and consulted when it is loaded, so both command lines carry this.
inline constexpr std::string_view kPchContentValidationFlag =
    "-fpch-validate-input-files-content";
// Including a header does not only parse it: a template the header uses is
// instantiated again in every translation unit that reaches it, and for these
// headers that work dwarfs the parse a prepared header already saves. So the
// instantiations go into the prepared header too, and a unit including it does
// none of them. Only the command preparing one carries this -- it decides what
// the artifact holds, not how a unit reads it -- and it asks that every header
// named compile on its own, which is what an umbrella header is.
inline constexpr std::string_view kPchInstantiateTemplatesFlag =
    "-fpch-instantiate-templates";
// The DPI-C boundary surface a user's foreign sources compile against (LRM 35):
// the generated prototypes plus the standard header they are spelled in. Both
// sit at the project root so one include path reaches them.
inline constexpr std::string_view kDpiAbiHeader = "dpi.h";
inline constexpr std::string_view kSvdpiHeader = "svdpi.h";
// Where a portable project keeps its copies of the user's DPI-C sources, so the
// directory builds on another machine without reaching back to their originals.
inline constexpr std::string_view kDpiSourceDir = "dpi";

}  // namespace lyra::driver
