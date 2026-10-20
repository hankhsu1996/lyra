#pragma once

#include <array>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::driver::pch {

// Whether a build offers the compiler a prepared header at all, as the caller
// asked on the command line.
enum class Policy : std::uint8_t { kAttempt, kSkip };

// A prepared header stays usable only while the headers it was built from are
// unchanged, and clang decides that by timestamp unless asked to read the
// bytes. Not every one of those headers is ours to leave alone -- a checkout
// restores one byte for byte under a new timestamp -- so the question has to be
// asked of the content. The answer is recorded when the header is prepared and
// consulted when it is loaded, so both command lines carry this.
inline constexpr std::string_view kContentValidationFlag =
    "-fpch-validate-input-files-content";
// Including a header does not only parse it: a template the header uses is
// instantiated again in every translation unit that reaches it, and for these
// headers that work dwarfs the parse a prepared header already saves. So the
// instantiations go into the prepared header too, and a unit including it does
// none of them. Only the command preparing one carries this -- it decides what
// the artifact holds, not how a unit reads it -- and it asks that every header
// named compile on its own, which is what an umbrella header is.
inline constexpr std::string_view kInstantiateTemplatesFlag =
    "-fpch-instantiate-templates";
// GCC passes over a prepared header it cannot use and reads the header itself,
// saying so only as a warning, so a header that will never be taken is never
// found out and every build pays for it. Made an error, the refusal reaches the
// compile that runs again without one, and the header is dropped.
inline constexpr std::string_view kRefuseUnusableFlag = "-Werror=invalid-pch";

// Which command lines an optional flag goes on: the one preparing a header,
// every compile against it, or both.
enum class FlagReach : std::uint8_t {
  kPreparing,
  kCompiling,
  kPreparingAndCompiling
};

// A flag a prepared header is better for and does not need.
struct OptionalFlag {
  std::string_view flag;
  FlagReach reach;
};

// Every such flag. Each is a compiler's own and stands alone -- clang takes all
// three and GCC only the last -- so a build asks the compiler in hand about
// each one by itself and passes those it takes, without asking which compiler
// it is.
inline constexpr std::array<OptionalFlag, 3> kOptionalFlags{{
    {.flag = kInstantiateTemplatesFlag, .reach = FlagReach::kPreparing},
    {.flag = kContentValidationFlag,
     .reach = FlagReach::kPreparingAndCompiling},
    {.flag = kRefuseUnusableFlag, .reach = FlagReach::kCompiling},
}};

// What a compiler that prepares headers at all is told beyond how every unit
// is compiled: when preparing one, and when compiling against one.
struct PreparedHeaderFlags {
  std::vector<std::string> preparing;
  std::vector<std::string> compiling;
};

// A prelude a compile is offered: the prepared header as the store keeps it,
// and what a compile is told to be offered it.
struct Offer {
  ActionResult prepared;
  std::vector<std::string> flags;
};

// Whether `cxx` prepares a header, and which of `kOptionalFlags` it takes:
// an empty header is prepared plainly, then with each flag alone, and a
// command counts as accepted only where it succeeds and prints nothing, so a
// compiler that warns of a flag it ignores is not taken at its word. Nothing
// where the plain one fails. The probe is an action whose inputs are the
// compiler and the probe's command line, so it runs once per compiler.
auto AcceptedPreparedHeaderFlags(
    const std::filesystem::path& cxx, ActionCache& actions)
    -> std::optional<PreparedHeaderFlags>;

// Prepares the runtime's prelude, or finds it kept, and lays out in `dir` how
// a compile is offered it: `kPreludeOfferHeader`, which includes the prelude,
// and the prepared header beside it under that name with
// `kPreparedHeaderSuffix`, where GCC looks for one and clang does as well for
// GCC's sake. A compile told to include that header reads the prepared one
// instead, and a compiler that refuses it reads the header and so the prelude.
//
// The prepared header is the output of an action whose inputs are everything
// that goes into it: the compiler, where the runtime headers are,
// `prelude_inputs` -- every file the prelude reads, by its content -- and the
// command line that
// prepares it. Finding one kept therefore means it was made from exactly what
// is on the machine now, which GCC, unlike clang, never checks for itself.
//
// What comes back is an offer and never a requirement. Whether the compiler
// accepts it is the compiler's to answer, and it answers by failing, so a
// caller passes it only where it can compile again without it -- nothing a
// build produces, and no reason a build fails, may depend on what this
// returned. `optimization` must be the one the including translation unit is
// compiled at: a header prepared under different options is refused. Returns
// nullopt when the policy skips it, the compiler prepares no header, or nothing
// is kept, since a header prepared for one build costs more than it saves.
//
// A header the compiler refuses is evicted from the cache, so that whoever
// asks next prepares it again.
auto OfferPrelude(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    const stable_hash::Fingerprint& prelude_inputs, Policy policy,
    ActionCache& actions, OptimizationLevel optimization,
    const std::filesystem::path& dir) -> std::optional<Offer>;

}  // namespace lyra::driver::pch
