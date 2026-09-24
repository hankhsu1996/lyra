#pragma once

#include <cstdint>
#include <filesystem>
#include <optional>

#include "lyra/driver/project_layout.hpp"

namespace lyra::driver::pch {

// Whether a build offers the compiler a prepared header at all, as the caller
// asked on the command line.
enum class Policy : std::uint8_t { kAttempt, kSkip };

// Return the PCH path to pass via `-include-pch`, building it on demand and
// keeping it in `store`, beside the programs a build keeps. The name it is kept
// under is computed from everything that goes into it (the compiler, where the
// runtime headers are and every byte of them, and how the header is prepared),
// so finding one means its content matches and no staleness check is needed at
// lookup time.
//
// What comes back is an offer and never a requirement. Whether the compiler
// accepts it is the compiler's to answer, and it answers by failing, so a
// caller passes it only where it can compile again without it -- nothing a
// build produces, and no reason a build fails, may depend on what this
// returned. `optimization` must be the one the including translation unit is
// compiled at: a header prepared under different options is refused. Returns
// nullopt when the policy skips it, the compiler is not clang, or there is no
// store to keep one in.
auto EnsureCached(
    const std::filesystem::path& cxx, const std::filesystem::path& include_root,
    Policy policy, const std::optional<std::filesystem::path>& store,
    Optimization optimization) -> std::optional<std::filesystem::path>;

// Drop one precompiled header, so that whoever asks next builds it again. For
// the caller that has just watched the compiler refuse this one, which is the
// only evidence there is that it had gone stale.
auto Discard(const std::filesystem::path& pch_path) -> void;

}  // namespace lyra::driver::pch
