#pragma once

#include <functional>
#include <map>
#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::hir {

// The units of a design by the name each is compiled under, which is ordered
// so that what is built from it comes out the same on every run.
template <typename T>
using ByUnit = std::map<std::string, T, std::less<>>;

// What each unit of the design publishes. The name a unit is compiled under is
// left out, since it says which application of a definition asked, and every
// other unit a signature names is written as what that unit publishes -- so
// two units publish alike wherever what they declare and what they name does.
[[nodiscard]] auto PublishedFingerprintsOf(const UnitSignatures& signatures)
    -> ByUnit<stable_hash::Fingerprint>;

// What `unit` lowered to, as far as a program compiled from it can tell.
// Left out are the name the unit is compiled under and every name the program
// never reads, which reaches the spelling of emitted text at most; a place in
// the source is what a report the program makes prints of it. Another unit
// whose declarations it reads is written as what that unit publishes, from
// `published`, and one whose code it runs is written as the name a call into
// it names, since the call holds that name and nothing of the callee's body.
[[nodiscard]] auto BodyFingerprintOf(
    const CompilationUnit& unit,
    const ByUnit<stable_hash::Fingerprint>& published,
    const diag::SourceManager& sources) -> stable_hash::Fingerprint;

}  // namespace lyra::hir
