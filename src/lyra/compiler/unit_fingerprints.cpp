#include "lyra/compiler/unit_fingerprints.hpp"

#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/fingerprint.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::compiler {

UnitFingerprints::UnitFingerprints(
    const hir::UnitSignatures& signatures, const diag::SourceManager& sources)
    : sources_(&sources),
      published_(
          support::RecordsStatistics()
              ? hir::PublishedFingerprintsOf(signatures)
              : hir::ByUnit<stable_hash::Fingerprint>{}) {
  for (const auto& [name, fingerprint] : published_) {
    support::RecordFingerprint(
        name, "signature", stable_hash::HexOf(fingerprint));
  }
}

void UnitFingerprints::Lowered(const hir::CompilationUnit& unit) const {
  if (!support::RecordsStatistics()) {
    return;
  }
  support::RecordFingerprint(
      unit.name, "body",
      stable_hash::HexOf(hir::BodyFingerprintOf(unit, published_, *sources_)));
}

}  // namespace lyra::compiler
