#pragma once

#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/fingerprint.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::compiler {

// Tells a run that records itself what each unit of the design is one with
// another unit by: what it publishes, told as the design is declared, and what
// it lowered to, told beside the other units' lowering while its HIR is in
// hand.
class UnitFingerprints {
 public:
  UnitFingerprints(
      const hir::UnitSignatures& signatures,
      const diag::SourceManager& sources);

  // From whichever thread lowered `unit`.
  void Lowered(const hir::CompilationUnit& unit) const;

 private:
  const diag::SourceManager* sources_;
  hir::ByUnit<stable_hash::Fingerprint> published_;
};

}  // namespace lyra::compiler
