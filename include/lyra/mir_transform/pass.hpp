#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/diag/source_manager.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"

namespace lyra::mir_transform {

// What a rewrite of one body reads besides the body: the unit holding it, and
// the front end's answers about the source the body was lowered from, as rustc
// hands every MIR pass the `TyCtxt` holding the session's source map.
struct PassContext {
  const mir::CompilationUnit* unit;
  const diag::SourceManager* sources;
};

// One rewrite of MIR into MIR, named as its function is, as rustc names a pass
// by its type. A pass rewrites one body at a time, as rustc's do, so it runs
// the same on the bodies a unit holds and on one the unit does not, answering
// nothing where it leaves the body as it is. `run_on_unit` is what it does
// first to what is no body, and `run_on_body` is null for a rewrite of what no
// generated body can be, a process (LRM 9.2).
struct Pass {
  std::string_view name;
  auto (*run_on_body)(
      const PassContext& context, const mir::CodeSource& at,
      const mir::CallableCode& code)
      -> std::optional<mir::CallableCode> = nullptr;
  auto (*run_on_unit)(mir::CompilationUnit unit)
      -> mir::CompilationUnit = nullptr;
};

// The passes that bring a unit from the phase it is in into `into`, which a
// correct program needs, as Swift's mandatory passes are the ones that commit a
// SIL stage: what a later stage reads is the form they leave, so no build skips
// them. They run in order, and the unit is in `into` once the last has.
struct Conversion {
  mir::Phase into;
  std::span<const Pass> passes;
};

// An optional rewrite a build asks to have run or skipped, by its name.
struct PassOverride {
  std::string name;
  bool runs;
};

// When the unit is checked, as LLVM's `opt` names the same choice: once, as
// it leaves the last rewrite; as lowered, after every optional rewrite and on
// reaching each phase; or not at all, which only a command showing the unit to
// a person asks for.
enum class VerifierKind : std::uint8_t { kNone, kOutput, kEachStep };

// What a build asks of the rewrites beyond running them as the pipeline states:
// optional ones run or skipped, the last word on each winning, and when the
// unit is checked.
struct PassOptions {
  std::vector<PassOverride> overrides;
  VerifierKind verifier = VerifierKind::kOutput;
};

}  // namespace lyra::mir_transform
