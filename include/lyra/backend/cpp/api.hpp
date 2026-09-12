#pragma once

#include <optional>
#include <span>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/compiler/unit_program_record.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Why this backend cannot realize the unit the record describes, or nothing
// where it can. A unit reaching a property or a behavior through a reference
// with no class view is refused whole rather than emitted with a gap: this
// backend realizes an object as a target-language class and reaches a member by
// writing its name, so a position settled while the design elaborates is one it
// has no spelling for, and what it cannot realize it declines rather than
// falling back to another form.
//
// It answers from what a unit published, so every unit can be asked before any
// of them is rendered -- which is what lets a run that meets a gap name every
// one of them.
auto RefusalFor(const compiler::UnitProgramRecord& record)
    -> std::optional<diag::Diagnostic>;

// The translation unit one compiled unit becomes. The design root is a unit
// like any other here.
auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppArtifact;

// The program entry, which constructs the design root. Besides the root's own
// header it includes the header of every unit that defines a symbol only
// foreign C refers to (LRM 35.7), because no SV referrer pulls one in; which
// units those are is what each record states.
auto EmitCppHostMain(
    std::span<const compiler::UnitProgramRecord> records,
    const mir::CompilationUnit& root) -> CppArtifact;

}  // namespace lyra::backend::cpp
