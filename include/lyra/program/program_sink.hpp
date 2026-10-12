#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::program {

// How this build turns a unit's module into an object: how hard to compile
// it, which build of the code generator does, and the actions the compile is
// performed or found kept through.
struct ObjectBuild {
  driver::Optimization optimization;
  std::filesystem::path code_generator;
  driver::ActionCache* actions;
};

// One unit's part of the program: its object file, and the text of what the
// unit states of the foreign name space where it states anything.
struct BuiltUnit {
  driver::ActionResult object;
  std::optional<std::string> dpi_fragment;
};

// Takes one unit to its object, while the unit and every form it passed
// through are released. It reads the unit alone, so several units may be
// built at once.
//
// Compiling is an action whose inputs are the module, the code generator and
// the level, and the object of one kept is taken instead of compiling.
// Before either, the module is checked against the runtime library this
// compiler ships, which is the library the program links: a module naming an
// entry that library does not publish is refused by name, so the absence is
// reported as an operation nobody wrote rather than as a link that cannot
// resolve a symbol.
auto BuildUnit(const mir::CompilationUnit& unit, const ObjectBuild& build)
    -> diag::Result<BuiltUnit>;

// What the program is linked from: one object per unit plus the design root's
// and the entry the program starts at, and what the units stated of the
// program's foreign name space (LRM 35), which the foreign sources are compiled
// against.
struct ProgramObjects {
  std::vector<driver::ActionResult> objects;
  std::vector<dpi::AbiFragment> dpi_fragments;
};

// Collects a design's program from its units in the order they are collected,
// which is the order the objects are linked in and named by. Finishing adds
// the design root's object and the entry's.
class ProgramSink {
 public:
  void Collect(BuiltUnit unit);

  auto Finish(
      const mir::CompilationUnit& root,
      const ObjectBuild& build) && -> diag::Result<ProgramObjects>;

 private:
  ProgramObjects program_;
};

}  // namespace lyra::program
