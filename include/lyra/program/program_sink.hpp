#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/artifact_store.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::program {

// How this build turns a unit's module into an object: how hard to compile
// it, which build of the code generator does, where objects are kept for the
// next build and whether one kept may be taken, and where this build's objects
// are written.
struct ObjectBuild {
  driver::Optimization optimization;
  driver::ContentName code_generator;
  std::optional<std::filesystem::path> store;
  bool reuse_kept;
  std::filesystem::path object_dir;
};

// An object file the program links, and the name it is kept under.
struct ObjectFile {
  std::filesystem::path path;
  driver::ContentName name;
};

// One unit's part of the program: its object file, and the text of what the
// unit states of the foreign name space where it states anything.
struct BuiltUnit {
  ObjectFile object;
  std::optional<std::string> dpi_fragment;
};

// Takes one unit to its object, while the unit and every form it passed
// through are released. It reads the unit alone and writes an object named for
// its content, so several units may be built at once.
//
// The object is named by the module it is compiled from, the code generator
// and the level, and a kept one of that name is taken instead of compiling.
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
  std::vector<ObjectFile> objects;
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
