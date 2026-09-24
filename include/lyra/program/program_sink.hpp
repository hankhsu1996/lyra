#pragma once

#include <filesystem>
#include <span>
#include <vector>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::program {

// What the program is composed from: one module per unit plus the design
// root's and the entry the program starts at, and what each unit stated of the
// program's foreign name space (LRM 35), which the foreign sources are compiled
// against.
struct EmittedProgram {
  std::vector<backend::llvm_backend::EmittedModule> modules;
  std::vector<dpi::AbiFragment> dpi_fragments;
};

// Collects a design's program one unit at a time: each unit is taken to a
// module as it arrives and the module is kept, while the unit and every form
// it passed through are not. Finishing adds the design root's module and the
// entry, and hands everything over to be compiled.
//
// Each module is checked, as it is emitted, against the runtime library this
// compiler ships, which is the library the program links. A module naming an
// entry that library does not publish is refused by name, so the absence is
// reported as an operation nobody wrote rather than as a link that cannot
// resolve a symbol.
class ProgramSink {
 public:
  auto Take(const mir::CompilationUnit& unit) -> diag::Result<void>;

  auto Finish(
      const mir::CompilationUnit& root) && -> diag::Result<EmittedProgram>;

 private:
  // Emits and keeps the unit's module, answering with the form it was emitted
  // from.
  auto Emit(const mir::CompilationUnit& unit)
      -> diag::Result<compiler::ExecutableUnit>;
  auto Keep(backend::llvm_backend::EmittedModule module) -> diag::Result<void>;

  std::vector<backend::llvm_backend::EmittedModule> modules_;
  std::vector<dpi::AbiFragment> dpi_fragments_;
};

// Writes each module's object into `object_dir` and links them, with the
// foreign objects, into the program at `program`. An object is named by its
// module's position, because a unit's own name is an identifier of the source
// and may not be a file name.
auto CompileProgram(
    std::vector<backend::llvm_backend::EmittedModule> modules,
    std::span<const std::filesystem::path> foreign_objects,
    const std::filesystem::path& object_dir,
    const std::filesystem::path& runtime_lib,
    const std::filesystem::path& program, const std::filesystem::path& cxx)
    -> diag::Result<void>;

}  // namespace lyra::program
