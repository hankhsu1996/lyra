#pragma once

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::lowering::ast_to_hir {

// Driver-supplied facts threaded into AST-to-HIR lowering. `Compilation&` is
// the slang elaboration root; `SourceMapper&` translates slang source
// locations. The assertion policy is what decides whether an assertion
// construct is elided rather than refused.
class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      support::AssertionPolicy assertion_policy)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        assertion_policy_(assertion_policy) {
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *compilation_;
  }
  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }
  [[nodiscard]] auto AssertionPolicy() const -> support::AssertionPolicy {
    return assertion_policy_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  support::AssertionPolicy assertion_policy_;
};

// The design once every unit has declared itself and before any unit's bodies
// are lowered: every namespace the design declares, then every distinct
// design-element body reachable from the tops, each tagged with whether its
// instances exist as objects, together with what each published. A unit reads
// only its own scope, the frontend, and what the other units published -- never
// their bodies -- so its bodies lower into a self-contained unit with no
// cross-unit HIR references.
//
// What each unit holds between the two steps is its declarations, so what is
// resident until a unit is lowered is what it declared, and its bodies exist
// only while it is being lowered and handed on. The elaborated AST they are
// lowered from is held here as well, since the units are its only readers.
//
// The frontend is read by one unit at a time. It elaborates what a reader
// first touches, without synchronizing that, and a unit's lowering reaches
// past its own body -- a name walks the elaborated hierarchy to whatever
// instance it lands on -- so what the units read cannot be elaborated in
// advance short of elaborating every instance, which costs a design with
// repeated instances several times the frontend's memory. A frontend that
// synchronized its own first-read computation could be read by every unit at
// once. What follows a unit's HIR reads no frontend, so it runs as wide as its
// caller allows.
class DeclaredDesign {
 public:
  // Declares every unit of `front_end`. A unit whose declarations fail is
  // reported and the rest declare anyway, so one run accounts for every unit;
  // nothing comes back after any such failure, because a body resolves names
  // against what the units published and would fail for want of a promise
  // nobody made, burying the account this step exists to give.
  static auto Declare(
      std::unique_ptr<slang::ast::Compilation> front_end,
      const frontend::SlangSourceMapper& source_mapper,
      support::AssertionPolicy assertion_policy, diag::DiagnosticSink& sink)
      -> std::optional<DeclaredDesign>;

  DeclaredDesign(DeclaredDesign&&) noexcept;
  auto operator=(DeclaredDesign&&) noexcept -> DeclaredDesign&;
  DeclaredDesign(const DeclaredDesign&) = delete;
  auto operator=(const DeclaredDesign&) -> DeclaredDesign& = delete;
  ~DeclaredDesign();

  [[nodiscard]] auto UnitCount() const -> std::size_t;

  // Lowers the bodies of the unit at `index`, once, and releases everything
  // it held for them. Calls for different indices may come from several
  // threads; they read the frontend one at a time.
  auto LowerUnit(std::size_t index) -> diag::Result<hir::CompilationUnit>;

  // What the design's units published, which is read by every unit's bodies
  // and by whatever composes the design from its units.
  [[nodiscard]] auto Signatures() const -> const hir::UnitSignatures&;

 private:
  struct Units;
  explicit DeclaredDesign(std::unique_ptr<Units> units);

  std::unique_ptr<Units> units_;
};

// A top-level block is an auto-promoted, uninstantiated module, named twice
// because the two names answer different questions and coincide only when the
// module carries no parameters.
struct TopLevelUnit {
  // What the design's hierarchy shows for this top. Nothing instantiates a
  // top, so it stands under its own module identifier (LRM 23.3), and that is
  // the name `%m` prints and an upward hierarchical name matches.
  std::string instance_name;
  // The compiled unit it is an instance of. One module compiles to one unit
  // per distinct parameterization, so this is the artifact's name.
  std::string unit_name;
};

// The design's tops, a subset of the compiled units: a unit reached only
// through instantiation is compiled but is not a top.
//
// A top is where the design begins, so nothing instantiates it and its ports
// are connected to nothing. Two kinds of port may not be left unconnected -- an
// interface port (LRM 23.3.3.4) and a `ref` port (LRM 23.3.3.2) -- so a module
// declaring either is a design element and not a design.
auto TopLevelUnits(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<TopLevelUnit>>;

}  // namespace lyra::lowering::ast_to_hir
