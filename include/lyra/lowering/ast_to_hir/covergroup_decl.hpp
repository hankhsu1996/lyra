#pragma once

#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// The covergroup `covergroup` as the source declares it (LRM 19.3): its
// formals, those of an overriding `sample`, how it is sampled, its options,
// coverpoints and crosses, every expression it holds lowered into the
// declaration's own body. `declaring_frame` stands where the scope replicating
// the covergroup stands, so its expressions reach what encloses it as a body
// of that scope does.
auto LowerCovergroupDecl(
    UnitLowerer& unit_lowerer, const slang::ast::CovergroupType& covergroup,
    const WalkFrame& declaring_frame, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupDecl>;

// The property of a base class holding the instance a class's covergroup
// variable `prop` names, where it is the variable of a covergroup extending
// one a base class embeds (LRM 19.4.1), and none where `prop` holds its own.
// The derived covergroup's instance replaces the base's, and every class of
// the lineage names that one instance, so such a variable is no storage.
auto InheritedCovergroupProperty(const slang::ast::ClassPropertySymbol& prop)
    -> const slang::ast::ClassPropertySymbol*;

}  // namespace lyra::lowering::ast_to_hir
