#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;

auto LowerType(const slang::ast::Type& type, SourceSpan source, Context* ctx)
    -> TypeId;

// Canonical key element type for foreach-over-associative-array.
// Wildcard associative arrays (key_type is invalid) use int.
// Used by both Phase 0 seeding and Phase 1 desugaring to ensure one
// canonical semantic rule.
auto ForeachSnapshotKeyType(TypeId aa_type, Context* ctx) -> TypeId;

}  // namespace lyra::lowering::ast_to_hir
