#pragma once

#include <slang/text/SourceLocation.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context {
  DiagnosticSink* sink;
  hir::Arena* hir_arena;
  TypeArena* type_arena;
  ConstantArena* constant_arena;
  SymbolTable* symbol_table;
  ScopeTable* scope_table;
  SourceMapper* source_mapper;

  [[nodiscard]] auto SpanOf(slang::SourceRange range) const -> SourceSpan {
    return source_mapper->SpanOf(range);
  }
};

}  // namespace lyra::lowering::ast_to_hir
