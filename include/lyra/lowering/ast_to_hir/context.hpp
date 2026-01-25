#pragma once

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>

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
  DiagnosticSink* sink = nullptr;
  hir::Arena* hir_arena = nullptr;
  TypeArena* type_arena = nullptr;
  ConstantArena* constant_arena = nullptr;
  SymbolTable* symbol_table = nullptr;
  ScopeTable* scope_table = nullptr;
  SourceMapper* source_mapper = nullptr;

  // Counter for generating unique synthetic variable names
  uint32_t temp_counter = 0;

  // Cached global precision (avoids repeated hierarchy walks).
  // Computed once per compilation, used by all ModuleLowerer instances.
  std::optional<int> cached_global_precision;

  [[nodiscard]] auto SpanOf(slang::SourceRange range) const -> SourceSpan {
    return source_mapper->SpanOf(range);
  }

  // Generate a unique synthetic variable name
  [[nodiscard]] auto MakeTempName(std::string_view prefix) -> std::string {
    return std::format("__lyra_{}_{}", prefix, temp_counter++);
  }

  template <typename... Args>
  void ErrorFmt(
      SourceSpan span, std::format_string<Args...> fmt, Args&&... args) {
    sink->Error(span, std::format(fmt, std::forward<Args>(args)...));
  }
};

}  // namespace lyra::lowering::ast_to_hir
