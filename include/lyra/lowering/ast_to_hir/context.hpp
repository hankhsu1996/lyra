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

/// Canonical types used across AST→HIR lowering.
/// Initialized once per compilation via InternBuiltinTypes().
struct BuiltinTypes {
  /// Internal simulation ticks (uint64, two-state).
  /// Used by $time lowering for raw tick queries.
  TypeId tick_type;
};

/// Initialize canonical builtin types in the given arena.
/// Called once per compilation from LowerDesign.
inline auto InternBuiltinTypes(TypeArena& arena) -> BuiltinTypes {
  return BuiltinTypes{
      .tick_type = arena.Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 64, .is_signed = false, .is_four_state = false}),
  };
}

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

  // Canonical builtin types. Initialized once in LowerDesign.
  BuiltinTypes builtin_types{};

  [[nodiscard]] auto GetTickType() const -> TypeId {
    return builtin_types.tick_type;
  }

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

  template <typename... Args>
  void UnsupportedFmt(
      SourceSpan span, UnsupportedCategory cat, std::format_string<Args...> fmt,
      Args&&... args) {
    sink->Unsupported(span, std::format(fmt, std::forward<Args>(args)...), cat);
  }
};

}  // namespace lyra::lowering::ast_to_hir
