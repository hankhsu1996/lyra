#pragma once

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>

#include <slang/text/SourceLocation.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"

namespace lyra::lowering::ast_to_hir {

// AST->HIR compile-global builtin semantic type catalog.
// Populated once by Init() in Phase 0. Phase 1 body lowering reads these
// as stable handles through named accessors. Pre-init access throws.
class BuiltinTypeCatalog {
 public:
  void Init(TypeArena& arena) {
    tick_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 64, .is_signed = false, .is_four_state = false});
    int_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 32, .is_signed = true, .is_four_state = false});
    int4_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 32, .is_signed = true, .is_four_state = true});
    bit_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 1, .is_signed = false, .is_four_state = false});
    logic_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 1, .is_signed = false, .is_four_state = true});
    real_type_ = arena.Intern(TypeKind::kReal, std::monostate{});
    shortreal_type_ = arena.Intern(TypeKind::kShortReal, std::monostate{});
    uint32_type_ = arena.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 32, .is_signed = false, .is_four_state = false});
    string_type_ = arena.Intern(TypeKind::kString, std::monostate{});
    void_type_ = arena.Intern(TypeKind::kVoid, std::monostate{});
  }

  [[nodiscard]] auto TickType() const -> TypeId {
    return Checked(tick_type_);
  }
  [[nodiscard]] auto IntType() const -> TypeId {
    return Checked(int_type_);
  }
  [[nodiscard]] auto Int4Type() const -> TypeId {
    return Checked(int4_type_);
  }
  [[nodiscard]] auto BitType() const -> TypeId {
    return Checked(bit_type_);
  }
  [[nodiscard]] auto LogicType() const -> TypeId {
    return Checked(logic_type_);
  }
  [[nodiscard]] auto UInt32Type() const -> TypeId {
    return Checked(uint32_type_);
  }
  [[nodiscard]] auto RealType() const -> TypeId {
    return Checked(real_type_);
  }
  [[nodiscard]] auto ShortRealType() const -> TypeId {
    return Checked(shortreal_type_);
  }
  [[nodiscard]] auto StringType() const -> TypeId {
    return Checked(string_type_);
  }
  [[nodiscard]] auto VoidType() const -> TypeId {
    return Checked(void_type_);
  }

 private:
  [[nodiscard]] static auto Checked(TypeId id) -> TypeId {
    if (!id) {
      throw common::InternalError(
          "BuiltinTypeCatalog", "accessed before Init()");
    }
    return id;
  }

  TypeId tick_type_ = kInvalidTypeId;
  TypeId int_type_ = kInvalidTypeId;
  TypeId int4_type_ = kInvalidTypeId;
  TypeId bit_type_ = kInvalidTypeId;
  TypeId logic_type_ = kInvalidTypeId;
  TypeId uint32_type_ = kInvalidTypeId;
  TypeId real_type_ = kInvalidTypeId;
  TypeId shortreal_type_ = kInvalidTypeId;
  TypeId string_type_ = kInvalidTypeId;
  TypeId void_type_ = kInvalidTypeId;
};

// Lowering context for AST->HIR.
// All fields are non-owning references to shared compilation state.
//
// This struct is intentionally shallow-copyable: ForkForBodyLowering()
// copies all shared references and overrides hir_arena,
// active_constant_arena, and sink to point to body-local storage.
// All other fields (type_arena, symbol_table, etc.) remain shared.
struct Context {
  DiagnosticSink* sink = nullptr;
  hir::Arena* hir_arena = nullptr;
  TypeArena* type_arena = nullptr;
  // Active constant domain: design-global during Phase 0, overridden to
  // body-local constant arena during Phase 1 body lowering.
  ConstantArena* active_constant_arena = nullptr;
  SymbolTable* symbol_table = nullptr;
  ScopeTable* scope_table = nullptr;
  SourceMapper* source_mapper = nullptr;

  // Counter for generating unique synthetic variable names
  uint32_t temp_counter = 0;

  // Cached global precision (avoids repeated hierarchy walks).
  // Computed once per compilation, used by all ModuleLowerer instances.
  std::optional<int> cached_global_precision;

  // Compile-global builtin semantic types. Initialized once in LowerDesign.
  BuiltinTypeCatalog builtin_types;

  // Fork a body-lowering context that enters the body-local artifact
  // domain. HIR nodes and constants are directed into body-local storage;
  // diagnostics go to the body-local sink. Shared design-global state
  // (type_arena, symbol_table, etc.) is inherited by shallow copy.
  [[nodiscard]] auto ForkForBodyLowering(
      hir::Arena& body_arena, ConstantArena& body_constant_arena,
      DiagnosticSink& body_sink) const -> Context {
    Context body_ctx = *this;
    body_ctx.hir_arena = &body_arena;
    body_ctx.active_constant_arena = &body_constant_arena;
    body_ctx.sink = &body_sink;
    return body_ctx;
  }

  [[nodiscard]] auto TickType() const -> TypeId {
    return builtin_types.TickType();
  }
  [[nodiscard]] auto IntType() const -> TypeId {
    return builtin_types.IntType();
  }
  [[nodiscard]] auto Int4Type() const -> TypeId {
    return builtin_types.Int4Type();
  }
  [[nodiscard]] auto BitType() const -> TypeId {
    return builtin_types.BitType();
  }
  [[nodiscard]] auto LogicType() const -> TypeId {
    return builtin_types.LogicType();
  }
  [[nodiscard]] auto UInt32Type() const -> TypeId {
    return builtin_types.UInt32Type();
  }
  [[nodiscard]] auto RealType() const -> TypeId {
    return builtin_types.RealType();
  }
  [[nodiscard]] auto ShortRealType() const -> TypeId {
    return builtin_types.ShortRealType();
  }
  [[nodiscard]] auto StringType() const -> TypeId {
    return builtin_types.StringType();
  }
  [[nodiscard]] auto VoidType() const -> TypeId {
    return builtin_types.VoidType();
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
