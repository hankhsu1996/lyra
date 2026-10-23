#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CallExpression;
class Expression;
class NewCovergroupExpression;
class Symbol;
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `member` is one the front end declares on a covergroup, a coverpoint
// or a cross -- `option`, `type_option`, a built-in method (LRM 19.8, 19.10)
// -- or is a coverpoint or a cross itself.
[[nodiscard]] auto IsCovergroupMember(const slang::ast::Symbol& member) -> bool;

// Whether `options` names the `option` or the `type_option` of a covergroup, a
// coverpoint or a cross (LRM 19.7, 19.10).
[[nodiscard]] auto NamesCoverageOptions(const slang::ast::Expression& options)
    -> bool;

// The option `field` of what `options` names, of type `type`, read and written
// as a variable is.
template <ExprLowerer Lowerer>
auto LowerCoverageOption(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::Expression& options, std::string_view field,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Whether `member`, named without a handle in an expression of a covergroup a
// class embeds (LRM 19.4), is a member of the embedding object.
[[nodiscard]] auto ReachedThroughEmbedding(
    const WalkFrame& frame, const slang::ast::Symbol& member) -> bool;

// That object, as the covergroup's expressions reach it.
auto EmbeddingObject(const WalkFrame& frame, diag::SourceSpan span)
    -> hir::ExprId;

// `new` of a covergroup (LRM 19.3), given what the source passed, already
// lowered.
template <ExprLowerer Lowerer>
auto LowerNewCovergroup(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::NewCovergroupExpression& expr,
    std::vector<hir::ExprId> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// A call of a built-in method of a covergroup, a coverpoint or a cross (LRM
// 19.8), given what the source passed, already lowered.
template <ExprLowerer Lowerer>
auto LowerCovergroupMethodCall(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::CallExpression& call,
    std::vector<std::optional<hir::ExprId>> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
