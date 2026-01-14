#pragma once

namespace lyra::mir {
class Statement;
class ConditionalStatement;
class CaseStatement;
class WhileStatement;
class DoWhileStatement;
class ForStatement;
class RepeatStatement;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_lir {
class LirBuilder;
class LoweringContext;

// Conditional statement lowering
void LowerConditionalStatement(
    const mir::ConditionalStatement& if_stmt, LirBuilder& builder,
    LoweringContext& lowering_context);
void LowerCaseStatement(
    const mir::CaseStatement& case_stmt, LirBuilder& builder,
    LoweringContext& lowering_context);

// Loop statement lowering
void LowerWhileLoop(
    const mir::WhileStatement& statement, LirBuilder& builder,
    LoweringContext& lowering_context);
void LowerDoWhileLoop(
    const mir::DoWhileStatement& statement, LirBuilder& builder,
    LoweringContext& lowering_context);
void LowerForLoop(
    const mir::ForStatement& statement, LirBuilder& builder,
    LoweringContext& lowering_context);
void LowerRepeatLoop(
    const mir::RepeatStatement& statement, LirBuilder& builder,
    LoweringContext& lowering_context);

}  // namespace lyra::lowering::mir_to_lir
