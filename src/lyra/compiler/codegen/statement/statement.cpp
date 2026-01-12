#include "lyra/mir/statement.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/codegen/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::compiler {

using codegen::GetElementWidthAfterIndices;
using codegen::IsWideWidth;

void Codegen::EmitStatement(const mir::Statement& stmt) {
  switch (stmt.kind) {
    case mir::Statement::Kind::kBlock: {
      const auto& block = mir::As<mir::BlockStatement>(stmt);
      for (const auto& s : block.statements) {
        EmitStatement(*s);
      }
      break;
    }
    case mir::Statement::Kind::kAssign: {
      const auto& assign = mir::As<mir::AssignStatement>(stmt);
      out_ << std::string(indent_ * 2, ' ');

      if (assign.target.IsPacked() && assign.target.IsElementSelect()) {
        // Packed element assignment: vec[idx] = val (possibly multi-dim)
        const auto& base_type = assign.target.base_type.value();
        size_t total_width = base_type.GetBitWidth();
        size_t element_width = GetElementWidthAfterIndices(
            base_type, assign.target.indices.size());

        bool storage_is_wide = IsWideWidth(total_width);
        bool element_is_wide = IsWideWidth(element_width);

        if (storage_is_wide || element_is_wide) {
          // Wide storage or wide element - use InsertSlice
          // Generate: vec = vec.InsertSlice(val, bit_position, element_width)
          out_ << assign.target.symbol->name << " = "
               << assign.target.symbol->name << ".InsertSlice(";
          EmitExpression(*assign.value);
          out_ << ", ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << ", " << element_width << ");\n";
        } else {
          // Narrow storage and narrow element - use mask-and-merge
          // Generate: vec = (vec & ~(mask << adj_idx)) | ((val & mask) <<
          // adj_idx)
          used_type_aliases_ |= TypeAlias::kBit;
          uint64_t mask = common::MakeBitMask(element_width);
          out_ << assign.target.symbol->name << " = ("
               << assign.target.symbol->name << " & ~(Bit<" << total_width
               << ">{" << mask << "ULL} << ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << ")) | ((Bit<" << total_width << ">{";
          EmitExpression(*assign.value);
          out_ << ".Value() & " << mask << "ULL} << ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << "));\n";
        }
      } else {
        EmitAssignmentTarget(assign.target);
        out_ << " = ";
        EmitExpression(*assign.value);
        out_ << ";\n";
      }
      break;
    }
    case mir::Statement::Kind::kExpression: {
      const auto& expr_stmt = mir::As<mir::ExpressionStatement>(stmt);
      // Handle system tasks specially (not system functions)
      if (expr_stmt.expression->kind == mir::Expression::Kind::kSystemCall) {
        const auto& syscall =
            mir::As<mir::SystemCallExpression>(*expr_stmt.expression);
        if (syscall.is_task) {
          EmitSystemTask(syscall);
          break;
        }
        // System functions fall through to expression handling
      }
      // Check if expression is an assignment that produces an unused value
      // (packed struct field or packed element assignment uses comma
      // expression)
      bool needs_void_cast = false;
      if (expr_stmt.expression->kind == mir::Expression::Kind::kAssignment) {
        const auto& assign =
            mir::As<mir::AssignmentExpression>(*expr_stmt.expression);
        // Only packed structs need void cast; unpacked structs use direct
        // assignment
        bool is_packed_struct_field =
            assign.target.IsStructFieldAssignment() &&
            assign.target.base_type &&
            !assign.target.base_type->IsUnpackedStruct();
        needs_void_cast =
            is_packed_struct_field ||
            (assign.target.IsPacked() && assign.target.IsElementSelect());
      }
      Indent();
      if (needs_void_cast) {
        out_ << "(void)(";
      }
      EmitExpression(*expr_stmt.expression);
      if (needs_void_cast) {
        out_ << ")";
      }
      out_ << ";\n";
      break;
    }
    case mir::Statement::Kind::kConditional: {
      const auto& cond = mir::As<mir::ConditionalStatement>(stmt);

      // Check if this is unique/unique0/priority if
      bool needs_overlap_check =
          cond.check == mir::UniquePriorityCheck::kUnique ||
          cond.check == mir::UniquePriorityCheck::kUnique0;
      bool needs_priority_check =
          cond.check == mir::UniquePriorityCheck::kPriority;

      if (needs_overlap_check || needs_priority_check) {
        EmitUniquePriorityIf(cond);
      } else {
        EmitConditional(cond, false);
      }
      break;
    }
    case mir::Statement::Kind::kWhile: {
      const auto& while_stmt = mir::As<mir::WhileStatement>(stmt);
      EmitWhileLoop(while_stmt);
      break;
    }
    case mir::Statement::Kind::kDoWhile: {
      const auto& do_while = mir::As<mir::DoWhileStatement>(stmt);
      EmitDoWhileLoop(do_while);
      break;
    }
    case mir::Statement::Kind::kFor: {
      const auto& for_stmt = mir::As<mir::ForStatement>(stmt);
      EmitForLoop(for_stmt);
      break;
    }
    case mir::Statement::Kind::kRepeat: {
      const auto& repeat_stmt = mir::As<mir::RepeatStatement>(stmt);
      EmitRepeatLoop(repeat_stmt);
      break;
    }
    case mir::Statement::Kind::kCase: {
      const auto& case_stmt = mir::As<mir::CaseStatement>(stmt);
      EmitCaseStatement(case_stmt);
      break;
    }
    case mir::Statement::Kind::kBreak: {
      Line("break;");
      break;
    }
    case mir::Statement::Kind::kContinue: {
      Line("continue;");
      break;
    }
    case mir::Statement::Kind::kDelay: {
      const auto& delay = mir::As<mir::DelayStatement>(stmt);
      // Scale delay based on module's timescale
      uint64_t scaled_delay = delay.delay_amount * DelayMultiplier();
      if (scaled_delay == 0) {
        // #0 delay goes to Inactive region (same time slot)
        Line("co_await lyra::sdk::ZeroDelay();");
      } else {
        // Non-zero delay goes to delay queue (future time slot)
        Line(
            "co_await lyra::sdk::Delay(" + std::to_string(scaled_delay) + ");");
      }
      break;
    }
    case mir::Statement::Kind::kWaitEvent: {
      const auto& wait = mir::As<mir::WaitEventStatement>(stmt);
      if (wait.triggers.empty()) {
        // Constant continuous assignments have no triggers - nothing will ever
        // cause re-evaluation, so execute once and complete the process.
        Line("co_return;");
        break;
      }

      // Helper to generate trigger expression based on edge kind
      auto trigger_expr = [](const std::string& var_name,
                             common::EdgeKind kind) -> std::string {
        switch (kind) {
          case common::EdgeKind::kPosedge:
            return "lyra::sdk::Posedge(&" + var_name + ")";
          case common::EdgeKind::kNegedge:
            return "lyra::sdk::Negedge(&" + var_name + ")";
          case common::EdgeKind::kAnyChange:
          case common::EdgeKind::kBothEdge:
            return "lyra::sdk::Change(&" + var_name + ")";
        }
        return "lyra::sdk::Change(&" + var_name + ")";
      };

      if (wait.triggers.size() == 1) {
        // Single trigger: co_await Posedge(&clk_);
        const auto& trigger = wait.triggers[0];
        std::string var_path = GetTriggerPath(trigger);
        Line("co_await " + trigger_expr(var_path, trigger.edge_kind) + ";");
      } else {
        // Check if all triggers are AnyChange
        bool all_any_change =
            std::ranges::all_of(wait.triggers, [](const auto& trigger) {
              return trigger.edge_kind == common::EdgeKind::kAnyChange;
            });

        if (all_any_change) {
          // Optimized: co_await AnyChange(&a, &b, &c);
          Indent();
          out_ << "co_await lyra::sdk::AnyChange(";
          for (size_t i = 0; i < wait.triggers.size(); ++i) {
            if (i > 0) {
              out_ << ", ";
            }
            out_ << "&" << GetTriggerPath(wait.triggers[i]);
          }
          out_ << ");\n";
        } else {
          // Mixed triggers: co_await AnyOf(Posedge(&clk_), Negedge(&rst_));
          Indent();
          out_ << "co_await lyra::sdk::AnyOf(";
          for (size_t i = 0; i < wait.triggers.size(); ++i) {
            if (i > 0) {
              out_ << ", ";
            }
            const auto& trigger = wait.triggers[i];
            std::string var_path = GetTriggerPath(trigger);
            out_ << trigger_expr(var_path, trigger.edge_kind);
          }
          out_ << ");\n";
        }
      }
      break;
    }
    case mir::Statement::Kind::kVariableDeclaration: {
      const auto& decl = mir::As<mir::VariableDeclarationStatement>(stmt);
      Indent();
      out_ << ToCppType(decl.variable.type) << " "
           << decl.variable.symbol->name;
      if (decl.initializer) {
        out_ << " = ";
        EmitExpression(*decl.initializer);
      } else {
        out_ << "{}";
      }
      out_ << ";\n";
      break;
    }
    case mir::Statement::Kind::kReturn: {
      const auto& ret = mir::As<mir::ReturnStatement>(stmt);
      if (ret.value) {
        Indent();
        out_ << "return ";
        EmitExpression(*ret.value);
        out_ << ";\n";
      } else {
        Line("return;");
      }
      break;
    }
    default:
      throw DiagnosticException(
          Diagnostic::Error(
              {}, "C++ codegen: unimplemented statement kind: " +
                      ToString(stmt.kind)));
  }
}

}  // namespace lyra::compiler
