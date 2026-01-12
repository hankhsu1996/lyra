#include <cstddef>
#include <cstdint>
#include <ios>

#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::compiler {

void Codegen::EmitConditional(
    const mir::ConditionalStatement& cond, bool is_else_if) {
  if (is_else_if) {
    out_ << " else if (";
  } else {
    Indent();
    out_ << "if (";
  }
  EmitExpression(*cond.condition);
  out_ << ") {\n";
  indent_++;
  EmitStatement(*cond.then_branch);
  indent_--;

  if (cond.else_branch) {
    // Check if else branch is another conditional (else-if chain)
    if (cond.else_branch->kind == mir::Statement::Kind::kConditional) {
      Indent();
      out_ << "}";
      const auto& else_cond =
          mir::As<mir::ConditionalStatement>(*cond.else_branch);
      EmitConditional(else_cond, true);
      return;  // The recursive call handles closing brace
    }
    // Regular else branch
    Indent();
    out_ << "} else {\n";
    indent_++;
    EmitStatement(*cond.else_branch);
    indent_--;
  }

  Indent();
  out_ << "}\n";
}

void Codegen::EmitUniquePriorityIf(const mir::ConditionalStatement& root) {
  auto [conditions, bodies, final_else] = mir::CollectIfChain(root);

  bool needs_overlap_check = root.check == mir::UniquePriorityCheck::kUnique ||
                             root.check == mir::UniquePriorityCheck::kUnique0;

  // Block scope for temp variables
  Indent();
  out_ << "{\n";
  indent_++;

  if (needs_overlap_check) {
    // unique/unique0 if: Evaluate ALL conditions, count, check overlap
    used_features_ |= CodegenFeature::kModuleName;

    // Declare match counter and condition results
    Line("int _if_count = 0;");
    for (size_t i = 0; i < conditions.size(); ++i) {
      Indent();
      out_ << "bool _cond_" << i << " = static_cast<bool>(";
      EmitExpression(*conditions[i]);
      out_ << ");\n";
      Indent();
      out_ << "if (_cond_" << i << ") _if_count++;\n";
    }

    // Overlap check: if (count > 1) emit warning
    Line("if (_if_count > 1) {");
    indent_++;
    Line(
        "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
        "\"multiple conditions true in unique if\");");
    indent_--;
    Line("}");

    // No-match check (unique only, not unique0)
    bool needs_no_match = root.check == mir::UniquePriorityCheck::kUnique &&
                          final_else == nullptr;
    if (needs_no_match) {
      Line("if (_if_count == 0) {");
      indent_++;
      Line(
          "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
          "\"no condition matched in unique if\");");
      indent_--;
      Line("}");
    }

    // Dispatch to first true condition
    bool first = true;
    for (size_t i = 0; i < conditions.size(); ++i) {
      Indent();
      if (first) {
        out_ << "if (_cond_" << i << ") {\n";
        first = false;
      } else {
        out_ << "} else if (_cond_" << i << ") {\n";
      }
      indent_++;
      if (bodies[i] != nullptr) {
        EmitStatement(*bodies[i]);
      }
      indent_--;
    }

    // Final else or close
    if (final_else != nullptr) {
      if (!conditions.empty()) {
        Indent();
        out_ << "} else {\n";
        indent_++;
        EmitStatement(*final_else);
        indent_--;
      } else {
        EmitStatement(*final_else);
      }
    }

    if (!conditions.empty()) {
      Indent();
      out_ << "}\n";
    }
  } else {
    // priority if: Short-circuit with no-match check
    used_features_ |= CodegenFeature::kModuleName;

    // Track if any branch was taken (only if no final else)
    if (final_else == nullptr) {
      Line("bool _matched = false;");
    }

    // Emit if-else-if chain
    bool first = true;
    for (size_t i = 0; i < conditions.size(); ++i) {
      Indent();
      if (first) {
        out_ << "if (";
        first = false;
      } else {
        out_ << "} else if (";
      }
      EmitExpression(*conditions[i]);
      out_ << ") {\n";
      indent_++;
      if (final_else == nullptr) {
        Line("_matched = true;");
      }
      if (bodies[i] != nullptr) {
        EmitStatement(*bodies[i]);
      }
      indent_--;
    }

    // Final else or close
    if (final_else != nullptr) {
      if (!conditions.empty()) {
        Indent();
        out_ << "} else {\n";
        indent_++;
        EmitStatement(*final_else);
        indent_--;
      } else {
        EmitStatement(*final_else);
      }
    }

    if (!conditions.empty()) {
      Indent();
      out_ << "}\n";
    }

    // No-match check (only if no final else)
    if (final_else == nullptr) {
      Line("if (!_matched) {");
      indent_++;
      Line(
          "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
          "\"no condition matched in priority if\");");
      indent_--;
      Line("}");
    }
  }

  indent_--;
  Indent();
  out_ << "}\n";
}

void Codegen::EmitCaseStatement(const mir::CaseStatement& case_stmt) {
  // Check if this is unique/unique0 (needs overlap detection)
  bool needs_overlap_check =
      case_stmt.check == mir::UniquePriorityCheck::kUnique ||
      case_stmt.check == mir::UniquePriorityCheck::kUnique0;

  // Block scope for the condition temp variable
  Indent();
  out_ << "{\n";
  indent_++;

  // Emit condition once into a temp
  Indent();
  out_ << "auto _case_cond = ";
  EmitExpression(*case_stmt.condition);
  out_ << ";\n";

  // Helper lambda to emit a case item's condition expression
  auto emit_item_condition = [&](const mir::CaseItem& item) {
    for (size_t i = 0; i < item.expressions.size(); ++i) {
      if (i > 0) {
        out_ << " || ";
      }
      int64_t mask = item.masks[i];
      auto umask = static_cast<uint64_t>(mask);
      bool is_all_ones = (mask == -1) || (umask == 0xFFFFFFFF) ||
                         (umask == 0xFFFFFFFFFFFFFFFF);
      if (is_all_ones) {
        out_ << "_case_cond == ";
      } else {
        out_ << "(static_cast<uint64_t>(_case_cond) & 0x" << std::hex << umask
             << std::dec << "ULL) == ";
      }
      EmitExpression(*item.expressions[i]);
    }
  };

  if (needs_overlap_check) {
    // unique/unique0: Evaluate ALL conditions, count matches, check overlap
    used_features_ |= CodegenFeature::kModuleName;

    // Declare match counter and flags
    Line("int _match_count = 0;");
    for (size_t i = 0; i < case_stmt.items.size(); ++i) {
      Indent();
      out_ << "bool _matched_" << i << " = false;\n";
    }

    // Evaluate all conditions and increment counter
    for (size_t i = 0; i < case_stmt.items.size(); ++i) {
      const auto& item = case_stmt.items[i];
      Indent();
      out_ << "if (";
      emit_item_condition(item);
      out_ << ") {\n";
      indent_++;
      Indent();
      out_ << "_matched_" << i << " = true;\n";
      Line("_match_count++;");
      indent_--;
      Line("}");
    }

    // Overlap check: if (count > 1) emit warning
    Line("if (_match_count > 1) {");
    indent_++;
    Line(
        "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
        "\"multiple case items match\");");
    indent_--;
    Line("}");

    // No-match check (unique only, not unique0)
    bool needs_no_match =
        case_stmt.check == mir::UniquePriorityCheck::kUnique &&
        !case_stmt.default_case;
    if (needs_no_match) {
      Line("if (_match_count == 0) {");
      indent_++;
      Line(
          "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
          "\"no matching case item\");");
      indent_--;
      Line("}");
    }

    // Dispatch to first matching item
    bool first = true;
    for (size_t i = 0; i < case_stmt.items.size(); ++i) {
      Indent();
      if (first) {
        out_ << "if (_matched_" << i << ") {\n";
        first = false;
      } else {
        out_ << "} else if (_matched_" << i << ") {\n";
      }
      indent_++;
      if (case_stmt.items[i].statement) {
        EmitStatement(*case_stmt.items[i].statement);
      }
      indent_--;
    }

    // Default case or close
    if (case_stmt.default_case) {
      if (!case_stmt.items.empty()) {
        Indent();
        out_ << "} else {\n";
        indent_++;
        EmitStatement(*case_stmt.default_case);
        indent_--;
      } else {
        EmitStatement(*case_stmt.default_case);
      }
    }

    if (!case_stmt.items.empty()) {
      Indent();
      out_ << "}\n";
    }
  } else {
    // Regular or priority case: short-circuit if-else-if chain
    bool first = true;
    for (const auto& item : case_stmt.items) {
      Indent();
      if (first) {
        out_ << "if (";
        first = false;
      } else {
        out_ << "} else if (";
      }
      emit_item_condition(item);
      out_ << ") {\n";
      indent_++;
      if (item.statement) {
        EmitStatement(*item.statement);
      }
      indent_--;
    }

    // Default case or priority no-match warning
    if (case_stmt.default_case) {
      if (!case_stmt.items.empty()) {
        Indent();
        out_ << "} else {\n";
        indent_++;
        EmitStatement(*case_stmt.default_case);
        indent_--;
      } else {
        EmitStatement(*case_stmt.default_case);
      }
    } else if (
        case_stmt.check == mir::UniquePriorityCheck::kPriority &&
        !case_stmt.items.empty()) {
      used_features_ |= CodegenFeature::kModuleName;
      Indent();
      out_ << "} else {\n";
      indent_++;
      Line(
          "lyra::sdk::SeverityMessage(\"WARNING\", \"\", 0, kModuleName, "
          "\"no matching case item\");");
      indent_--;
    }

    if (!case_stmt.items.empty()) {
      Indent();
      out_ << "}\n";
    }
  }

  // Close block scope
  indent_--;
  Indent();
  out_ << "}\n";
}

}  // namespace lyra::compiler
