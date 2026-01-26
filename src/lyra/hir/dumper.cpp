#include "lyra/hir/dumper.hpp"

#include <format>
#include <ostream>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/symbol_table.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"

namespace lyra::hir {

Dumper::Dumper(
    const Arena* arena, const TypeArena* types, const ConstantArena* constants,
    const SymbolTable* symbols, std::ostream* out)
    : arena_(arena),
      types_(types),
      constants_(constants),
      symbols_(symbols),
      out_(out) {
}

void Dumper::PrintIndent() {
  for (int i = 0; i < indent_; ++i) {
    *out_ << "  ";
  }
}

void Dumper::Indent() {
  ++indent_;
}

void Dumper::Dedent() {
  if (indent_ == 0) {
    throw common::InternalError("Dumper::Dedent", "indent underflow");
  }
  --indent_;
}

auto Dumper::TypeString(TypeId id) const -> std::string {
  if (!id) {
    return "<invalid>";
  }
  return ToString((*types_)[id]);
}

auto Dumper::SymbolName(SymbolId id) const -> std::string {
  if (!id) {
    return "<invalid>";
  }
  return (*symbols_)[id].name;
}

auto Dumper::ConstantString(ConstId id) const -> std::string {
  if (!id) {
    return "<invalid>";
  }
  const Constant& c = (*constants_)[id];
  if (const auto* integral = std::get_if<IntegralConstant>(&c.value)) {
    if (integral->value.empty()) {
      return "0";
    }
    return std::format("{}", integral->value[0]);
  }
  if (const auto* str = std::get_if<StringConstant>(&c.value)) {
    return std::format("\"{}\"", str->value);
  }
  return "<constant>";
}

void Dumper::Dump(const Design& design) {
  *out_ << "Design {\n";
  Indent();
  for (const auto& element : design.elements) {
    std::visit([this](const auto& elem) { Dump(elem); }, element);
  }
  Dedent();
  *out_ << "}\n";
}

void Dumper::Dump(const Module& module) {
  PrintIndent();
  *out_ << std::format("Module {} {{\n", SymbolName(module.symbol));
  Indent();

  for (ProcessId id : module.processes) {
    Dump(id);
  }
  for (FunctionId id : module.functions) {
    Dump(id);
  }
  for (TaskId id : module.tasks) {
    Dump(id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(const Package& package) {
  PrintIndent();
  *out_ << std::format("Package {} {{}}\n", SymbolName(package.symbol));
}

void Dumper::Dump(ProcessId id) {
  const Process& proc = (*arena_)[id];
  PrintIndent();

  std::string_view kind_str = [&] {
    switch (proc.kind) {
      case ProcessKind::kInitial:
        return "initial";
      case ProcessKind::kAlways:
        return "always";
      case ProcessKind::kAlwaysComb:
        return "always_comb";
      case ProcessKind::kAlwaysFf:
        return "always_ff";
      case ProcessKind::kAlwaysLatch:
        return "always_latch";
      case ProcessKind::kFinal:
        return "final";
    }
  }();

  *out_ << kind_str << " ";
  Dump(proc.body);
}

void Dumper::Dump(FunctionId id) {
  const Function& func = (*arena_)[id];
  PrintIndent();
  *out_ << std::format(
      "function {} -> {} ", SymbolName(func.symbol),
      TypeString(func.return_type));
  Dump(func.body);
}

void Dumper::Dump(TaskId id) {
  const Task& task = (*arena_)[id];
  PrintIndent();
  *out_ << std::format("task {} ", SymbolName(task.symbol));
  Dump(task.body);
}

void Dumper::Dump(StatementId id) {
  const Statement& stmt = (*arena_)[id];

  switch (stmt.kind) {
    case StatementKind::kBlock: {
      const auto& data = std::get<BlockStatementData>(stmt.data);
      *out_ << "{\n";
      Indent();
      for (StatementId child : data.statements) {
        PrintIndent();
        Dump(child);
      }
      Dedent();
      PrintIndent();
      *out_ << "}\n";
      break;
    }

    case StatementKind::kVariableDeclaration: {
      const auto& data = std::get<VariableDeclarationStatementData>(stmt.data);
      const Symbol& sym = (*symbols_)[data.symbol];
      *out_ << std::format("var {} : {}", sym.name, TypeString(sym.type));
      if (data.init) {
        *out_ << " = ";
        Dump(data.init);
      }
      *out_ << ";\n";
      break;
    }

    case StatementKind::kAssignment: {
      const auto& data = std::get<AssignmentStatementData>(stmt.data);
      Dump(data.target);
      *out_ << " = ";
      Dump(data.value);
      *out_ << ";\n";
      break;
    }

    case StatementKind::kExpression: {
      const auto& data = std::get<ExpressionStatementData>(stmt.data);
      Dump(data.expression);
      *out_ << ";\n";
      break;
    }

    case StatementKind::kConditional: {
      const auto& data = std::get<ConditionalStatementData>(stmt.data);
      switch (data.check) {
        case UniquePriorityCheck::kNone:
          break;
        case UniquePriorityCheck::kUnique:
          *out_ << "unique ";
          break;
        case UniquePriorityCheck::kUnique0:
          *out_ << "unique0 ";
          break;
        case UniquePriorityCheck::kPriority:
          *out_ << "priority ";
          break;
      }
      *out_ << "if (";
      Dump(data.condition);
      *out_ << ") ";
      Dump(data.then_branch);
      if (data.else_branch) {
        PrintIndent();
        *out_ << "else ";
        Dump(*data.else_branch);
      }
      break;
    }

    case StatementKind::kCase: {
      const auto& data = std::get<CaseStatementData>(stmt.data);
      switch (data.check) {
        case UniquePriorityCheck::kNone:
          break;
        case UniquePriorityCheck::kUnique:
          *out_ << "unique ";
          break;
        case UniquePriorityCheck::kUnique0:
          *out_ << "unique0 ";
          break;
        case UniquePriorityCheck::kPriority:
          *out_ << "priority ";
          break;
      }
      *out_ << "case (";
      Dump(data.selector);
      *out_ << ") {\n";
      Indent();
      for (const auto& item : data.items) {
        PrintIndent();
        bool first = true;
        for (ExpressionId e : item.expressions) {
          if (!first) {
            *out_ << ", ";
          }
          first = false;
          Dump(e);
        }
        *out_ << ": ";
        Dump(item.statement);
      }
      if (data.default_statement.has_value()) {
        PrintIndent();
        *out_ << "default: ";
        Dump(*data.default_statement);
      }
      Dedent();
      PrintIndent();
      *out_ << "}\n";
      break;
    }

    case StatementKind::kForLoop: {
      const auto& data = std::get<ForLoopStatementData>(stmt.data);
      *out_ << "for (";

      bool first = true;

      // Dump variable declarations
      for (StatementId var_decl : data.var_decls) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        const Statement& var_stmt = (*arena_)[var_decl];
        const auto& var_data =
            std::get<VariableDeclarationStatementData>(var_stmt.data);
        const Symbol& sym = (*symbols_)[var_data.symbol];
        *out_ << std::format("var {} : {}", sym.name, TypeString(sym.type));
        if (var_data.init) {
          *out_ << " = ";
          Dump(var_data.init);
        }
      }

      // Dump init expressions
      for (ExpressionId init_expr : data.init_exprs) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(init_expr);
      }

      *out_ << "; ";

      if (data.condition) {
        Dump(*data.condition);
      }

      *out_ << "; ";

      // Dump step expressions
      first = true;
      for (ExpressionId step_expr : data.steps) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(step_expr);
      }

      *out_ << ") ";
      Dump(data.body);
      break;
    }

    case StatementKind::kWhileLoop: {
      const auto& data = std::get<WhileLoopStatementData>(stmt.data);
      *out_ << "while (";
      Dump(data.condition);
      *out_ << ") ";
      Dump(data.body);
      break;
    }

    case StatementKind::kDoWhileLoop: {
      const auto& data = std::get<DoWhileLoopStatementData>(stmt.data);
      *out_ << "do ";
      Dump(data.body);
      PrintIndent();
      *out_ << "while (";
      Dump(data.condition);
      *out_ << ");\n";
      break;
    }

    case StatementKind::kRepeatLoop: {
      const auto& data = std::get<RepeatLoopStatementData>(stmt.data);
      *out_ << "repeat (";
      Dump(data.count);
      *out_ << ") ";
      Dump(data.body);
      break;
    }

    case StatementKind::kBreak:
      *out_ << "break;\n";
      break;

    case StatementKind::kContinue:
      *out_ << "continue;\n";
      break;

    case StatementKind::kTerminate: {
      const auto& data = std::get<TerminateStatementData>(stmt.data);
      const char* name = nullptr;
      switch (data.kind) {
        case TerminationKind::kFinish:
          name = "$finish";
          break;
        case TerminationKind::kStop:
          name = "$stop";
          break;
        case TerminationKind::kExit:
          name = "$exit";
          break;
        case TerminationKind::kFatal:
          name = "$fatal";
          break;
      }
      *out_ << name << "(" << data.level;
      for (ExpressionId arg : data.message_args) {
        *out_ << ", ";
        Dump(arg);
      }
      *out_ << ");\n";
      break;
    }

    case StatementKind::kReturn: {
      const auto& data = std::get<ReturnStatementData>(stmt.data);
      *out_ << "return";
      if (data.value) {
        *out_ << " ";
        Dump(data.value);
      }
      *out_ << ";\n";
      break;
    }

    case StatementKind::kDelay: {
      const auto& data = std::get<DelayStatementData>(stmt.data);
      *out_ << std::format("#{};\n", data.ticks);
      break;
    }

    case StatementKind::kEventWait: {
      *out_ << "@(...);\n";
      break;
    }
  }
}

void Dumper::Dump(ExpressionId id) {
  const Expression& expr = (*arena_)[id];

  switch (expr.kind) {
    case ExpressionKind::kConstant: {
      const auto& data = std::get<ConstantExpressionData>(expr.data);
      *out_ << ConstantString(data.constant);
      break;
    }

    case ExpressionKind::kNameRef: {
      const auto& data = std::get<NameRefExpressionData>(expr.data);
      *out_ << SymbolName(data.symbol);
      break;
    }

    case ExpressionKind::kUnaryOp: {
      const auto& data = std::get<UnaryExpressionData>(expr.data);
      struct UnaryInfo {
        std::string_view str;
        bool is_prefix;
      };
      auto info = [&]() -> UnaryInfo {
        switch (data.op) {
          case UnaryOp::kPlus:
            return {.str = "+", .is_prefix = true};
          case UnaryOp::kMinus:
            return {.str = "-", .is_prefix = true};
          case UnaryOp::kPreincrement:
            return {.str = "++", .is_prefix = true};
          case UnaryOp::kPostincrement:
            return {.str = "++", .is_prefix = false};
          case UnaryOp::kPredecrement:
            return {.str = "--", .is_prefix = true};
          case UnaryOp::kPostdecrement:
            return {.str = "--", .is_prefix = false};
          case UnaryOp::kLogicalNot:
            return {.str = "!", .is_prefix = true};
          case UnaryOp::kBitwiseNot:
            return {.str = "~", .is_prefix = true};
          case UnaryOp::kReductionAnd:
            return {.str = "&", .is_prefix = true};
          case UnaryOp::kReductionNand:
            return {.str = "~&", .is_prefix = true};
          case UnaryOp::kReductionOr:
            return {.str = "|", .is_prefix = true};
          case UnaryOp::kReductionNor:
            return {.str = "~|", .is_prefix = true};
          case UnaryOp::kReductionXor:
            return {.str = "^", .is_prefix = true};
          case UnaryOp::kReductionXnor:
            return {.str = "~^", .is_prefix = true};
        }
        return {.str = "?", .is_prefix = true};
      }();
      if (info.is_prefix) {
        *out_ << info.str;
        Dump(data.operand);
      } else {
        Dump(data.operand);
        *out_ << info.str;
      }
      break;
    }

    case ExpressionKind::kBinaryOp: {
      const auto& data = std::get<BinaryExpressionData>(expr.data);
      *out_ << "(";
      Dump(data.lhs);
      std::string_view op_str = [&]() -> std::string_view {
        switch (data.op) {
          case BinaryOp::kAdd:
            return " + ";
          case BinaryOp::kSubtract:
            return " - ";
          case BinaryOp::kMultiply:
            return " * ";
          case BinaryOp::kDivide:
            return " / ";
          case BinaryOp::kMod:
            return " % ";
          case BinaryOp::kPower:
            return " ** ";
          case BinaryOp::kBitwiseAnd:
            return " & ";
          case BinaryOp::kBitwiseOr:
            return " | ";
          case BinaryOp::kBitwiseXor:
            return " ^ ";
          case BinaryOp::kBitwiseXnor:
            return " ~^ ";
          case BinaryOp::kLogicalAnd:
            return " && ";
          case BinaryOp::kLogicalOr:
            return " || ";
          case BinaryOp::kLogicalImplication:
            return " -> ";
          case BinaryOp::kLogicalEquivalence:
            return " <-> ";
          case BinaryOp::kEqual:
            return " == ";
          case BinaryOp::kNotEqual:
            return " != ";
          case BinaryOp::kCaseEqual:
            return " === ";
          case BinaryOp::kCaseNotEqual:
            return " !== ";
          case BinaryOp::kWildcardEqual:
            return " ==? ";
          case BinaryOp::kWildcardNotEqual:
            return " !=? ";
          case BinaryOp::kLessThan:
            return " < ";
          case BinaryOp::kLessThanEqual:
            return " <= ";
          case BinaryOp::kGreaterThan:
            return " > ";
          case BinaryOp::kGreaterThanEqual:
            return " >= ";
          case BinaryOp::kLogicalShiftLeft:
            return " << ";
          case BinaryOp::kLogicalShiftRight:
            return " >> ";
          case BinaryOp::kArithmeticShiftLeft:
            return " <<< ";
          case BinaryOp::kArithmeticShiftRight:
            return " >>> ";
        }
        return " ? ";
      }();
      *out_ << op_str;
      Dump(data.rhs);
      *out_ << ")";
      break;
    }

    case ExpressionKind::kCast: {
      const auto& data = std::get<CastExpressionData>(expr.data);
      *out_ << "cast<" << TypeString(expr.type) << ">(";
      Dump(data.operand);
      *out_ << ")";
      break;
    }

    case ExpressionKind::kBitCast: {
      const auto& data = std::get<BitCastExpressionData>(expr.data);
      *out_ << "bitcast<" << TypeString(expr.type) << ">(";
      Dump(data.operand);
      *out_ << ")";
      break;
    }

    case ExpressionKind::kSystemCall: {
      const auto& data = std::get<SystemCallExpressionData>(expr.data);
      std::visit(
          [&](const auto& syscall_data) {
            using T = std::decay_t<decltype(syscall_data)>;
            if constexpr (std::is_same_v<T, DisplaySystemCallData>) {
              const char* name = nullptr;
              if (syscall_data.descriptor.has_value()) {
                name = syscall_data.print_kind == PrintKind::kDisplay
                           ? "$fdisplay"
                           : "$fwrite";
              } else {
                name = syscall_data.print_kind == PrintKind::kDisplay
                           ? "$display"
                           : "$write";
              }
              *out_ << name << "(";
              bool first = true;
              if (syscall_data.descriptor) {
                Dump(*syscall_data.descriptor);
                first = false;
              }
              for (const FormatOp& op : syscall_data.ops) {
                if (!first) {
                  *out_ << ", ";
                }
                first = false;
                if (op.kind == FormatKind::kLiteral) {
                  *out_ << std::format("literal(\"{}\")", op.literal);
                } else {
                  const char* kind_str = "?";
                  switch (op.kind) {
                    case FormatKind::kDecimal:
                      kind_str = "%d";
                      break;
                    case FormatKind::kHex:
                      kind_str = "%h";
                      break;
                    case FormatKind::kBinary:
                      kind_str = "%b";
                      break;
                    case FormatKind::kOctal:
                      kind_str = "%o";
                      break;
                    case FormatKind::kString:
                      kind_str = "%s";
                      break;
                    case FormatKind::kReal:
                      kind_str = "%f";
                      break;
                    case FormatKind::kTime:
                      kind_str = "%t";
                      break;
                    case FormatKind::kLiteral:
                      break;  // Already handled above
                  }
                  *out_ << kind_str << ":";
                  Dump(*op.value);
                }
              }
              *out_ << ")";
            } else if constexpr (std::is_same_v<T, SeveritySystemCallData>) {
              std::string_view name = [&] {
                switch (syscall_data.level) {
                  case Severity::kInfo:
                    return "$info";
                  case Severity::kWarning:
                    return "$warning";
                  case Severity::kError:
                    return "$error";
                }
              }();
              *out_ << name << "(";
              bool first = true;
              for (ExpressionId arg : syscall_data.args) {
                if (!first) {
                  *out_ << ", ";
                }
                first = false;
                Dump(arg);
              }
              *out_ << ")";
            }
          },
          data);
      break;
    }

    case ExpressionKind::kConditional: {
      const auto& data = std::get<ConditionalExpressionData>(expr.data);
      *out_ << "(";
      Dump(data.condition);
      *out_ << " ? ";
      Dump(data.then_expr);
      *out_ << " : ";
      Dump(data.else_expr);
      *out_ << ")";
      break;
    }

    case ExpressionKind::kAssignment: {
      const auto& data = std::get<AssignmentExpressionData>(expr.data);
      Dump(data.target);
      *out_ << " = ";
      Dump(data.value);
      break;
    }

    case ExpressionKind::kCompoundAssignment: {
      const auto& data = std::get<CompoundAssignmentExpressionData>(expr.data);
      Dump(data.target);
      std::string_view op_str = [&] {
        switch (data.op) {
          case BinaryOp::kAdd:
            return " += ";
          case BinaryOp::kSubtract:
            return " -= ";
          case BinaryOp::kMultiply:
            return " *= ";
          case BinaryOp::kDivide:
            return " /= ";
          case BinaryOp::kMod:
            return " %= ";
          case BinaryOp::kBitwiseAnd:
            return " &= ";
          case BinaryOp::kBitwiseOr:
            return " |= ";
          case BinaryOp::kBitwiseXor:
            return " ^= ";
          case BinaryOp::kLogicalShiftLeft:
            return " <<= ";
          case BinaryOp::kLogicalShiftRight:
            return " >>= ";
          case BinaryOp::kArithmeticShiftLeft:
            return " <<<= ";
          case BinaryOp::kArithmeticShiftRight:
            return " >>>= ";
          default:
            return " <?op>= ";
        }
      }();
      *out_ << op_str;
      Dump(data.operand);
      break;
    }

    case ExpressionKind::kElementAccess: {
      const auto& data = std::get<ElementAccessExpressionData>(expr.data);
      Dump(data.base);
      *out_ << "[";
      Dump(data.index);
      *out_ << "]";
      break;
    }

    case ExpressionKind::kMemberAccess: {
      const auto& data = std::get<MemberAccessExpressionData>(expr.data);
      Dump(data.base);
      *out_ << ".<" << data.field_index << ">";
      break;
    }

    case ExpressionKind::kUnionMemberAccess: {
      const auto& data = std::get<UnionMemberAccessExpressionData>(expr.data);
      Dump(data.base);
      *out_ << ".union<" << data.member_index << ">";
      break;
    }

    case ExpressionKind::kStructLiteral: {
      const auto& data = std::get<StructLiteralExpressionData>(expr.data);
      *out_ << "'{";
      bool first = true;
      for (ExpressionId field_expr : data.field_values) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(field_expr);
      }
      *out_ << "}";
      break;
    }

    case ExpressionKind::kArrayLiteral: {
      const auto& data = std::get<ArrayLiteralExpressionData>(expr.data);
      *out_ << "'{";
      bool first = true;
      for (ExpressionId elem : data.elements) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(elem);
      }
      *out_ << "}";
      break;
    }

    case ExpressionKind::kCall: {
      const auto& data = std::get<CallExpressionData>(expr.data);
      *out_ << SymbolName(data.callee) << "(";
      bool first = true;
      for (ExpressionId arg : data.arguments) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(arg);
      }
      *out_ << ")";
      break;
    }

    case ExpressionKind::kNewArray: {
      const auto& data = std::get<NewArrayExpressionData>(expr.data);
      *out_ << "new[";
      Dump(data.size_expr);
      *out_ << "]";
      if (data.init_expr) {
        *out_ << "(";
        Dump(*data.init_expr);
        *out_ << ")";
      }
      break;
    }

    case ExpressionKind::kBuiltinMethodCall: {
      const auto& data = std::get<BuiltinMethodCallExpressionData>(expr.data);
      Dump(data.receiver);
      switch (data.method) {
        case BuiltinMethod::kSize:
          *out_ << ".size()";
          break;
        case BuiltinMethod::kDelete:
          *out_ << ".delete(";
          break;
        case BuiltinMethod::kPushBack:
          *out_ << ".push_back(";
          break;
        case BuiltinMethod::kPushFront:
          *out_ << ".push_front(";
          break;
        case BuiltinMethod::kPopBack:
          *out_ << ".pop_back()";
          break;
        case BuiltinMethod::kPopFront:
          *out_ << ".pop_front()";
          break;
        case BuiltinMethod::kInsert:
          *out_ << ".insert(";
          break;
        case BuiltinMethod::kEnumNext:
          *out_ << ".next(";
          break;
        case BuiltinMethod::kEnumPrev:
          *out_ << ".prev(";
          break;
        case BuiltinMethod::kEnumName:
          *out_ << ".name()";
          break;
      }
      // Methods with no args or that close parens themselves
      bool no_args = data.method == BuiltinMethod::kSize ||
                     data.method == BuiltinMethod::kPopBack ||
                     data.method == BuiltinMethod::kPopFront ||
                     data.method == BuiltinMethod::kEnumName;
      if (!no_args) {
        bool first = true;
        for (ExpressionId arg : data.args) {
          if (!first) {
            *out_ << ", ";
          }
          first = false;
          Dump(arg);
        }
        *out_ << ")";
      }
      break;
    }

    case ExpressionKind::kPackedElementSelect: {
      const auto& data = std::get<PackedElementSelectExpressionData>(expr.data);
      Dump(data.base);
      *out_ << "[";
      Dump(data.index);
      *out_ << "]";  // Same as element access for dump purposes
      break;
    }

    case ExpressionKind::kPackedFieldAccess: {
      const auto& data = std::get<PackedFieldAccessExpressionData>(expr.data);
      Dump(data.base);
      *out_ << ".<packed:" << data.field_index << ">";
      break;
    }

    case ExpressionKind::kBitSelect: {
      const auto& data = std::get<BitSelectExpressionData>(expr.data);
      Dump(data.base);
      *out_ << "[";
      Dump(data.index);
      *out_ << "]";  // Same as element access for dump purposes
      break;
    }

    case ExpressionKind::kRangeSelect: {
      const auto& data = std::get<RangeSelectExpressionData>(expr.data);
      Dump(data.base);
      *out_ << "[" << data.left << ":" << data.right << "]";
      break;
    }

    case ExpressionKind::kIndexedPartSelect: {
      const auto& data = std::get<IndexedPartSelectExpressionData>(expr.data);
      Dump(data.base);
      *out_ << "[";
      Dump(data.index);
      *out_ << (data.ascending ? " +: " : " -: ");
      *out_ << data.width << "]";
      break;
    }

    case ExpressionKind::kConcat: {
      const auto& data = std::get<ConcatExpressionData>(expr.data);
      *out_ << "{";
      bool first = true;
      for (ExpressionId op : data.operands) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(op);
      }
      *out_ << "}";
      break;
    }

    case ExpressionKind::kHierarchicalRef: {
      const auto& data = std::get<HierarchicalRefExpressionData>(expr.data);
      *out_ << SymbolName(data.target);
      break;
    }

    case ExpressionKind::kMathCall: {
      const auto& data = std::get<MathCallExpressionData>(expr.data);
      *out_ << ToString(data.fn) << "(";
      bool first = true;
      for (ExpressionId arg : data.args) {
        if (!first) {
          *out_ << ", ";
        }
        first = false;
        Dump(arg);
      }
      *out_ << ")";
      break;
    }
  }
}

}  // namespace lyra::hir
