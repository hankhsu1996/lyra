#include "lyra/hir/dumper.hpp"

#include <cassert>
#include <format>

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
  assert(indent_ > 0);
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

  const char* kind_str = "initial";
  switch (proc.kind) {
    case ProcessKind::kInitial:
      kind_str = "initial";
      break;
    case ProcessKind::kAlways:
      kind_str = "always";
      break;
    case ProcessKind::kAlwaysComb:
      kind_str = "always_comb";
      break;
    case ProcessKind::kAlwaysFf:
      kind_str = "always_ff";
      break;
    case ProcessKind::kAlwaysLatch:
      kind_str = "always_latch";
      break;
    case ProcessKind::kFinal:
      kind_str = "final";
      break;
  }

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
      const char* op_str = "";
      bool is_prefix = true;
      switch (data.op) {
        case UnaryOp::kPlus:
          op_str = "+";
          break;
        case UnaryOp::kMinus:
          op_str = "-";
          break;
        case UnaryOp::kPreincrement:
          op_str = "++";
          break;
        case UnaryOp::kPostincrement:
          op_str = "++";
          is_prefix = false;
          break;
        case UnaryOp::kPredecrement:
          op_str = "--";
          break;
        case UnaryOp::kPostdecrement:
          op_str = "--";
          is_prefix = false;
          break;
        case UnaryOp::kLogicalNot:
          op_str = "!";
          break;
        case UnaryOp::kBitwiseNot:
          op_str = "~";
          break;
        case UnaryOp::kReductionAnd:
          op_str = "&";
          break;
        case UnaryOp::kReductionNand:
          op_str = "~&";
          break;
        case UnaryOp::kReductionOr:
          op_str = "|";
          break;
        case UnaryOp::kReductionNor:
          op_str = "~|";
          break;
        case UnaryOp::kReductionXor:
          op_str = "^";
          break;
        case UnaryOp::kReductionXnor:
          op_str = "~^";
          break;
      }
      if (is_prefix) {
        *out_ << op_str;
        Dump(data.operand);
      } else {
        Dump(data.operand);
        *out_ << op_str;
      }
      break;
    }

    case ExpressionKind::kBinaryOp: {
      const auto& data = std::get<BinaryExpressionData>(expr.data);
      *out_ << "(";
      Dump(data.lhs);
      const char* op_str = "";
      switch (data.op) {
        case BinaryOp::kAdd:
          op_str = " + ";
          break;
        case BinaryOp::kSubtract:
          op_str = " - ";
          break;
        case BinaryOp::kMultiply:
          op_str = " * ";
          break;
        case BinaryOp::kDivide:
          op_str = " / ";
          break;
        case BinaryOp::kMod:
          op_str = " % ";
          break;
        case BinaryOp::kPower:
          op_str = " ** ";
          break;
        case BinaryOp::kBitwiseAnd:
          op_str = " & ";
          break;
        case BinaryOp::kBitwiseOr:
          op_str = " | ";
          break;
        case BinaryOp::kBitwiseXor:
          op_str = " ^ ";
          break;
        case BinaryOp::kBitwiseXnor:
          op_str = " ~^ ";
          break;
        case BinaryOp::kLogicalAnd:
          op_str = " && ";
          break;
        case BinaryOp::kLogicalOr:
          op_str = " || ";
          break;
        case BinaryOp::kLogicalImplication:
          op_str = " -> ";
          break;
        case BinaryOp::kLogicalEquivalence:
          op_str = " <-> ";
          break;
        case BinaryOp::kEqual:
          op_str = " == ";
          break;
        case BinaryOp::kNotEqual:
          op_str = " != ";
          break;
        case BinaryOp::kCaseEqual:
          op_str = " === ";
          break;
        case BinaryOp::kCaseNotEqual:
          op_str = " !== ";
          break;
        case BinaryOp::kWildcardEqual:
          op_str = " ==? ";
          break;
        case BinaryOp::kWildcardNotEqual:
          op_str = " !=? ";
          break;
        case BinaryOp::kLessThan:
          op_str = " < ";
          break;
        case BinaryOp::kLessThanEqual:
          op_str = " <= ";
          break;
        case BinaryOp::kGreaterThan:
          op_str = " > ";
          break;
        case BinaryOp::kGreaterThanEqual:
          op_str = " >= ";
          break;
        case BinaryOp::kLogicalShiftLeft:
          op_str = " << ";
          break;
        case BinaryOp::kLogicalShiftRight:
          op_str = " >> ";
          break;
        case BinaryOp::kArithmeticShiftLeft:
          op_str = " <<< ";
          break;
        case BinaryOp::kArithmeticShiftRight:
          op_str = " >>> ";
          break;
      }
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

    case ExpressionKind::kSystemCall: {
      const auto& data = std::get<SystemCallExpressionData>(expr.data);
      const auto& display = std::get<DisplaySystemCallData>(data);
      const char* name = "$display";
      switch (display.radix) {
        case PrintRadix::kDecimal:
          name = display.append_newline ? "$display" : "$write";
          break;
        case PrintRadix::kBinary:
          name = display.append_newline ? "$displayb" : "$writeb";
          break;
        case PrintRadix::kOctal:
          name = display.append_newline ? "$displayo" : "$writeo";
          break;
        case PrintRadix::kHex:
          name = display.append_newline ? "$displayh" : "$writeh";
          break;
      }
      *out_ << name << "(";
      bool first = true;
      for (ExpressionId arg : display.args) {
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
