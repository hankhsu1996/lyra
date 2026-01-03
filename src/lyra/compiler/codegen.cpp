#include "lyra/compiler/codegen.hpp"

#include <format>
#include <stdexcept>

#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::compiler {

namespace {

// C++ operator precedence (higher value = binds tighter)
// See: https://en.cppreference.com/w/cpp/language/operator_precedence
constexpr int kPrecLowest = 0;           // Top level (no parent)
constexpr int kPrecAssign = 1;           // = += -= etc.
constexpr int kPrecTernary = 2;          // ?:
constexpr int kPrecLogicalOr = 3;        // ||
constexpr int kPrecLogicalAnd = 4;       // &&
constexpr int kPrecBitwiseOr = 5;        // |
constexpr int kPrecBitwiseXor = 6;       // ^
constexpr int kPrecBitwiseAnd = 7;       // &
constexpr int kPrecEquality = 8;         // == !=
constexpr int kPrecRelational = 9;       // < <= > >=
constexpr int kPrecShift = 10;           // << >>
constexpr int kPrecAdditive = 11;        // + -
constexpr int kPrecMultiplicative = 12;  // * / %
constexpr int kPrecUnary = 13;           // ! ~ - + ++ --
constexpr int kPrecPrimary = 14;         // Literals, identifiers, calls

auto GetBinaryPrecedence(mir::BinaryOperator op) -> int {
  switch (op) {
    case mir::BinaryOperator::kLogicalOr:
      return kPrecLogicalOr;
    case mir::BinaryOperator::kLogicalAnd:
      return kPrecLogicalAnd;
    case mir::BinaryOperator::kLogicalImplication:
    case mir::BinaryOperator::kLogicalEquivalence:
      return kPrecLogicalOr;  // Treat like ||
    case mir::BinaryOperator::kBitwiseOr:
      return kPrecBitwiseOr;
    case mir::BinaryOperator::kBitwiseXor:
    case mir::BinaryOperator::kBitwiseXnor:
      return kPrecBitwiseXor;
    case mir::BinaryOperator::kBitwiseAnd:
      return kPrecBitwiseAnd;
    case mir::BinaryOperator::kEquality:
    case mir::BinaryOperator::kInequality:
    case mir::BinaryOperator::kCaseEquality:
    case mir::BinaryOperator::kCaseInequality:
    case mir::BinaryOperator::kWildcardEquality:
    case mir::BinaryOperator::kWildcardInequality:
      return kPrecEquality;
    case mir::BinaryOperator::kGreaterThan:
    case mir::BinaryOperator::kGreaterThanEqual:
    case mir::BinaryOperator::kLessThan:
    case mir::BinaryOperator::kLessThanEqual:
      return kPrecRelational;
    case mir::BinaryOperator::kLogicalShiftLeft:
    case mir::BinaryOperator::kLogicalShiftRight:
    case mir::BinaryOperator::kArithmeticShiftLeft:
    case mir::BinaryOperator::kArithmeticShiftRight:
      return kPrecShift;
    case mir::BinaryOperator::kAddition:
    case mir::BinaryOperator::kSubtraction:
      return kPrecAdditive;
    case mir::BinaryOperator::kMultiplication:
    case mir::BinaryOperator::kDivision:
    case mir::BinaryOperator::kModulo:
    case mir::BinaryOperator::kPower:
      return kPrecMultiplicative;
  }
  return kPrecPrimary;
}

auto IsSigned(const common::Type& type) -> bool {
  if (type.kind != common::Type::Kind::kTwoState) {
    return false;
  }
  return std::get<common::TwoStateData>(type.data).is_signed;
}

auto ToCppType(const common::Type& type) -> std::string {
  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return "void";
    case common::Type::Kind::kString:
      return "std::string";
    case common::Type::Kind::kTwoState: {
      auto data = std::get<common::TwoStateData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;
      // TODO(hankhsu): Add signed Bit type support
      if (!is_signed && width <= 64) {
        return std::format("lyra::sdk::Bit<{}>", width);
      }
      // Fallback to primitive types for signed values
      if (width == 1) {
        return "bool";
      }
      if (width <= 8) {
        return is_signed ? "int8_t" : "uint8_t";
      }
      if (width <= 16) {
        return is_signed ? "int16_t" : "uint16_t";
      }
      if (width <= 32) {
        return is_signed ? "int32_t" : "uint32_t";
      }
      if (width <= 64) {
        return is_signed ? "int64_t" : "uint64_t";
      }
      return "/* TODO: wide integer */";
    }
  }
  return "/* unknown type */";
}

auto ToCppUnsignedType(const common::Type& type) -> std::string {
  if (type.kind != common::Type::Kind::kTwoState) {
    return "/* unknown type */";
  }
  auto data = std::get<common::TwoStateData>(type.data);
  size_t width = data.bit_width;
  if (width == 1) {
    return "bool";
  }
  if (width <= 8) {
    return "uint8_t";
  }
  if (width <= 16) {
    return "uint16_t";
  }
  if (width <= 32) {
    return "uint32_t";
  }
  if (width <= 64) {
    return "uint64_t";
  }
  return "/* TODO: wide integer */";
}

auto ToCppOperator(mir::BinaryOperator op) -> const char* {
  switch (op) {
    case mir::BinaryOperator::kAddition:
      return "+";
    case mir::BinaryOperator::kSubtraction:
      return "-";
    case mir::BinaryOperator::kMultiplication:
      return "*";
    case mir::BinaryOperator::kDivision:
      return "/";
    case mir::BinaryOperator::kModulo:
      return "%";
    case mir::BinaryOperator::kPower:
      return "/* power */";  // C++ doesn't have ** operator

    case mir::BinaryOperator::kBitwiseAnd:
      return "&";
    case mir::BinaryOperator::kBitwiseOr:
      return "|";
    case mir::BinaryOperator::kBitwiseXor:
      return "^";
    case mir::BinaryOperator::kBitwiseXnor:
      return "/* xnor */";  // C++ doesn't have xnor operator

    case mir::BinaryOperator::kLogicalAnd:
      return "&&";
    case mir::BinaryOperator::kLogicalOr:
      return "||";
    case mir::BinaryOperator::kLogicalImplication:
      return "/* implication */";
    case mir::BinaryOperator::kLogicalEquivalence:
      return "/* equivalence */";

    case mir::BinaryOperator::kEquality:
      return "==";
    case mir::BinaryOperator::kInequality:
      return "!=";
    case mir::BinaryOperator::kCaseEquality:
      return "==";  // In C++, just use ==
    case mir::BinaryOperator::kCaseInequality:
      return "!=";
    case mir::BinaryOperator::kWildcardEquality:
      return "/* wildcard eq */";
    case mir::BinaryOperator::kWildcardInequality:
      return "/* wildcard neq */";
    case mir::BinaryOperator::kGreaterThan:
      return ">";
    case mir::BinaryOperator::kGreaterThanEqual:
      return ">=";
    case mir::BinaryOperator::kLessThan:
      return "<";
    case mir::BinaryOperator::kLessThanEqual:
      return "<=";

    case mir::BinaryOperator::kLogicalShiftLeft:
      return "<<";
    case mir::BinaryOperator::kLogicalShiftRight:
      return ">>";
    case mir::BinaryOperator::kArithmeticShiftLeft:
      return "<<";
    case mir::BinaryOperator::kArithmeticShiftRight:
      return ">>";
  }
  return "/* unknown */";
}

}  // namespace

auto Codegen::Generate(const mir::Module& module) -> std::string {
  out_.str("");
  indent_ = 0;

  EmitHeader();
  EmitClass(module);

  return out_.str();
}

void Codegen::EmitHeader() {
  Line("#include <lyra/sdk/sdk.hpp>");
  Line("");
}

void Codegen::EmitClass(const mir::Module& module) {
  Line("class " + module.name + " : public lyra::sdk::Module {");
  Line(" public:");
  indent_++;

  // Constructor
  Line(module.name + "() : Module(\"" + module.name + "\") {");
  indent_++;
  for (const auto& process : module.processes) {
    Line("RegisterProcess(&" + module.name + "::" + process->name + ");");
  }
  indent_--;
  Line("}");
  Line("");

  // Process methods
  for (const auto& process : module.processes) {
    EmitProcess(*process);
  }

  Line("");
  // Member variables
  EmitVariables(module.variables);
  indent_--;
  Line("};");
}

void Codegen::EmitVariables(const std::vector<common::Variable>& variables) {
  for (const auto& var : variables) {
    std::string type_str = ToCppType(var.type);
    Line(type_str + " " + std::string(var.symbol->name) + "{};");
  }
}

void Codegen::EmitProcess(const mir::Process& process) {
  Line("auto " + process.name + "() -> lyra::sdk::Task {");
  indent_++;
  if (process.body) {
    EmitStatement(*process.body);
  }
  Line("co_return;");
  indent_--;
  Line("}");
  Line("");
}

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
      out_ << assign.target->name << " = ";
      EmitExpression(*assign.value);
      out_ << ";\n";
      break;
    }
    case mir::Statement::Kind::kExpression: {
      const auto& expr_stmt = mir::As<mir::ExpressionStatement>(stmt);
      // Handle system calls specially
      if (expr_stmt.expression->kind == mir::Expression::Kind::kSystemCall) {
        const auto& syscall =
            mir::As<mir::SystemCallExpression>(*expr_stmt.expression);
        if (syscall.name == "$finish") {
          Line("co_await lyra::sdk::Finish();");
          break;
        }
        throw std::runtime_error(
            "C++ codegen: unsupported system call: " + syscall.name);
      }
      Indent();
      EmitExpression(*expr_stmt.expression);
      out_ << ";\n";
      break;
    }
    case mir::Statement::Kind::kConditional: {
      const auto& cond = mir::As<mir::ConditionalStatement>(stmt);
      EmitConditional(cond, false);
      break;
    }
    case mir::Statement::Kind::kWhile: {
      const auto& while_stmt = mir::As<mir::WhileStatement>(stmt);
      Indent();
      out_ << "while (";
      EmitExpression(*while_stmt.condition);
      out_ << ") {\n";
      indent_++;
      EmitStatement(*while_stmt.body);
      indent_--;
      Indent();
      out_ << "}\n";
      break;
    }
    case mir::Statement::Kind::kDoWhile: {
      const auto& do_while = mir::As<mir::DoWhileStatement>(stmt);
      Indent();
      out_ << "do {\n";
      indent_++;
      EmitStatement(*do_while.body);
      indent_--;
      Indent();
      out_ << "} while (";
      EmitExpression(*do_while.condition);
      out_ << ");\n";
      break;
    }
    case mir::Statement::Kind::kFor: {
      const auto& for_stmt = mir::As<mir::ForStatement>(stmt);
      // Emit initializers (variable declarations must be outside for loop in
      // C++)
      for (const auto& init : for_stmt.initializers) {
        EmitStatement(*init);
      }
      Indent();
      out_ << "for (; ";
      if (for_stmt.condition) {
        EmitExpression(*for_stmt.condition);
      }
      out_ << "; ";
      for (size_t i = 0; i < for_stmt.steps.size(); ++i) {
        if (i > 0) {
          out_ << ", ";
        }
        EmitExpression(*for_stmt.steps[i]);
      }
      out_ << ") {\n";
      indent_++;
      EmitStatement(*for_stmt.body);
      indent_--;
      Indent();
      out_ << "}\n";
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
      Line(
          "co_await lyra::sdk::Delay(" + std::to_string(delay.delay_amount) +
          ");");
      break;
    }
    case mir::Statement::Kind::kWaitEvent: {
      const auto& wait = mir::As<mir::WaitEventStatement>(stmt);
      if (wait.triggers.empty()) {
        break;
      }

      auto edge_kind_str = [](common::EdgeKind kind) -> std::string {
        switch (kind) {
          case common::EdgeKind::kPosedge:
            return "lyra::sdk::EdgeKind::kPosedge";
          case common::EdgeKind::kNegedge:
            return "lyra::sdk::EdgeKind::kNegedge";
          case common::EdgeKind::kAnyChange:
            return "lyra::sdk::EdgeKind::kAnyChange";
          case common::EdgeKind::kBothEdge:
            return "lyra::sdk::EdgeKind::kBothEdge";
        }
        return "lyra::sdk::EdgeKind::kAnyChange";
      };

      if (wait.triggers.size() == 1) {
        // Single trigger - use simple ImplicitEvent
        const auto& trigger = wait.triggers[0];
        std::string var_name(trigger.variable->name);
        Line(
            "co_await lyra::sdk::ImplicitEvent(&" + var_name + ", " +
            edge_kind_str(trigger.edge_kind) + ");");
      } else {
        // Multiple triggers (OR semantics) - use ImplicitEventOr
        Indent();
        out_ << "co_await lyra::sdk::ImplicitEventOr({\n";
        indent_++;
        for (size_t i = 0; i < wait.triggers.size(); ++i) {
          const auto& trigger = wait.triggers[i];
          std::string var_name(trigger.variable->name);
          Indent();
          out_ << "{lyra::sdk::MakeTriggerChecker(&" << var_name << ", "
               << edge_kind_str(trigger.edge_kind) << ")}";
          if (i + 1 < wait.triggers.size()) {
            out_ << ",";
          }
          out_ << "\n";
        }
        indent_--;
        Indent();
        out_ << "});\n";
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
    default:
      throw std::runtime_error(
          "C++ codegen: unimplemented statement kind: " + ToString(stmt.kind));
  }
}

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

void Codegen::EmitExpression(const mir::Expression& expr, int parent_prec) {
  switch (expr.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& lit = mir::As<mir::LiteralExpression>(expr);
      // Emit bool literals as true/false for 1-bit unsigned types
      if (lit.literal.type.kind == common::Type::Kind::kTwoState) {
        auto data = std::get<common::TwoStateData>(lit.literal.type.data);
        if (data.bit_width == 1 && !data.is_signed &&
            lit.literal.value.IsInt64()) {
          out_ << (lit.literal.value.AsInt64() != 0 ? "true" : "false");
          break;
        }
      }
      out_ << lit.literal.ToString();
      break;
    }
    case mir::Expression::Kind::kIdentifier: {
      const auto& ident = mir::As<mir::IdentifierExpression>(expr);
      out_ << ident.symbol->name;
      break;
    }
    case mir::Expression::Kind::kBinary: {
      const auto& bin = mir::As<mir::BinaryExpression>(expr);
      if (bin.op == mir::BinaryOperator::kBitwiseXnor) {
        // XNOR: ~(a ^ b) - uses function-like syntax, always parens
        out_ << "~(";
        EmitExpression(*bin.left, kPrecBitwiseXor);
        out_ << " ^ ";
        EmitExpression(*bin.right, kPrecBitwiseXor);
        out_ << ")";
      } else if (
          bin.op == mir::BinaryOperator::kLogicalShiftRight &&
          IsSigned(bin.left->type)) {
        // Logical right shift on signed: cast to unsigned, shift, cast back
        out_ << "static_cast<" << ToCppType(bin.left->type) << ">(";
        out_ << "static_cast<" << ToCppUnsignedType(bin.left->type) << ">(";
        EmitExpression(*bin.left, kPrecLowest);
        out_ << ") >> ";
        EmitExpression(*bin.right, kPrecShift);
        out_ << ")";
      } else {
        int prec = GetBinaryPrecedence(bin.op);
        bool needs_parens = prec < parent_prec;
        if (needs_parens) {
          out_ << "(";
        }
        EmitExpression(*bin.left, prec);
        out_ << " " << ToCppOperator(bin.op) << " ";
        // Right operand needs higher prec for left-associativity
        EmitExpression(*bin.right, prec + 1);
        if (needs_parens) {
          out_ << ")";
        }
      }
      break;
    }
    case mir::Expression::Kind::kAssignment: {
      const auto& assign = mir::As<mir::AssignmentExpression>(expr);
      if (assign.is_non_blocking) {
        out_ << "this->ScheduleNba(&" << assign.target->name << ", ";
        EmitExpression(*assign.value, kPrecLowest);
        out_ << ")";
      } else {
        out_ << assign.target->name << " = ";
        EmitExpression(*assign.value, kPrecAssign);
      }
      break;
    }
    case mir::Expression::Kind::kTernary: {
      const auto& ternary = mir::As<mir::TernaryExpression>(expr);
      bool needs_parens = kPrecTernary < parent_prec;
      if (needs_parens) {
        out_ << "(";
      }
      EmitExpression(*ternary.condition, kPrecTernary);
      out_ << " ? ";
      EmitExpression(*ternary.true_expression, kPrecTernary);
      out_ << " : ";
      EmitExpression(*ternary.false_expression, kPrecTernary);
      if (needs_parens) {
        out_ << ")";
      }
      break;
    }
    case mir::Expression::Kind::kUnary: {
      const auto& unary = mir::As<mir::UnaryExpression>(expr);
      bool needs_parens = kPrecUnary < parent_prec;
      if (needs_parens) {
        out_ << "(";
      }
      switch (unary.op) {
        case mir::UnaryOperator::kPlus:
          out_ << "+";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kMinus:
          out_ << "-";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kLogicalNot:
          out_ << "!";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kBitwiseNot:
          out_ << "~";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kPreincrement:
          out_ << "++";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kPredecrement:
          out_ << "--";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kPostincrement:
          EmitExpression(*unary.operand, kPrecUnary);
          out_ << "++";
          break;
        case mir::UnaryOperator::kPostdecrement:
          EmitExpression(*unary.operand, kPrecUnary);
          out_ << "--";
          break;
        case mir::UnaryOperator::kReductionAnd:
          // 1 if all bits are 1: !~a (branchless)
          out_ << "!~";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kReductionNand:
          // 1 if not all bits are 1: !!~a (branchless)
          out_ << "!!~";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kReductionOr:
          // 1 if any bit is 1: !!a (branchless)
          out_ << "!!";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kReductionNor:
          // 1 if no bits are 1: !a (branchless)
          out_ << "!";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
        case mir::UnaryOperator::kReductionXor:
          // 1 if odd number of 1-bits: __builtin_parityll
          out_ << "__builtin_parityll(static_cast<uint64_t>(static_cast<"
               << ToCppUnsignedType(unary.operand->type) << ">(";
          EmitExpression(*unary.operand, kPrecLowest);
          out_ << ")))";
          break;
        case mir::UnaryOperator::kReductionXnor:
          // 1 if even number of 1-bits: !__builtin_parityll
          out_ << "!__builtin_parityll(static_cast<uint64_t>(static_cast<"
               << ToCppUnsignedType(unary.operand->type) << ">(";
          EmitExpression(*unary.operand, kPrecLowest);
          out_ << ")))";
          break;
        default:
          out_ << "/* TODO: " << ToString(unary.op) << " */";
          EmitExpression(*unary.operand, kPrecUnary);
          break;
      }
      if (needs_parens) {
        out_ << ")";
      }
      break;
    }
    case mir::Expression::Kind::kConversion: {
      const auto& conv = mir::As<mir::ConversionExpression>(expr);
      out_ << "static_cast<" << ToCppType(conv.target_type) << ">(";
      EmitExpression(*conv.value, kPrecLowest);
      out_ << ")";
      break;
    }
    default:
      throw std::runtime_error(
          "C++ codegen: unimplemented expression kind: " + ToString(expr.kind));
  }
}

void Codegen::Indent() {
  out_ << std::string(indent_ * 2, ' ');
}

void Codegen::Line(const std::string& text) {
  if (!text.empty()) {
    Indent();
  }
  out_ << text << "\n";
}

}  // namespace lyra::compiler
