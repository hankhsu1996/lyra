#include "lyra/compiler/codegen.hpp"

#include <format>
#include <stdexcept>

#include "lyra/common/sv_format.hpp"
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
    case common::Type::Kind::kReal:
      return "Real";
    case common::Type::Kind::kString:
      return "std::string";
    case common::Type::Kind::kTwoState: {
      auto data = std::get<common::TwoStateData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      if (width > 64) {
        return "/* TODO: wide integer */";
      }

      // Use LRM-aligned type aliases for standard signed integer types
      if (is_signed) {
        if (width == 8) {
          return "Byte";
        }
        if (width == 16) {
          return "ShortInt";
        }
        if (width == 32) {
          return "Int";
        }
        if (width == 64) {
          return "LongInt";
        }
        // Non-standard signed widths use Bit<N, true>
        return std::format("Bit<{}, true>", width);
      }

      // Unsigned types use Bit<N>
      return std::format("Bit<{}>", width);
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
  if (width > 64) {
    return "/* TODO: wide integer */";
  }
  // Always return unsigned Bit<N> regardless of original signedness
  return std::format("Bit<{}>", width);
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
  Line("#include <print>");
  Line("#include <iostream>");
  Line("#include <lyra/sdk/sdk.hpp>");
  Line("");
}

void Codegen::EmitClass(const mir::Module& module) {
  Line("class " + module.name + " : public lyra::sdk::Module {");
  indent_++;

  // Type aliases for cleaner generated code
  Line("template <std::size_t N, bool S = false>");
  Line("using Bit = lyra::sdk::Bit<N, S>;");
  Line("using Int = lyra::sdk::Int;");
  Line("using LongInt = lyra::sdk::LongInt;");
  Line("using ShortInt = lyra::sdk::ShortInt;");
  Line("using Byte = lyra::sdk::Byte;");
  Line("using Real = double;");
  Line("");

  indent_--;
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

void Codegen::EmitVariables(const std::vector<mir::ModuleVariable>& variables) {
  for (const auto& mod_var : variables) {
    std::string type_str = ToCppType(mod_var.variable.type);
    Indent();
    out_ << type_str << " " << mod_var.variable.symbol->name;
    if (mod_var.initializer) {
      out_ << " = ";
      EmitExpression(*mod_var.initializer);
    } else {
      out_ << "{}";
    }
    out_ << ";\n";
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
        if (syscall.name == "$display") {
          // Empty $display - just print newline
          // Use std::cout to allow capture via rdbuf redirection
          if (syscall.arguments.empty()) {
            Line("std::println(std::cout, \"\");");
            break;
          }

          // Check if first arg is a format string (string literal with %)
          if (syscall.arguments[0]->kind == mir::Expression::Kind::kLiteral) {
            const auto& lit =
                mir::As<mir::LiteralExpression>(*syscall.arguments[0]);
            if (lit.literal.type.kind == common::Type::Kind::kString) {
              auto sv_fmt = lit.literal.value.AsString();
              if (sv_fmt.find('%') != std::string::npos) {
                // Transform SV format to std::println
                auto cpp_fmt = common::TransformToStdFormat(sv_fmt);
                Indent();
                out_ << "std::println(std::cout, \"" << cpp_fmt << "\"";
                for (size_t i = 1; i < syscall.arguments.size(); ++i) {
                  out_ << ", ";
                  EmitExpression(*syscall.arguments[i]);
                }
                out_ << ");\n";
                break;
              }
            }
          }

          // No format specifiers - generate format string with {} placeholders
          // No automatic spacing - matches C++ printf behavior
          std::string fmt_str;
          for (size_t i = 0; i < syscall.arguments.size(); ++i) {
            fmt_str += "{}";
          }

          Indent();
          out_ << "std::println(std::cout, \"" << fmt_str << "\"";
          for (size_t i = 0; i < syscall.arguments.size(); ++i) {
            out_ << ", ";
            EmitExpression(*syscall.arguments[i]);
          }
          out_ << ");\n";
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
        // Single trigger: co_await Posedge(&clk);
        const auto& trigger = wait.triggers[0];
        std::string var_name(trigger.variable->name);
        Line("co_await " + trigger_expr(var_name, trigger.edge_kind) + ";");
      } else {
        // Check if all triggers are AnyChange
        bool all_any_change = std::all_of(
            wait.triggers.begin(), wait.triggers.end(), [](const auto& t) {
              return t.edge_kind == common::EdgeKind::kAnyChange;
            });

        if (all_any_change) {
          // Optimized: co_await AnyChange(&a, &b, &c);
          Indent();
          out_ << "co_await lyra::sdk::AnyChange(";
          for (size_t i = 0; i < wait.triggers.size(); ++i) {
            if (i > 0) {
              out_ << ", ";
            }
            out_ << "&" << wait.triggers[i].variable->name;
          }
          out_ << ");\n";
        } else {
          // Mixed triggers: co_await AnyOf(Posedge(&clk), Negedge(&rst));
          Indent();
          out_ << "co_await lyra::sdk::AnyOf(";
          for (size_t i = 0; i < wait.triggers.size(); ++i) {
            if (i > 0) {
              out_ << ", ";
            }
            const auto& trigger = wait.triggers[i];
            std::string var_name(trigger.variable->name);
            out_ << trigger_expr(var_name, trigger.edge_kind);
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
      // Emit literals with their proper SDK type
      if (lit.literal.type.kind == common::Type::Kind::kTwoState) {
        // Emit as typed literal: Type{value}
        out_ << ToCppType(lit.literal.type) << "{" << lit.literal.ToString()
             << "}";
      } else {
        // Non-integral types (string, etc.)
        out_ << lit.literal.ToString();
      }
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
