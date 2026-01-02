#include "lyra/codegen/cpp_codegen.hpp"

#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::codegen {

namespace {

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

auto CppCodegen::Generate(const mir::Module& module) -> std::string {
  out_.str("");
  indent_ = 0;

  EmitHeader();
  EmitClass(module);

  return out_.str();
}

void CppCodegen::EmitHeader() {
  Line("#include <lyra/sdk/sdk.hpp>");
  Line("");
  Line("using namespace lyra::sdk;");
  Line("");
}

void CppCodegen::EmitClass(const mir::Module& module) {
  Line("class " + module.name + " : public Module {");
  Line(" public:");
  indent_++;

  // Constructor
  Line(module.name + "() : Module(\"" + module.name + "\") {");
  indent_++;
  for (size_t i = 0; i < module.processes.size(); ++i) {
    const auto& process = module.processes[i];
    if (process->process_kind == mir::ProcessKind::kInitial) {
      Line("RegisterInitial(&" + module.name + "::" + process->name + ");");
    }
  }
  indent_--;
  Line("}");
  Line("");

  // Process methods
  for (const auto& process : module.processes) {
    EmitProcess(*process);
  }

  indent_--;
  Line("");
  Line(" public:");
  indent_++;
  EmitVariables(module.variables);
  indent_--;
  Line("};");
}

void CppCodegen::EmitVariables(const std::vector<common::Variable>& variables) {
  for (const auto& var : variables) {
    std::string type_str = "int";  // TODO: proper type mapping
    Line(type_str + " " + std::string(var.symbol->name) + " = 0;");
  }
}

void CppCodegen::EmitProcess(const mir::Process& process) {
  Line("auto " + process.name + "() -> Task {");
  indent_++;
  if (process.body) {
    EmitStatement(*process.body);
  }
  Line("co_return;");
  indent_--;
  Line("}");
  Line("");
}

void CppCodegen::EmitStatement(const mir::Statement& stmt) {
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
      Indent();
      EmitExpression(*expr_stmt.expression);
      out_ << ";\n";
      break;
    }
    default:
      Line("// TODO: " + ToString(stmt.kind));
      break;
  }
}

void CppCodegen::EmitExpression(const mir::Expression& expr) {
  switch (expr.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& lit = mir::As<mir::LiteralExpression>(expr);
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
      out_ << "(";
      EmitExpression(*bin.left);
      out_ << " " << ToCppOperator(bin.op) << " ";
      EmitExpression(*bin.right);
      out_ << ")";
      break;
    }
    case mir::Expression::Kind::kAssignment: {
      const auto& assign = mir::As<mir::AssignmentExpression>(expr);
      out_ << assign.target->name << " = ";
      EmitExpression(*assign.value);
      break;
    }
    default:
      out_ << "/* TODO: " << ToString(expr.kind) << " */";
      break;
  }
}

void CppCodegen::Indent() {
  out_ << std::string(indent_ * 2, ' ');
}

void CppCodegen::Line(const std::string& text) {
  Indent();
  out_ << text << "\n";
}

}  // namespace lyra::codegen
