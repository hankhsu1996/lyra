#include "lyra/compiler/codegen.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <ios>
#include <string>
#include <unordered_set>
#include <vector>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/sv_format.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operators.hpp"
#include "lyra/mir/process.hpp"
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

// Check if a module uses any array types (requiring <array> include)
auto UsesArrayType(const mir::Module& module) -> bool {
  auto is_array = [](const auto& item) {
    return item.variable.type.kind == common::Type::Kind::kUnpackedArray;
  };
  return std::ranges::any_of(module.variables, is_array) ||
         std::ranges::any_of(module.ports, is_array);
}

}  // namespace

auto Codegen::IsSigned(const common::Type& type) -> bool {
  if (type.kind != common::Type::Kind::kIntegral) {
    return false;
  }
  return std::get<common::IntegralData>(type.data).is_signed;
}

auto Codegen::ToCppType(const common::Type& type) -> std::string {
  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return "void";
    case common::Type::Kind::kReal:
      used_type_aliases_ |= TypeAlias::kReal;
      return "Real";
    case common::Type::Kind::kShortReal:
      used_type_aliases_ |= TypeAlias::kShortReal;
      return "ShortReal";
    case common::Type::Kind::kString:
      return "std::string";
    case common::Type::Kind::kIntegral: {
      auto data = std::get<common::IntegralData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      if (width > 64) {
        return "/* TODO: wide integer */";
      }

      // Use LRM-aligned type aliases for standard signed integer types
      if (is_signed) {
        if (width == 8) {
          used_type_aliases_ |= TypeAlias::kByte;
          return "Byte";
        }
        if (width == 16) {
          used_type_aliases_ |= TypeAlias::kShortInt;
          return "ShortInt";
        }
        if (width == 32) {
          used_type_aliases_ |= TypeAlias::kInt;
          return "Int";
        }
        if (width == 64) {
          used_type_aliases_ |= TypeAlias::kLongInt;
          return "LongInt";
        }
        // Non-standard signed widths use Bit<N, true>
        used_type_aliases_ |= TypeAlias::kBit;
        return std::format("Bit<{}, true>", width);
      }

      // Unsigned types use Bit<N>
      used_type_aliases_ |= TypeAlias::kBit;
      return std::format("Bit<{}>", width);
    }
    case common::Type::Kind::kUnpackedArray: {
      const auto& array_data = std::get<common::UnpackedArrayData>(type.data);
      return std::format(
          "std::array<{}, {}>", ToCppType(*array_data.element_type),
          array_data.size);
    }
  }
  return "/* unknown type */";
}

auto Codegen::ToCppUnsignedType(const common::Type& type) -> std::string {
  if (type.kind != common::Type::Kind::kIntegral) {
    return "/* unknown type */";
  }
  auto data = std::get<common::IntegralData>(type.data);
  size_t width = data.bit_width;
  if (width > 64) {
    return "/* TODO: wide integer */";
  }
  // Always return unsigned Bit<N> regardless of original signedness
  used_type_aliases_ |= TypeAlias::kBit;
  return std::format("Bit<{}>", width);
}

auto Codegen::Generate(const mir::Module& module) -> std::string {
  out_.str("");
  indent_ = 0;
  port_symbols_.clear();
  used_type_aliases_ = TypeAlias::kNone;
  used_features_ = CodegenFeature::kNone;

  // Store timescale info for delay scaling
  timescale_ = module.timescale;
  if (module.timescale) {
    global_precision_power_ = module.timescale->precision_power;
  } else {
    global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;
  }

  // Populate port symbols for identifier emission (output/inout ports only)
  // Input ports are public variables without underscore suffix
  for (const auto& port : module.ports) {
    if (port.direction != mir::PortDirection::kInput) {
      port_symbols_.insert(port.variable.symbol);
    }
  }

  // Two-phase generation: generate class first to collect feature usage,
  // then emit header with conditional includes, then append class.
  std::ostringstream class_out;
  class_out.swap(out_);
  EmitClass(module);
  std::string class_content = out_.str();
  out_.swap(class_out);  // Restore empty output stream

  // Now emit header (we know used_features_ from EmitClass)
  EmitHeader(module.submodules, UsesArrayType(module));

  // Append class content
  out_ << class_content;

  return out_.str();
}

auto Codegen::DelayMultiplier() const -> uint64_t {
  if (timescale_) {
    return timescale_->DelayMultiplier(global_precision_power_);
  }
  return common::TimeScale::Default().DelayMultiplier(global_precision_power_);
}

void Codegen::EmitHeader(
    const std::vector<mir::SubmoduleInstance>& submodules, bool uses_arrays) {
  if (uses_arrays) {
    Line("#include <array>");
  }
  if ((used_features_ & CodegenFeature::kCmath) != CodegenFeature::kNone) {
    Line("#include <cmath>");
  }
  Line("#include <iostream>");
  Line("#include <print>");
  Line("#include <lyra/sdk/sdk.hpp>");

  // Include headers for submodule types
  std::unordered_set<std::string> included;
  for (const auto& submod : submodules) {
    if (!included.contains(submod.module_type)) {
      Line("#include \"" + submod.module_type + ".hpp\"");
      included.insert(submod.module_type);
    }
  }

  Line("");
}

void Codegen::EmitTypeAliases() {
  // Only emit type aliases that were actually used
  if ((used_type_aliases_ & TypeAlias::kBit) != TypeAlias::kNone) {
    Line("template <std::size_t N, bool S = false>");
    Line("using Bit = lyra::sdk::Bit<N, S>;");
  }
  if ((used_type_aliases_ & TypeAlias::kInt) != TypeAlias::kNone) {
    Line("using Int = lyra::sdk::Int;");
  }
  if ((used_type_aliases_ & TypeAlias::kLongInt) != TypeAlias::kNone) {
    Line("using LongInt = lyra::sdk::LongInt;");
  }
  if ((used_type_aliases_ & TypeAlias::kShortInt) != TypeAlias::kNone) {
    Line("using ShortInt = lyra::sdk::ShortInt;");
  }
  if ((used_type_aliases_ & TypeAlias::kByte) != TypeAlias::kNone) {
    Line("using Byte = lyra::sdk::Byte;");
  }
  if ((used_type_aliases_ & TypeAlias::kReal) != TypeAlias::kNone) {
    Line("using Real = double;");
  }
  if ((used_type_aliases_ & TypeAlias::kShortReal) != TypeAlias::kNone) {
    Line("using ShortReal = float;");
  }
  if (used_type_aliases_ != TypeAlias::kNone) {
    Line("");
  }
}

void Codegen::EmitTimescaleConstants(const mir::Module& module) {
  // Only emit timescale constants that were actually used
  bool any_emitted = false;

  if ((used_features_ & CodegenFeature::kTimeDivisor) !=
      CodegenFeature::kNone) {
    Line(
        "static constexpr uint64_t kTimeDivisor = " +
        std::to_string(DelayMultiplier()) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModuleUnitPower) !=
      CodegenFeature::kNone) {
    int8_t module_unit_power = timescale_
                                   ? timescale_->unit_power
                                   : common::TimeScale::kDefaultUnitPower;
    Line(
        "static constexpr int8_t kModuleUnitPower = " +
        std::to_string(static_cast<int>(module_unit_power)) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModulePrecisionPower) !=
      CodegenFeature::kNone) {
    int8_t module_precision_power =
        timescale_ ? timescale_->precision_power
                   : common::TimeScale::kDefaultPrecisionPower;
    Line(
        "static constexpr int8_t kModulePrecisionPower = " +
        std::to_string(static_cast<int>(module_precision_power)) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModuleName) != CodegenFeature::kNone) {
    Line(
        "static constexpr std::string_view kModuleName = \"" + module.name +
        "\";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kTimescaleStr) !=
      CodegenFeature::kNone) {
    auto ts = timescale_.value_or(common::TimeScale::Default());
    Line(
        "static constexpr std::string_view kTimescaleStr = \"" + ts.ToString() +
        "\";");
    any_emitted = true;
  }

  if (any_emitted) {
    Line("");
  }
}

void Codegen::EmitClass(const mir::Module& module) {
  // Two-phase generation: first generate body to collect type alias and feature
  // usage, then emit class with only the aliases/constants that were used.

  // Phase 1: Save current output stream and generate body to a temporary buffer
  std::ostringstream saved_out;
  saved_out.swap(out_);
  int saved_indent = indent_;
  indent_ = 1;  // Body is indented inside class

  // Generate body content (this populates used_type_aliases_ and
  // used_features_)
  indent_--;
  Line(" public:");
  indent_++;

  // Input port member variables (public, for parent to drive)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      std::string type_str = ToCppType(port.variable.type);
      Line(type_str + " " + std::string(port.variable.symbol->name) + ";");
    }
  }

  // Constructor with port parameters (output/inout ports only)
  Indent();
  out_ << module.name << "(";

  // Generate port parameters (output/inout ports only)
  bool first = true;
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not constructor parameters
    }
    if (!first) {
      out_ << ", ";
    }
    first = false;
    std::string type_str = ToCppType(port.variable.type);
    out_ << type_str << "& " << port.variable.symbol->name;
  }

  out_ << ")\n";
  indent_++;
  Indent();
  out_ << ": Module(\"" << module.name << "\")";

  // Port initializers (output/inout ports only)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not reference members
    }
    out_ << ", " << port.variable.symbol->name << "_("
         << port.variable.symbol->name << ")";
  }

  // Submodule initializers - pass output port bindings as references
  for (const auto& submod : module.submodules) {
    out_ << ", " << submod.instance_name << "_(";
    bool first = true;
    for (const auto& binding : submod.output_bindings) {
      if (!first) {
        out_ << ", ";
      }
      first = false;
      EmitExpression(*binding.signal);
    }
    out_ << ")";
  }

  out_ << " {\n";
  indent_--;
  indent_++;
  for (const auto& process : module.processes) {
    Line("RegisterProcess(&" + module.name + "::" + process->name + ");");
  }
  // Register child modules for hierarchical execution
  for (const auto& submod : module.submodules) {
    Line("RegisterChild(&" + submod.instance_name + "_);");
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

  // Output/inout port reference members (must be before submodules for
  // initialization order - C++ initializes members in declaration order)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports declared as public variables above
    }
    std::string type_str = ToCppType(port.variable.type);
    Line(type_str + "& " + std::string(port.variable.symbol->name) + "_;");
  }

  // Submodule members (after port references so they can use them)
  for (const auto& submod : module.submodules) {
    Line(submod.module_type + " " + submod.instance_name + "_;");
  }

  // Phase 2: Now emit the class with conditional type aliases, then append body
  std::string body = out_.str();
  out_.swap(saved_out);  // Restore original output stream
  indent_ = saved_indent;

  // Emit class declaration
  Line("class " + module.name + " : public lyra::sdk::Module {");
  indent_++;

  // Emit only the type aliases that were actually used
  EmitTypeAliases();

  // Emit only the timescale constants that were actually used
  EmitTimescaleConstants(module);

  // Append the body content
  out_ << body;

  // Close the class
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

      if (assign.target.IsPacked() && assign.target.IsElementSelect()) {
        // Packed element assignment: vec[idx] = val
        // Generate: vec = (vec & ~(1ULL << adj_idx)) | ((val & 1ULL) <<
        // adj_idx)
        const auto& base_type = assign.target.base_type.value();
        size_t total_width = base_type.GetBitWidth();
        int32_t lower_bound = base_type.GetElementLower();
        size_t element_width = base_type.GetElementWidth();

        used_type_aliases_ |= TypeAlias::kBit;
        uint64_t mask = (1ULL << element_width) - 1;
        out_ << assign.target.symbol->name << " = ("
             << assign.target.symbol->name << " & ~(Bit<" << total_width << ">{"
             << mask << "ULL} << ";
        EmitPackedBitPosition(
            *assign.target.element_index, lower_bound, element_width);
        out_ << ")) | ((Bit<" << total_width << ">{";
        EmitExpression(*assign.value);
        out_ << ".Value() & " << mask << "ULL} << ";
        EmitPackedBitPosition(
            *assign.target.element_index, lower_bound, element_width);
        out_ << "));\n";
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
      // Handle system calls specially
      if (expr_stmt.expression->kind == mir::Expression::Kind::kSystemCall) {
        const auto& syscall =
            mir::As<mir::SystemCallExpression>(*expr_stmt.expression);

        // Simulation control tasks: $finish, $stop, $exit
        if (syscall.name == "$finish" || syscall.name == "$stop" ||
            syscall.name == "$exit") {
          // Get level argument (default 1), $exit has no argument
          std::string level_str = "1";
          if (!syscall.arguments.empty()) {
            if (const auto* lit = dynamic_cast<const mir::LiteralExpression*>(
                    syscall.arguments[0].get())) {
              level_str = std::to_string(lit->literal.value.AsInt64());
            }
          }

          if (syscall.name == "$stop") {
            Line("co_await lyra::sdk::Stop(" + level_str + ");");
          } else if (syscall.name == "$exit") {
            // $exit uses Finish with name "$exit" (exit code 0)
            // Note: Per LRM, $exit waits for program blocks - not implemented
            Line("co_await lyra::sdk::Finish(" + level_str + ", \"$exit\");");
          } else {
            // $finish (exit code 0)
            Line("co_await lyra::sdk::Finish(" + level_str + ");");
          }
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
                auto specs = common::ParseDisplayFormat(sv_fmt);
                auto needs_cast = common::NeedsIntCast(sv_fmt);
                Indent();
                out_ << "std::println(std::cout, \"" << cpp_fmt << "\"";

                // Emit arguments - for %t, wrap with FormatTimeValue
                for (size_t i = 0; i < specs.size(); ++i) {
                  out_ << ", ";
                  if (specs[i].spec == 't') {
                    // %t - format the time argument using $timeformat settings
                    // Use .Value() to extract uint64_t from Bit<64>
                    used_features_ |= CodegenFeature::kModuleUnitPower;
                    out_ << "lyra::sdk::FormatTimeValue(";
                    EmitExpression(*syscall.arguments[i + 1]);
                    out_ << ".Value(), kModuleUnitPower)";
                  } else if (i < needs_cast.size() && needs_cast[i]) {
                    // Cast to int64_t if format spec has width
                    out_ << "static_cast<int64_t>(";
                    EmitExpression(*syscall.arguments[i + 1]);
                    out_ << ")";
                  } else {
                    EmitExpression(*syscall.arguments[i + 1]);
                  }
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
          for (const auto& arg : syscall.arguments) {
            out_ << ", ";
            EmitExpression(*arg);
          }
          out_ << ");\n";
          break;
        }
        if (syscall.name == "$timeformat") {
          // $timeformat(units, precision, suffix, min_width)
          // Cast Bit types to native types since Bit only has implicit bool
          Indent();
          out_ << "lyra::sdk::TimeFormat(";
          for (size_t i = 0; i < syscall.arguments.size(); ++i) {
            if (i > 0) {
              out_ << ", ";
            }
            if (i == 0) {
              // units: int8_t
              out_ << "static_cast<int8_t>(";
              EmitExpression(*syscall.arguments[i]);
              out_ << ")";
            } else if (i == 1 || i == 3) {
              // precision/min_width: int
              out_ << "static_cast<int>(";
              EmitExpression(*syscall.arguments[i]);
              out_ << ")";
            } else {
              // suffix: string (no cast needed)
              EmitExpression(*syscall.arguments[i]);
            }
          }
          out_ << ");\n";
          break;
        }
        if (syscall.name == "$printtimescale") {
          // $printtimescale() - print current module's timescale
          used_features_ |= CodegenFeature::kModuleName;
          used_features_ |= CodegenFeature::kTimescaleStr;
          Line(
              "std::println(std::cout, \"Time scale of ({}) is {}\", "
              "kModuleName, kTimescaleStr);");
          break;
        }
        if (syscall.name == "$printtimescale_root") {
          // $printtimescale($root) - print global precision (unit = precision)
          Indent();
          out_ << "std::println(std::cout, \"Time scale of ($root) is {} / "
                  "{}\", lyra::sdk::PowerToString(lyra::sdk::global_precision_"
                  "power), lyra::sdk::PowerToString(lyra::sdk::global_"
                  "precision_power));\n";
          break;
        }
        throw DiagnosticException(
            Diagnostic::Error(
                {}, "C++ codegen: unsupported system call: " + syscall.name));
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
    case mir::Statement::Kind::kRepeat: {
      const auto& repeat_stmt = mir::As<mir::RepeatStatement>(stmt);
      Indent();
      out_ << "for (int _repeat_i = static_cast<int>(";
      EmitExpression(*repeat_stmt.count);
      out_ << "); _repeat_i > 0; --_repeat_i) {\n";
      indent_++;
      EmitStatement(*repeat_stmt.body);
      indent_--;
      Indent();
      out_ << "}\n";
      break;
    }
    case mir::Statement::Kind::kCase: {
      const auto& case_stmt = mir::As<mir::CaseStatement>(stmt);

      // Block scope for the condition temp variable
      Indent();
      out_ << "{\n";
      indent_++;

      // Emit condition once into a temp
      Indent();
      out_ << "auto _case_cond = ";
      EmitExpression(*case_stmt.condition);
      out_ << ";\n";

      // Emit if-else-if chain for each item
      bool first = true;
      for (const auto& item : case_stmt.items) {
        Indent();
        if (first) {
          out_ << "if (";
          first = false;
        } else {
          out_ << "} else if (";
        }

        // Emit condition:
        // Normal case: (_case_cond == expr0) || (_case_cond == expr1) || ...
        // Wildcard case: ((_case_cond & mask0) == expr0) || ...
        for (size_t i = 0; i < item.expressions.size(); ++i) {
          if (i > 0) {
            out_ << " || ";
          }
          int64_t mask = item.masks[i];
          // Check if mask is all-1s (no wildcards) - can use direct comparison
          // A mask is "all ones" if it's -1 or all bits are set for the width
          auto umask = static_cast<uint64_t>(mask);
          bool is_all_ones = (mask == -1) ||
                             (umask == 0xFFFFFFFF) ||        // 32-bit all 1s
                             (umask == 0xFFFFFFFFFFFFFFFF);  // 64-bit all 1s
          if (is_all_ones) {
            // No wildcards - direct comparison
            out_ << "_case_cond == ";
          } else {
            // Wildcard case - apply mask
            // Cast to uint64_t to avoid negative hex output
            out_ << "(static_cast<uint64_t>(_case_cond) & 0x" << std::hex
                 << umask << std::dec << "ULL) == ";
          }
          EmitExpression(*item.expressions[i]);
        }
        out_ << ") {\n";
        indent_++;
        if (item.statement) {
          EmitStatement(*item.statement);
        }
        indent_--;
      }

      // Emit default case (if any)
      if (case_stmt.default_case) {
        if (!case_stmt.items.empty()) {
          Indent();
          out_ << "} else {\n";
          indent_++;
          EmitStatement(*case_stmt.default_case);
          indent_--;
        } else {
          // No items, just default
          EmitStatement(*case_stmt.default_case);
        }
      }

      // Close if chain
      if (!case_stmt.items.empty()) {
        Indent();
        out_ << "}\n";
      }

      // Close block scope
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
      // Scale delay based on module's timescale
      uint64_t scaled_delay = delay.delay_amount * DelayMultiplier();
      Line("co_await lyra::sdk::Delay(" + std::to_string(scaled_delay) + ");");
      break;
    }
    case mir::Statement::Kind::kWaitEvent: {
      const auto& wait = mir::As<mir::WaitEventStatement>(stmt);
      if (wait.triggers.empty()) {
        break;
      }

      // Helper to get variable name with underscore suffix for ports
      auto get_var_name = [this](const slang::ast::Symbol* symbol) {
        std::string name(symbol->name);
        if (port_symbols_.contains(symbol)) {
          name += "_";
        }
        return name;
      };

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
        std::string var_name = get_var_name(trigger.variable);
        Line("co_await " + trigger_expr(var_name, trigger.edge_kind) + ";");
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
            out_ << "&" << get_var_name(wait.triggers[i].variable);
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
            std::string var_name = get_var_name(trigger.variable);
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
      throw DiagnosticException(
          Diagnostic::Error(
              {}, "C++ codegen: unimplemented statement kind: " +
                      ToString(stmt.kind)));
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
      if (lit.literal.type.kind == common::Type::Kind::kIntegral) {
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
      // Append underscore for port reference members (Google style)
      if (port_symbols_.contains(ident.symbol)) {
        out_ << "_";
      }
      break;
    }
    case mir::Expression::Kind::kBinary: {
      const auto& bin = mir::As<mir::BinaryExpression>(expr);
      if (bin.op == mir::BinaryOperator::kPower) {
        // Power: std::pow(a, b) - C++ doesn't have ** operator
        used_features_ |= CodegenFeature::kCmath;
        out_ << "std::pow(";
        EmitExpression(*bin.left, kPrecLowest);
        out_ << ", ";
        EmitExpression(*bin.right, kPrecLowest);
        out_ << ")";
      } else if (bin.op == mir::BinaryOperator::kBitwiseXnor) {
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
        // TODO(hankhsu): Support NBA for array elements
        out_ << "this->ScheduleNba(&" << assign.target.symbol->name;
        // Append underscore for port reference members (Google style)
        if (port_symbols_.contains(assign.target.symbol)) {
          out_ << "_";
        }
        out_ << ", ";
        EmitExpression(*assign.value, kPrecLowest);
        out_ << ")";
      } else if (assign.target.IsPacked() && assign.target.IsElementSelect()) {
        // Packed element assignment as expression
        // Result is the assigned value (the RHS)
        out_ << "(";
        const auto& base_type = assign.target.base_type.value();
        size_t total_width = base_type.GetBitWidth();
        int32_t lower_bound = base_type.GetElementLower();
        size_t element_width = base_type.GetElementWidth();

        used_type_aliases_ |= TypeAlias::kBit;
        uint64_t mask = (1ULL << element_width) - 1;
        out_ << assign.target.symbol->name << " = ("
             << assign.target.symbol->name << " & ~(Bit<" << total_width << ">{"
             << mask << "ULL} << ";
        EmitPackedBitPosition(
            *assign.target.element_index, lower_bound, element_width);
        out_ << ")) | ((Bit<" << total_width << ">{";
        EmitExpression(*assign.value, kPrecLowest);
        out_ << ".Value() & " << mask << "ULL} << ";
        EmitPackedBitPosition(
            *assign.target.element_index, lower_bound, element_width);
        out_ << ")), ";
        EmitExpression(*assign.value, kPrecLowest);
        out_ << ")";
      } else {
        EmitAssignmentTarget(assign.target);
        out_ << " = ";
        EmitExpression(*assign.value, kPrecAssign);
      }
      break;
    }
    case mir::Expression::Kind::kElementSelect: {
      const auto& select = mir::As<mir::ElementSelectExpression>(expr);

      // Check if this is bit/element selection (packed type) or array access
      if (select.value->type.kind == common::Type::Kind::kIntegral) {
        // Get element width from result type
        size_t element_width = expr.type.GetBitWidth();

        // Get element_lower for non-zero-based ranges
        int32_t lower_bound = select.value->type.GetElementLower();

        if (element_width == 1) {
          // Single bit selection: value.GetBit(index - lower_bound)
          EmitExpression(*select.value, kPrecPrimary);
          out_ << ".GetBit(";
          if (lower_bound != 0) {
            out_ << "static_cast<int>(";
          }
          EmitExpression(*select.selector, kPrecLowest);
          if (lower_bound != 0) {
            out_ << ") - " << lower_bound;
          }
          out_ << ")";
        } else {
          // Multi-bit element selection from 2D packed array
          // Generate: static_cast<ResultType>((static_cast<uint64_t>(value) >>
          // ((index - lb) * width)) & mask)
          // Need to cast to uint64_t first to avoid ambiguous operator& with
          // Bit<N>
          out_ << "static_cast<" << ToCppType(expr.type)
               << ">((static_cast<uint64_t>(";
          EmitExpression(*select.value, kPrecLowest);
          out_ << ") >> ((static_cast<size_t>(";
          EmitExpression(*select.selector, kPrecLowest);
          if (lower_bound != 0) {
            out_ << ") - " << lower_bound;
          } else {
            out_ << ")";
          }
          out_ << ") * " << element_width << ")) & "
               << std::format("0x{:X}ULL", (1ULL << element_width) - 1) << ")";
        }
      } else {
        // Array element access: array[index]
        EmitExpression(*select.value, kPrecPrimary);
        // Cast index to size_t to avoid Bit<N> → bool → size_t issues
        out_ << "[static_cast<size_t>(";
        EmitExpression(*select.selector, kPrecLowest);
        out_ << ")]";
      }
      break;
    }
    case mir::Expression::Kind::kRangeSelect: {
      const auto& range = mir::As<mir::RangeSelectExpression>(expr);
      size_t result_width = expr.type.GetBitWidth();

      // Compute LSB (minimum of left and right bounds)
      int32_t lsb = std::min(range.left, range.right);

      // Adjust for non-zero-based ranges (e.g., bit [63:32])
      if (range.value->type.kind == common::Type::Kind::kIntegral) {
        lsb -= range.value->type.GetElementLower();
      }

      // Generate: static_cast<ResultType>((static_cast<uint64_t>(value) >> lsb)
      // & mask) Need to cast to uint64_t first to avoid ambiguous operator&
      // with Bit<N>
      out_ << "static_cast<" << ToCppType(expr.type)
           << ">((static_cast<uint64_t>(";
      EmitExpression(*range.value, kPrecLowest);
      out_ << ") >> " << lsb << ") & "
           << std::format("0x{:X}ULL", (1ULL << result_width) - 1) << ")";
      break;
    }
    case mir::Expression::Kind::kIndexedRangeSelect: {
      const auto& indexed = mir::As<mir::IndexedRangeSelectExpression>(expr);
      size_t result_width = expr.type.GetBitWidth();
      uint64_t mask = (1ULL << result_width) - 1;
      int32_t lower = indexed.value->type.GetElementLower();

      // Generate: static_cast<ResultType>((static_cast<uint64_t>(value) >>
      // shift) & mask)
      out_ << "static_cast<" << ToCppType(expr.type)
           << ">((static_cast<uint64_t>(";
      EmitExpression(*indexed.value, kPrecLowest);
      out_ << ") >> ";

      // Ascending (+:): shift by start - lower
      // Descending (-:): shift by start - (width-1) - lower
      int32_t width_offset = indexed.is_ascending ? 0 : (indexed.width - 1);
      EmitSliceShift(*indexed.start, lower, width_offset);

      out_ << ") & " << std::format("0x{:X}ULL", mask) << ")";
      break;
    }
    case mir::Expression::Kind::kHierarchicalReference: {
      const auto& hier = mir::As<mir::HierarchicalReferenceExpression>(expr);
      EmitHierarchicalPath(hier.path);
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
    case mir::Expression::Kind::kSystemCall: {
      // System functions that return values
      const auto& syscall = mir::As<mir::SystemCallExpression>(expr);
      if (syscall.name == "$time") {
        used_features_ |= CodegenFeature::kTimeDivisor;
        out_ << "lyra::sdk::Time(kTimeDivisor)";
      } else if (syscall.name == "$stime") {
        used_features_ |= CodegenFeature::kTimeDivisor;
        out_ << "lyra::sdk::STime(kTimeDivisor)";
      } else if (syscall.name == "$realtime") {
        used_features_ |= CodegenFeature::kTimeDivisor;
        out_ << "lyra::sdk::RealTime(kTimeDivisor)";
      } else if (syscall.name == "$timeunit") {
        used_features_ |= CodegenFeature::kModuleUnitPower;
        out_ << "kModuleUnitPower";
      } else if (syscall.name == "$timeprecision") {
        used_features_ |= CodegenFeature::kModulePrecisionPower;
        out_ << "kModulePrecisionPower";
      } else if (
          syscall.name == "$timeunit_root" ||
          syscall.name == "$timeprecision_root") {
        out_ << "lyra::sdk::global_precision_power";
      } else if (syscall.name == "$signed" || syscall.name == "$unsigned") {
        // Cast to target signedness, preserving bit pattern
        out_ << "static_cast<" << ToCppType(syscall.type) << ">(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$itor") {
        // Convert integer to real
        out_ << "static_cast<Real>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$rtoi") {
        // Convert real to integer by truncation
        out_ << "static_cast<" << ToCppType(syscall.type) << ">(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$realtobits") {
        // Real to 64-bit IEEE 754 bits
        out_ << "std::bit_cast<uint64_t>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$bitstoreal") {
        // 64-bit bits to real
        out_ << "std::bit_cast<Real>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$shortrealtobits") {
        // Shortreal to 32-bit IEEE 754 bits
        out_ << "std::bit_cast<uint32_t>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else if (syscall.name == "$bitstoshortreal") {
        // 32-bit bits to shortreal
        out_ << "std::bit_cast<ShortReal>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << ")";
      } else {
        // System tasks like $display, $finish are handled in statement context
        throw common::InternalError(
            "codegen", "unexpected system call in expression: " + syscall.name);
      }
      break;
    }
    default:
      throw DiagnosticException(
          Diagnostic::Error(
              {}, "C++ codegen: unimplemented expression kind: " +
                      ToString(expr.kind)));
  }
}

void Codegen::EmitAssignmentTarget(const mir::AssignmentTarget& target) {
  if (target.IsHierarchical()) {
    EmitHierarchicalPath(target.hierarchical_path);
    return;
  }

  out_ << target.symbol->name;
  // Append underscore for port reference members (Google style)
  if (port_symbols_.contains(target.symbol)) {
    out_ << "_";
  }
  if (target.IsElementSelect()) {
    // Cast index to size_t to avoid Bit<N> → bool → size_t conversion issues
    out_ << "[static_cast<size_t>(";
    EmitExpression(*target.element_index, kPrecLowest);
    out_ << ")]";
  }
}

void Codegen::EmitPackedBitPosition(
    const mir::Expression& index_expr, int32_t lower_bound,
    size_t element_width) {
  // Emits: (static_cast<int/size_t>(index) - lower_bound) * element_width
  // or:    static_cast<size_t>(index) * element_width  when lower_bound == 0
  if (lower_bound != 0) {
    out_ << "((static_cast<int>(";
    EmitExpression(index_expr, kPrecLowest);
    out_ << ") - " << lower_bound << ") * " << element_width << ")";
  } else {
    out_ << "(static_cast<size_t>(";
    EmitExpression(index_expr, kPrecLowest);
    out_ << ") * " << element_width << ")";
  }
}

void Codegen::EmitSliceShift(
    const mir::Expression& start_expr, int32_t lower_bound,
    int32_t width_offset) {
  // Emits: (static_cast<size_t>(start) - width_offset - lower_bound)
  // width_offset is 0 for ascending (+:), or (width-1) for descending (-:)
  out_ << "(static_cast<size_t>(";
  EmitExpression(start_expr, kPrecLowest);
  out_ << ")";
  if (width_offset != 0) {
    out_ << " - " << width_offset;
  }
  if (lower_bound != 0) {
    out_ << " - " << lower_bound;
  }
  out_ << ")";
}

void Codegen::EmitHierarchicalPath(const std::vector<std::string>& path) {
  // Emit hierarchical path: ["child", "signal"] -> child_.signal
  // Instance names (all but last) get _ suffix, variable name (last) does not
  for (size_t i = 0; i < path.size(); ++i) {
    if (i > 0) {
      out_ << ".";
    }
    out_ << path[i];
    // Only instance names (not the final variable) get _ suffix
    if (i < path.size() - 1) {
      out_ << "_";
    }
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
