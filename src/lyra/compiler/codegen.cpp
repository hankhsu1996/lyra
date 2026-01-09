#include "lyra/compiler/codegen.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <ios>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/string_utils.hpp"
#include "lyra/common/sv_format.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operators.hpp"
#include "lyra/mir/package.hpp"
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

// Returns true if bit width requires WideBit (> 64 bits)
constexpr auto IsWideWidth(size_t width) -> bool {
  return width > 64;
}

// Convert an integral literal to string (for bit-packed format strings).
// Each 8 bits forms one character, MSB first, null bytes are skipped.
auto IntegralLiteralToString(const common::Literal& lit) -> std::string {
  std::string result;
  if (lit.type.kind != common::Type::Kind::kIntegral) {
    return result;
  }
  auto data = std::get<common::IntegralData>(lit.type.data);
  size_t width = data.bit_width;

  if (lit.value.IsInt64()) {
    auto bits = static_cast<uint64_t>(lit.value.AsInt64());
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else if (lit.value.IsWideBit()) {
    const auto& wide = lit.value.AsWideBit();
    for (size_t i = width; i >= 8; i -= 8) {
      size_t byte_start = i - 8;
      uint8_t ch = 0;
      for (size_t b = 0; b < 8; ++b) {
        ch |= static_cast<uint8_t>(wide.GetBit(byte_start + b) << b);
      }
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  }
  return result;
}

// Extract format string info from a MIR expression.
// Handles string literals and bit-packed strings (via is_string_literal flag).
auto ExtractFormatString(const mir::Expression& expr)
    -> common::FormatStringInfo {
  common::FormatStringInfo info;

  if (expr.kind != mir::Expression::Kind::kLiteral) {
    return info;
  }
  const auto& lit = mir::As<mir::LiteralExpression>(expr);
  if (lit.literal.type.kind == common::Type::Kind::kString) {
    info.text = lit.literal.value.AsString();
    info.is_string_literal = true;
  } else if (lit.literal.is_string_literal) {
    // Bit-packed string: slang typed it as integral but it came from a string
    // literal. Convert back to string for format detection.
    info.text = IntegralLiteralToString(lit.literal);
    info.is_string_literal = true;
  }
  info.has_format_specifiers =
      info.is_string_literal && info.text.find('%') != std::string::npos;
  return info;
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

// Properties for display/write variants
struct DisplayVariantProps {
  char default_format;  // 'd', 'b', 'o', or 'h' (for std::format)
  bool use_println;     // true for $display*, false for $write*
};

// Get properties for a display/write variant
auto GetDisplayVariantProps(std::string_view name) -> DisplayVariantProps {
  if (name == "$write") {
    return {.default_format = 'd', .use_println = false};
  }
  if (name == "$writeb") {
    return {.default_format = 'b', .use_println = false};
  }
  if (name == "$writeo") {
    return {.default_format = 'o', .use_println = false};
  }
  if (name == "$writeh") {
    return {.default_format = 'x', .use_println = false};
  }
  if (name == "$displayb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$displayo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$displayh") {
    return {.default_format = 'x', .use_println = true};
  }
  if (name == "$strobeb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$strobeo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$strobeh") {
    return {.default_format = 'x', .use_println = true};
  }
  if (name == "$monitorb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$monitoro") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$monitorh") {
    return {.default_format = 'x', .use_println = true};
  }
  // $display, $strobe, $monitor (default)
  return {.default_format = 'd', .use_println = true};
}

// Get element bit width after applying N indices to a multi-dimensional type
auto GetElementWidthAfterIndices(
    const common::Type& base_type, size_t num_indices) -> size_t {
  const common::Type* current = &base_type;
  for (size_t i = 0; i < num_indices; ++i) {
    current = &current->GetElementType();
  }
  return current->GetBitWidth();
}

}  // namespace

auto Codegen::IsSigned(const common::Type& type) -> bool {
  if (type.kind != common::Type::Kind::kIntegral) {
    return false;
  }
  return std::get<common::IntegralData>(type.data).is_signed;
}

auto Codegen::ToCppType(const common::Type& type) -> std::string {
  // Handle user-defined type aliases (typedef)
  if (type.alias_name) {
    const auto& name = *type.alias_name;

    // Get the C++ definition for this alias by computing it without the alias
    // name
    common::Type underlying = type;
    underlying.alias_name = std::nullopt;
    std::string cpp_def = ToCppType(underlying);

    // Register the alias for later emission
    user_type_aliases_[name] = cpp_def;

    return name;
  }

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

      if (IsWideWidth(width)) {
        // Wide integers use lyra::sdk::WideBit<N>
        if (is_signed) {
          return std::format("lyra::sdk::WideBit<{}, true>", width);
        }
        return std::format("lyra::sdk::WideBit<{}>", width);
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
    case common::Type::Kind::kPackedStruct: {
      // Packed structs are bitvectors - same as kIntegral
      auto data = std::get<common::PackedStructData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      if (IsWideWidth(width)) {
        if (is_signed) {
          return std::format("lyra::sdk::WideBit<{}, true>", width);
        }
        return std::format("lyra::sdk::WideBit<{}>", width);
      }

      used_type_aliases_ |= TypeAlias::kBit;
      if (is_signed) {
        return std::format("Bit<{}, true>", width);
      }
      return std::format("Bit<{}>", width);
    }
  }
  throw common::InternalError(
      "ToCppType", std::format("unhandled type kind: {}", type.ToString()));
}

auto Codegen::ToCppUnsignedType(const common::Type& type) -> std::string {
  if (type.kind != common::Type::Kind::kIntegral) {
    return "/* unknown type */";
  }
  auto data = std::get<common::IntegralData>(type.data);
  size_t width = data.bit_width;
  if (IsWideWidth(width)) {
    // Always return unsigned lyra::sdk::WideBit<N>
    return std::format("lyra::sdk::WideBit<{}>", width);
  }
  // Always return unsigned Bit<N> regardless of original signedness
  used_type_aliases_ |= TypeAlias::kBit;
  return std::format("Bit<{}>", width);
}

auto Codegen::Generate(const mir::Module& module, bool skip_sdk_aliases)
    -> std::string {
  out_.str("");
  indent_ = 0;
  port_symbols_.clear();
  used_type_aliases_ = TypeAlias::kNone;
  user_type_aliases_.clear();
  used_features_ = CodegenFeature::kNone;
  skip_sdk_aliases_ = skip_sdk_aliases;

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
  EmitHeader(module, UsesArrayType(module));

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

void Codegen::EmitHeader(const mir::Module& module, bool uses_arrays) {
  // System headers
  bool has_system_headers = false;
  if (uses_arrays) {
    Line("#include <array>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kCmath) != CodegenFeature::kNone) {
    Line("#include <cmath>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kDisplay) != CodegenFeature::kNone) {
    Line("#include <iostream>");
    Line("#include <print>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kMemIo) != CodegenFeature::kNone) {
    has_system_headers = true;
  }
  if (has_system_headers) {
    Line("");
  }
  // Lyra SDK header
  Line("#include <lyra/sdk/sdk.hpp>");
  if ((used_features_ & CodegenFeature::kMemIo) != CodegenFeature::kNone) {
    Line("#include <lyra/sdk/mem_io.hpp>");
  }

  // Include headers for submodule types
  std::unordered_set<std::string> included;
  for (const auto& submod : module.submodules) {
    if (!included.contains(submod.module_type)) {
      Line("#include \"" + submod.module_type + ".hpp\"");
      included.insert(submod.module_type);
    }
  }

  Line("");

  // Emit using directives for package imports
  for (const auto& pkg_name : module.wildcard_imports) {
    Line(std::format("using namespace {};", pkg_name));
  }
  for (const auto& [pkg, sym] : module.explicit_imports) {
    Line(std::format("using {}::{};", pkg, sym));
  }
  if (!module.wildcard_imports.empty() || !module.explicit_imports.empty()) {
    Line("");
  }
}

void Codegen::EmitTypeAliases() {
  bool any_emitted = false;

  // Emit SDK type aliases unless they're already at file scope (from
  // packages.hpp)
  if (!skip_sdk_aliases_) {
    if ((used_type_aliases_ & TypeAlias::kBit) != TypeAlias::kNone) {
      Line("template <std::size_t N, bool S = false>");
      Line("using Bit = lyra::sdk::Bit<N, S>;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kInt) != TypeAlias::kNone) {
      Line("using Int = lyra::sdk::Int;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kLongInt) != TypeAlias::kNone) {
      Line("using LongInt = lyra::sdk::LongInt;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kShortInt) != TypeAlias::kNone) {
      Line("using ShortInt = lyra::sdk::ShortInt;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kByte) != TypeAlias::kNone) {
      Line("using Byte = lyra::sdk::Byte;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kReal) != TypeAlias::kNone) {
      Line("using Real = double;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kShortReal) != TypeAlias::kNone) {
      Line("using ShortReal = float;");
      any_emitted = true;
    }
  }

  // Emit user-defined type aliases (typedef) - always needed for local typedefs
  for (const auto& [name, def] : user_type_aliases_) {
    Line(std::format("using {} = {};", name, def));
    any_emitted = true;
  }

  if (any_emitted) {
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

  out_ << ") : Module(\"" << module.name << "\")";

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

  // User-defined functions (helper methods, emitted before processes)
  for (const auto& function : module.functions) {
    EmitFunction(function);
  }

  // Process methods
  for (const auto& process : module.processes) {
    EmitProcess(*process);
  }

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

void Codegen::EmitFunction(const mir::FunctionDefinition& function) {
  Indent();
  out_ << "auto " << function.name << "(";

  // Parameters
  bool first = true;
  for (const auto& param : function.parameters) {
    if (!first) {
      out_ << ", ";
    }
    first = false;
    out_ << ToCppType(param.variable.type) << " "
         << param.variable.symbol->name;
  }

  out_ << ") -> " << ToCppType(function.return_type) << " {\n";
  indent_++;

  // Body (local variables are emitted via VariableDeclarationStatement)
  if (function.body) {
    EmitStatement(*function.body);
  }

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
        // Handle strobe variants - schedule to Postponed region
        if (syscall.name == "$strobe" || syscall.name == "$strobeb" ||
            syscall.name == "$strobeo" || syscall.name == "$strobeh") {
          used_features_ |= CodegenFeature::kDisplay;
          auto props = GetDisplayVariantProps(syscall.name);

          // Wrap print in lambda scheduled to Postponed region
          Line("lyra::sdk::current_scheduler->SchedulePostponed([=]() {");
          indent_++;

          // Empty call - just print newline
          if (syscall.arguments.empty()) {
            Line("std::println(std::cout, \"\");");
            indent_--;
            Line("});");
            break;
          }

          // Extract format string info from first argument
          auto fmt_info = ExtractFormatString(*syscall.arguments[0]);
          size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;

          // Print prefix (string literal without format specifiers)
          if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
            Indent();
            out_ << "std::print(std::cout, \"{}\", \""
                 << common::EscapeForCppString(fmt_info.text) << "\");\n";
          }

          // Print arguments with format string
          std::string sv_fmt =
              fmt_info.has_format_specifiers ? fmt_info.text : "";
          if (first_arg_idx < syscall.arguments.size()) {
            EmitFormattedPrint(
                syscall.arguments, first_arg_idx, sv_fmt, "std::println",
                props.default_format);
          } else {
            Line("std::println(std::cout, \"\");");
          }
          indent_--;
          Line("});");
          break;
        }
        // Handle monitor variants - register for value change tracking
        if (syscall.name == "$monitor" || syscall.name == "$monitorb" ||
            syscall.name == "$monitoro" || syscall.name == "$monitorh") {
          used_features_ |= CodegenFeature::kDisplay;
          auto props = GetDisplayVariantProps(syscall.name);

          // Empty call - print newline once (no values to monitor for changes)
          if (syscall.arguments.empty()) {
            Line("std::println(std::cout, \"\");");
            break;
          }

          // Extract format string info from first argument
          auto fmt_info = ExtractFormatString(*syscall.arguments[0]);
          size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;
          std::string sv_fmt =
              fmt_info.has_format_specifiers ? fmt_info.text : "";
          std::string prefix_str =
              (fmt_info.is_string_literal && !fmt_info.has_format_specifiers)
                  ? fmt_info.text
                  : "";

          // Generate block scope for previous value captures
          Line("{");
          indent_++;

          // Capture initial values for each monitored argument
          std::vector<std::string> prev_names;
          for (size_t i = first_arg_idx; i < syscall.arguments.size(); ++i) {
            auto prev_name = std::format("prev_{}", i - first_arg_idx);
            prev_names.push_back(prev_name);
            Indent();
            out_ << "auto " << prev_name << " = ";
            EmitExpression(*syscall.arguments[i]);
            out_ << ";\n";
          }

          // Generate SetMonitor call with lambda
          // Capture 'this' for class member access and prev values by copy
          Indent();
          out_ << "lyra::sdk::current_scheduler->SetMonitor([this";
          for (const auto& prev_name : prev_names) {
            out_ << ", " << prev_name;
          }
          out_ << "]() mutable {\n";
          indent_++;

          // Generate comparison: if (arg1 != prev_0 || arg2 != prev_1 || ...)
          // When no variables, use 'false' - nothing to track means never
          // prints again
          Indent();
          out_ << "if (";
          if (prev_names.empty()) {
            out_ << "false";
          } else {
            for (size_t i = first_arg_idx; i < syscall.arguments.size(); ++i) {
              if (i > first_arg_idx) {
                out_ << " || ";
              }
              EmitExpression(*syscall.arguments[i]);
              out_ << " != " << prev_names[i - first_arg_idx];
            }
          }
          out_ << ") {\n";
          indent_++;

          // Generate print statement inside the if block
          if (!prefix_str.empty()) {
            Indent();
            out_ << "std::print(std::cout, \"{}\", \""
                 << common::EscapeForCppString(prefix_str) << "\");\n";
          }
          if (first_arg_idx < syscall.arguments.size()) {
            EmitFormattedPrint(
                syscall.arguments, first_arg_idx, sv_fmt, "std::println",
                props.default_format);
          } else {
            Line("std::println(std::cout, \"\");");
          }

          // Update previous values
          for (size_t i = first_arg_idx; i < syscall.arguments.size(); ++i) {
            Indent();
            out_ << prev_names[i - first_arg_idx] << " = ";
            EmitExpression(*syscall.arguments[i]);
            out_ << ";\n";
          }

          indent_--;
          Line("}");  // End if
          indent_--;
          Line("});");  // End lambda and SetMonitor call

          // Print immediately on first call (IEEE 1800 §21.2.3)
          if (!prefix_str.empty()) {
            Indent();
            out_ << "std::print(std::cout, \"{}\", \""
                 << common::EscapeForCppString(prefix_str) << "\");\n";
          }
          if (first_arg_idx < syscall.arguments.size()) {
            EmitFormattedPrint(
                syscall.arguments, first_arg_idx, sv_fmt, "std::println",
                props.default_format);
          } else {
            Line("std::println(std::cout, \"\");");
          }

          indent_--;
          Line("}");  // End block scope
          break;
        }
        // Handle $monitoron and $monitoroff
        if (syscall.name == "$monitoron") {
          Line("lyra::sdk::current_scheduler->SetMonitorEnabled(true);");
          break;
        }
        if (syscall.name == "$monitoroff") {
          Line("lyra::sdk::current_scheduler->SetMonitorEnabled(false);");
          break;
        }
        // Handle all display/write variants
        if (syscall.name == "$display" || syscall.name == "$displayb" ||
            syscall.name == "$displayo" || syscall.name == "$displayh" ||
            syscall.name == "$write" || syscall.name == "$writeb" ||
            syscall.name == "$writeo" || syscall.name == "$writeh") {
          used_features_ |= CodegenFeature::kDisplay;
          auto props = GetDisplayVariantProps(syscall.name);
          std::string_view print_fn =
              props.use_println ? "std::println" : "std::print";

          // Empty call - just print newline if needed
          if (syscall.arguments.empty()) {
            if (props.use_println) {
              Line("std::println(std::cout, \"\");");
            }
            // $write with no args does nothing
            break;
          }

          // Extract format string info from first argument
          auto fmt_info = ExtractFormatString(*syscall.arguments[0]);
          size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;

          // Print prefix (string literal without format specifiers)
          if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
            Indent();
            out_ << "std::print(std::cout, \"{}\", \""
                 << common::EscapeForCppString(fmt_info.text) << "\");\n";
          }

          // Print remaining args with format string
          std::string sv_fmt =
              fmt_info.has_format_specifiers ? fmt_info.text : "";
          if (first_arg_idx < syscall.arguments.size()) {
            EmitFormattedPrint(
                syscall.arguments, first_arg_idx, sv_fmt, print_fn,
                props.default_format);
          } else if (props.use_println) {
            // No more args, but need newline
            Line("std::println(std::cout, \"\");");
          }
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
          used_features_ |= CodegenFeature::kDisplay;
          used_features_ |= CodegenFeature::kModuleName;
          used_features_ |= CodegenFeature::kTimescaleStr;
          Line(
              "std::println(std::cout, \"Time scale of ({}) is {}\", "
              "kModuleName, kTimescaleStr);");
          break;
        }
        if (syscall.name == "$printtimescale_root") {
          // $printtimescale($root) - print global precision (unit = precision)
          used_features_ |= CodegenFeature::kDisplay;
          Indent();
          out_ << "std::println(std::cout, \"Time scale of ($root) is {} / "
                  "{}\", lyra::sdk::PowerToString(lyra::sdk::global_precision_"
                  "power), lyra::sdk::PowerToString(lyra::sdk::global_"
                  "precision_power));\n";
          break;
        }
        if (syscall.name == "$readmemh" || syscall.name == "$readmemb" ||
            syscall.name == "$writememh" || syscall.name == "$writememb") {
          used_features_ |= CodegenFeature::kMemIo;
          bool is_read =
              syscall.name == "$readmemh" || syscall.name == "$readmemb";
          bool is_hex =
              syscall.name == "$readmemh" || syscall.name == "$writememh";
          bool has_start = syscall.arguments.size() >= 3;
          bool has_end = syscall.arguments.size() == 4;

          if (syscall.arguments.size() < 2 || syscall.arguments.size() > 4) {
            throw common::InternalError(
                "codegen", "mem I/O expects 2-4 arguments");
          }

          const auto& target_expr = *syscall.arguments[1];
          if (target_expr.kind != mir::Expression::Kind::kIdentifier) {
            throw common::InternalError(
                "codegen", "mem I/O target must be a named variable");
          }

          const auto& target = mir::As<mir::IdentifierExpression>(target_expr);
          const auto& target_type = target_expr.type;

          // Helper to emit filename, converting integral to string if needed
          auto emit_filename = [&]() {
            const auto& filename_arg = *syscall.arguments[0];

            // Integral literal (bit-packed string): convert to string at
            // runtime
            if (filename_arg.kind == mir::Expression::Kind::kLiteral &&
                filename_arg.type.kind == common::Type::Kind::kIntegral) {
              out_ << "lyra::sdk::IntToString(";
              EmitExpression(filename_arg);
              out_ << ")";
              return;
            }

            // Default: emit expression directly (e.g., string variable)
            EmitExpression(filename_arg);
          };

          if (target_type.kind == common::Type::Kind::kUnpackedArray) {
            const auto& arr =
                std::get<common::UnpackedArrayData>(target_type.data);
            Indent();
            out_ << "lyra::sdk::"
                 << (is_read ? "ReadMemArray(" : "WriteMemArray(")
                 << target.symbol->name << ", " << arr.lower_bound << ", ";
            emit_filename();
            out_ << ", " << (has_start ? "true" : "false") << ", ";
            if (has_start) {
              EmitExpression(*syscall.arguments[2]);
            } else {
              out_ << "0";
            }
            out_ << ", " << (has_end ? "true" : "false") << ", ";
            if (has_end) {
              EmitExpression(*syscall.arguments[3]);
            } else {
              out_ << "0";
            }
            out_ << ", " << (is_hex ? "true" : "false") << ");\n";
            break;
          }

          if (target_type.kind == common::Type::Kind::kIntegral) {
            const auto& integral =
                std::get<common::IntegralData>(target_type.data);
            size_t element_width = integral.element_type
                                       ? integral.element_type->GetBitWidth()
                                       : integral.bit_width;
            size_t element_count =
                integral.element_type ? integral.element_count : 1;
            int32_t lower_bound = integral.element_lower;
            Indent();
            out_ << "lyra::sdk::"
                 << (is_read ? "ReadMemPacked(" : "WriteMemPacked(")
                 << target.symbol->name << ", " << element_width << ", "
                 << element_count << ", " << lower_bound << ", ";
            emit_filename();
            out_ << ", " << (has_start ? "true" : "false") << ", ";
            if (has_start) {
              EmitExpression(*syscall.arguments[2]);
            } else {
              out_ << "0";
            }
            out_ << ", " << (has_end ? "true" : "false") << ", ";
            if (has_end) {
              EmitExpression(*syscall.arguments[3]);
            } else {
              out_ << "0";
            }
            out_ << ", " << (is_hex ? "true" : "false") << ");\n";
            break;
          }

          throw common::InternalError(
              "codegen", "mem I/O target must be an unpacked or packed array");
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
        auto integral_data =
            std::get<common::IntegralData>(lit.literal.type.data);
        // For wide types, use parentheses to avoid parsing ambiguity with >
        // For narrow types, use braces for uniform initialization
        if (IsWideWidth(integral_data.bit_width)) {
          out_ << ToCppType(lit.literal.type) << "(" << lit.literal.ToString()
               << ")";
        } else {
          out_ << ToCppType(lit.literal.type) << "{" << lit.literal.ToString()
               << "}";
        }
      } else {
        // Non-integral types (string, etc.)
        out_ << lit.literal.ToString();
      }
      break;
    }
    case mir::Expression::Kind::kIdentifier: {
      const auto& ident = mir::As<mir::IdentifierExpression>(expr);
      // Check if symbol is from a package (needs qualified access)
      const auto* parent_scope = ident.symbol->getParentScope();
      if (parent_scope != nullptr) {
        const auto& parent_symbol = parent_scope->asSymbol();
        if (parent_symbol.kind == slang::ast::SymbolKind::Package) {
          out_ << parent_symbol.name << "::";
        }
      }
      out_ << ident.symbol->name;
      // Append underscore for port reference members (Google style)
      if (port_symbols_.contains(ident.symbol)) {
        out_ << "_";
      }
      break;
    }
    case mir::Expression::Kind::kEnumValue: {
      // Emit enum value as integer literal (no enum class generation)
      const auto& ev = mir::As<mir::EnumValueExpression>(expr);
      out_ << ToCppType(ev.type) << "{" << ev.value << "}";
      break;
    }
    case mir::Expression::Kind::kEnumMethod: {
      const auto& em = mir::As<mir::EnumMethodExpression>(expr);
      switch (em.method) {
        case mir::EnumMethod::kNext:
        case mir::EnumMethod::kPrev: {
          // Generate inline lambda with switch
          // Cast to int64_t since Bit<N,S> is a class type that can't be used
          // directly in switch
          out_ << "[&]() -> " << ToCppType(em.type)
               << " { switch (static_cast<int64_t>(";
          EmitExpression(*em.receiver, kPrecLowest);
          out_ << ")) {";
          for (size_t i = 0; i < em.members.size(); ++i) {
            size_t target_idx = 0;
            if (em.method == mir::EnumMethod::kNext) {
              // next(N): move forward N positions with wrap-around
              // E.g., for enum {A, B, C, D, E} at position 3 (D), next(3) goes
              // to position (3+3) % 5 = 1 (B)
              target_idx =
                  (i + static_cast<size_t>(em.step)) % em.members.size();
            } else {
              // prev(N): move backward N positions with wrap-around
              // We add members.size() before subtracting to avoid underflow
              // when step > i. The inner modulo handles step >= size.
              // E.g., for enum {A, B, C, D, E} at position 1 (B), prev(3) goes
              // to position (1 + 5 - 3) % 5 = 3 (D)
              target_idx =
                  (i + em.members.size() -
                   (static_cast<size_t>(em.step) % em.members.size())) %
                  em.members.size();
            }
            out_ << " case " << em.members[i].value << ": return "
                 << ToCppType(em.type) << "{" << em.members[target_idx].value
                 << "};";
          }
          // Default case: when receiver has an invalid value (not matching any
          // enum member), the behavior is implementation-defined per IEEE
          // 1800-2023. We return the type's default value (0).
          out_ << " default: return " << ToCppType(em.type) << "{};";
          out_ << " } }()";
          break;
        }
        case mir::EnumMethod::kName: {
          // Generate inline lambda returning string
          out_ << "[&]() -> std::string { switch (static_cast<int64_t>(";
          EmitExpression(*em.receiver, kPrecLowest);
          out_ << ")) {";
          for (const auto& m : em.members) {
            out_ << " case " << m.value << ": return \"" << m.name << "\";";
          }
          // Default case: when receiver has an invalid value (not matching any
          // enum member), the behavior is implementation-defined per IEEE
          // 1800-2023. We return an empty string.
          out_ << " default: return \"\";";
          out_ << " } }()";
          break;
        }
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
        // Packed element assignment as expression (possibly multi-dim)
        // Result is the assigned value (the RHS)
        const auto& base_type = assign.target.base_type.value();
        size_t total_width = base_type.GetBitWidth();
        size_t element_width = GetElementWidthAfterIndices(
            base_type, assign.target.indices.size());

        bool storage_is_wide = IsWideWidth(total_width);
        bool element_is_wide = IsWideWidth(element_width);

        out_ << "(";
        if (storage_is_wide || element_is_wide) {
          // Wide storage or wide element - use InsertSlice
          out_ << assign.target.symbol->name << " = "
               << assign.target.symbol->name << ".InsertSlice(";
          EmitExpression(*assign.value, kPrecLowest);
          out_ << ", ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << ", " << element_width << ")";
        } else {
          // Narrow storage and narrow element - use mask-and-merge
          used_type_aliases_ |= TypeAlias::kBit;
          uint64_t mask = common::MakeBitMask(element_width);
          out_ << assign.target.symbol->name << " = ("
               << assign.target.symbol->name << " & ~(Bit<" << total_width
               << ">{" << mask << "ULL} << ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << ")) | ((Bit<" << total_width << ">{";
          EmitExpression(*assign.value, kPrecLowest);
          out_ << ".Value() & " << mask << "ULL} << ";
          EmitCompositePackedBitPosition(assign.target.indices, base_type);
          out_ << "))";
        }
        out_ << ", ";
        EmitExpression(*assign.value, kPrecLowest);
        out_ << ")";
      } else if (assign.target.IsStructFieldAssignment()) {
        // Struct field assignment: my_struct.field = value
        const auto& base_type = assign.target.base_type.value();
        size_t total_width = base_type.GetBitWidth();
        size_t field_width = *assign.target.field_bit_width;
        uint64_t bit_offset = *assign.target.field_bit_offset;

        bool storage_is_wide = IsWideWidth(total_width);
        bool field_is_wide = IsWideWidth(field_width);

        out_ << "(";
        if (storage_is_wide || field_is_wide) {
          // Wide storage or wide field - use InsertSlice
          out_ << assign.target.symbol->name << " = "
               << assign.target.symbol->name << ".InsertSlice(";
          EmitExpression(*assign.value, kPrecLowest);
          out_ << ", " << bit_offset << ", " << field_width << ")";
        } else {
          // Narrow storage and narrow field - use mask-and-merge
          used_type_aliases_ |= TypeAlias::kBit;
          uint64_t mask = common::MakeBitMask(field_width);
          out_ << assign.target.symbol->name << " = ("
               << assign.target.symbol->name << " & ~(Bit<" << total_width
               << ">{" << mask << "ULL} << " << bit_offset << ")) | ((Bit<"
               << total_width << ">{";
          EmitExpression(*assign.value, kPrecLowest);
          out_ << ".Value() & " << mask << "ULL} << " << bit_offset << "))";
        }
        out_ << ", ";
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
          size_t source_width = select.value->type.GetBitWidth();
          bool source_is_wide = IsWideWidth(source_width);
          bool element_is_wide = IsWideWidth(element_width);

          if (source_is_wide || element_is_wide) {
            // Wide source or wide element - use ExtractSlice
            // Generate: value.ExtractSlice((index - lb) * width, width)
            EmitExpression(*select.value, kPrecPrimary);
            out_ << ".ExtractSlice((static_cast<size_t>(";
            EmitExpression(*select.selector, kPrecLowest);
            if (lower_bound != 0) {
              out_ << ") - " << lower_bound;
            } else {
              out_ << ")";
            }
            out_ << ") * " << element_width << ", " << element_width << ")";
          } else {
            // Narrow source and narrow element - use uint64_t shift/mask
            // Generate: static_cast<ResultType>((static_cast<uint64_t>(value)
            // >>
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
                 << std::format("0x{:X}ULL", common::MakeBitMask(element_width))
                 << ")";
          }
        }
      } else {
        // Array element access: array[index]
        // For non-zero-based arrays (e.g., int arr[2:5]), subtract lower_bound
        int32_t lower_bound = select.value->type.GetElementLower();
        EmitExpression(*select.value, kPrecPrimary);
        out_ << "[static_cast<size_t>(";
        if (lower_bound != 0) {
          // Cast to int to avoid ambiguous operator- with Bit<N>
          out_ << "static_cast<int>(";
        }
        EmitExpression(*select.selector, kPrecLowest);
        if (lower_bound != 0) {
          out_ << ") - " << lower_bound;
        }
        out_ << ")]";
      }
      break;
    }
    case mir::Expression::Kind::kRangeSelect: {
      const auto& range = mir::As<mir::RangeSelectExpression>(expr);
      size_t result_width = expr.type.GetBitWidth();
      size_t source_width = range.value->type.GetBitWidth();

      // Compute LSB (minimum of left and right bounds)
      int32_t lsb = std::min(range.left, range.right);

      // Adjust for non-zero-based ranges (e.g., bit [63:32])
      if (range.value->type.kind == common::Type::Kind::kIntegral) {
        lsb -= range.value->type.GetElementLower();
      }

      uint64_t mask = common::MakeBitMask(result_width);
      auto emit_shift = [&]() { out_ << lsb; };
      EmitSliceExtract(
          expr.type, *range.value, emit_shift, mask, IsWideWidth(source_width));
      break;
    }
    case mir::Expression::Kind::kIndexedRangeSelect: {
      const auto& indexed = mir::As<mir::IndexedRangeSelectExpression>(expr);
      size_t result_width = expr.type.GetBitWidth();
      size_t source_width = indexed.value->type.GetBitWidth();
      uint64_t mask = common::MakeBitMask(result_width);
      int32_t lower = indexed.value->type.GetElementLower();

      // Ascending (+:): shift by start - lower
      // Descending (-:): shift by start - (width-1) - lower
      int32_t width_offset = indexed.is_ascending ? 0 : (indexed.width - 1);

      auto emit_shift = [&]() {
        EmitSliceShift(*indexed.start, lower, width_offset);
      };
      EmitSliceExtract(
          expr.type, *indexed.value, emit_shift, mask,
          IsWideWidth(source_width));
      break;
    }
    case mir::Expression::Kind::kHierarchicalReference: {
      const auto& hier = mir::As<mir::HierarchicalReferenceExpression>(expr);
      EmitHierarchicalPath(hier.instance_path, hier.target_symbol);
      break;
    }
    case mir::Expression::Kind::kMemberAccess: {
      // Struct field access: struct_val.field_name
      // Implemented as bit extraction using ExtractBits or shift-and-mask
      const auto& member = mir::As<mir::MemberAccessExpression>(expr);
      size_t source_width = member.value->type.GetBitWidth();
      size_t field_width = member.bit_width;
      uint64_t bit_offset = member.bit_offset;

      bool source_is_wide = IsWideWidth(source_width);
      bool field_is_wide = IsWideWidth(field_width);

      if (source_is_wide || field_is_wide) {
        // Wide source or wide field - use ExtractSlice
        EmitExpression(*member.value, kPrecPrimary);
        out_ << ".ExtractSlice(" << bit_offset << ", " << field_width << ")";
      } else {
        // Narrow source and narrow field - use shift-and-mask
        uint64_t mask = common::MakeBitMask(field_width);
        out_ << "static_cast<" << ToCppType(expr.type) << ">(("
             << "static_cast<uint64_t>(";
        EmitExpression(*member.value, kPrecLowest);
        out_ << ") >> " << bit_offset << ") & " << mask << "ULL)";
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

      // Integral to string conversion (LRM 6.16)
      if (conv.value->type.kind == common::Type::Kind::kIntegral &&
          conv.target_type.kind == common::Type::Kind::kString) {
        out_ << "lyra::sdk::IntToString(";
        EmitExpression(*conv.value, kPrecLowest);
        out_ << ")";
        break;
      }

      // Default conversion
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
      } else if (syscall.name == "$clog2") {
        // Ceiling of log base 2 (0 → 0)
        out_ << "[](uint64_t n) { return n == 0 ? 0 : std::bit_width(n - 1); }("
                "static_cast<uint64_t>(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        out_ << "))";
      } else if (common::HasDirectCppMapping(syscall.name)) {
        // Math functions with direct C++ mapping (using registry)
        const auto* info = common::FindSystemFunction(syscall.name);
        out_ << info->cpp_function << "(";
        EmitExpression(*syscall.arguments[0], kPrecLowest);
        if (info->category == common::SystemFunctionCategory::kMathBinary) {
          out_ << ", ";
          EmitExpression(*syscall.arguments[1], kPrecLowest);
        }
        out_ << ")";
      } else {
        // System tasks like $display, $finish are handled in statement context
        throw common::InternalError(
            "codegen", "unexpected system call in expression: " + syscall.name);
      }
      break;
    }
    case mir::Expression::Kind::kConcatenation: {
      const auto& concat = mir::As<mir::ConcatenationExpression>(expr);

      // String concatenation: emit (op0 + op1 + ...)
      if (expr.type.kind == common::Type::Kind::kString) {
        out_ << "(";
        for (size_t i = 0; i < concat.operands.size(); ++i) {
          if (i > 0) {
            out_ << " + ";
          }
          EmitExpression(*concat.operands[i], kPrecLowest);
        }
        out_ << ")";
        break;
      }

      // Integral concatenation
      size_t result_width = expr.type.GetBitWidth();

      // Debug assertion: sum of operand widths must equal result width
      [[maybe_unused]] size_t operand_width_sum = 0;
      for (const auto& op : concat.operands) {
        operand_width_sum += op->type.GetBitWidth();
      }
      assert(
          operand_width_sum == result_width &&
          "concatenation operand widths don't sum to result width");

      // SDK Concat<N>() handles both narrow and wide internally:
      // - Narrow (<=64 bits): returns uint64_t, handles sign-extension masking
      // - Wide (>64 bits): returns WideBit<N>
      out_ << "lyra::sdk::Concat<" << result_width << ">(";
      for (size_t i = 0; i < concat.operands.size(); ++i) {
        if (i > 0) {
          out_ << ", ";
        }
        EmitExpression(*concat.operands[i], kPrecLowest);
      }
      out_ << ")";
      break;
    }
    case mir::Expression::Kind::kReplication: {
      const auto& rep = mir::As<mir::ReplicationExpression>(expr);

      // String replication: emit helper function
      if (expr.type.kind == common::Type::Kind::kString) {
        out_ << "lyra::sdk::ReplicateString(";
        EmitExpression(*rep.operand, kPrecLowest);
        out_ << ", " << rep.count << ")";
        break;
      }

      // Integral replication
      size_t result_width = expr.type.GetBitWidth();

      // SDK Replicate<ResultWidth, Count>(value) expands to Concat internally
      out_ << "lyra::sdk::Replicate<" << result_width << ", " << rep.count
           << ">(";
      EmitExpression(*rep.operand, kPrecLowest);
      out_ << ")";
      break;
    }
    case mir::Expression::Kind::kFunctionCall: {
      const auto& call = mir::As<mir::FunctionCallExpression>(expr);
      out_ << call.function_name << "(";
      for (size_t i = 0; i < call.arguments.size(); ++i) {
        if (i > 0) {
          out_ << ", ";
        }
        EmitExpression(*call.arguments[i], kPrecLowest);
      }
      out_ << ")";
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
    EmitHierarchicalPath(target.instance_path, target.target_symbol);
    return;
  }

  out_ << target.symbol->name;
  // Append underscore for port reference members (Google style)
  if (port_symbols_.contains(target.symbol)) {
    out_ << "_";
  }
  if (target.IsElementSelect()) {
    // For non-zero-based unpacked arrays, subtract lower_bound from first index
    // TODO(hankhsu): Handle multi-dimensional non-zero-based arrays properly
    int32_t lower_bound = 0;
    if (target.base_type.has_value() &&
        target.base_type->kind == common::Type::Kind::kUnpackedArray) {
      lower_bound = target.base_type->GetElementLower();
    }

    // Emit all indices (for multi-dimensional arrays)
    bool first = true;
    for (const auto& idx : target.indices) {
      out_ << "[static_cast<size_t>(";
      // Apply lower_bound adjustment only for first index (1D case)
      if (first && lower_bound != 0) {
        out_ << "static_cast<int>(";
      }
      EmitExpression(*idx, kPrecLowest);
      if (first && lower_bound != 0) {
        out_ << ") - " << lower_bound;
      }
      out_ << ")]";
      first = false;
    }
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

void Codegen::EmitCompositePackedBitPosition(
    const std::vector<std::unique_ptr<mir::Expression>>& indices,
    const common::Type& base_type) {
  // For multi-dimensional packed arrays like bit[A][B][C] with indices [i][j]:
  // Composite index = i * B + j
  // Bit position = composite_index * result_element_width
  size_t element_width = GetElementWidthAfterIndices(base_type, indices.size());
  int32_t lower_bound = base_type.GetElementLower();

  if (indices.size() == 1) {
    // Single index - delegate to existing method
    EmitPackedBitPosition(*indices[0], lower_bound, element_width);
    return;
  }

  // Multiple indices - emit composite index calculation
  // ((idx0 * count1 + idx1) * count2 + idx2) * ... * element_width
  out_ << "(";

  // Build up the composite index
  const common::Type* current = &base_type;
  for (size_t i = 0; i < indices.size(); ++i) {
    if (i > 0) {
      out_ << " + ";
    }
    if (i < indices.size() - 1) {
      out_ << "(";
    }

    // Emit this index
    out_ << "static_cast<size_t>(";
    EmitExpression(*indices[i], kPrecLowest);
    out_ << ")";

    // Multiply by remaining dimensions' sizes
    const common::Type* temp = current;
    for (size_t j = i + 1; j < indices.size(); ++j) {
      temp = &temp->GetElementType();
      out_ << " * " << temp->GetElementCount();
    }

    if (i < indices.size() - 1) {
      out_ << ")";
    }
    current = &current->GetElementType();
  }

  // Adjust for outermost lower bound (TODO: handle per-dimension bounds)
  if (lower_bound != 0) {
    out_ << " - " << lower_bound;
  }

  // Multiply by element width
  out_ << ") * " << element_width;
}

void Codegen::EmitSliceExtract(
    const common::Type& result_type, const mir::Expression& value,
    const std::function<void()>& emit_shift, uint64_t mask, bool is_wide) {
  // Emit slice extraction: static_cast<ResultType>(...shift & mask...)
  // For wide types: shift the WideBit first, then cast to uint64_t
  // For normal types: cast to uint64_t first, then shift
  // This is because WideBit shift accesses all words, while uint64_t truncates.
  out_ << "static_cast<" << ToCppType(result_type) << ">(";
  if (is_wide) {
    out_ << "static_cast<uint64_t>(";
    EmitExpression(value, kPrecLowest);
    out_ << " >> ";
    emit_shift();
    out_ << ")";
  } else {
    out_ << "(static_cast<uint64_t>(";
    EmitExpression(value, kPrecLowest);
    out_ << ") >> ";
    emit_shift();
    out_ << ")";
  }
  out_ << " & " << std::format("0x{:X}ULL", mask) << ")";
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

void Codegen::EmitHierarchicalPath(
    const std::vector<mir::SymbolRef>& instance_path,
    mir::SymbolRef target_symbol) {
  // Emit hierarchical path: [child_sym], value_sym -> child_.value
  // Instance names get _ suffix, target variable does not
  for (const auto& inst_sym : instance_path) {
    out_ << inst_sym->name << "_.";
  }
  out_ << target_symbol->name;
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

auto Codegen::GetTriggerPath(const common::Trigger& trigger) const
    -> std::string {
  std::string path;
  // Emit instance path: instance symbols get _ suffix
  for (const auto& inst_sym : trigger.instance_path) {
    path += std::string(inst_sym->name) + "_.";
  }
  // Emit variable name
  path += trigger.variable->name;
  // Append _ for port reference members
  if (port_symbols_.contains(trigger.variable)) {
    path += "_";
  }
  return path;
}

void Codegen::EmitFormattedPrint(
    const std::vector<std::unique_ptr<mir::Expression>>& arguments,
    size_t first_arg_idx, const std::string& sv_fmt, std::string_view print_fn,
    char default_format) {
  if (!sv_fmt.empty()) {
    // Has format string - transform and emit with special handling
    auto cpp_fmt = common::TransformToStdFormat(sv_fmt);
    auto specs = common::ParseDisplayFormat(sv_fmt);
    auto needs_cast = common::NeedsIntCast(sv_fmt);
    Indent();
    out_ << print_fn << "(std::cout, \"" << cpp_fmt << "\"";

    // Emit arguments - for %t, wrap with FormatTimeValue; for width specs, cast
    for (size_t i = 0; i < specs.size(); ++i) {
      out_ << ", ";
      if (specs[i].spec == 't') {
        used_features_ |= CodegenFeature::kModuleUnitPower;
        out_ << "lyra::sdk::FormatTimeValue(";
        EmitExpression(*arguments[first_arg_idx + i]);
        out_ << ".Value(), kModuleUnitPower)";
      } else if (i < needs_cast.size() && needs_cast[i]) {
        out_ << "static_cast<int64_t>(";
        EmitExpression(*arguments[first_arg_idx + i]);
        out_ << ")";
      } else {
        EmitExpression(*arguments[first_arg_idx + i]);
      }
    }
    out_ << ");\n";
  } else {
    // No format string - generate default format from arg types
    std::string fmt_str;
    for (size_t i = first_arg_idx; i < arguments.size(); ++i) {
      if (arguments[i]->type.kind == common::Type::Kind::kIntegral) {
        fmt_str += "{:";
        fmt_str += default_format;
        fmt_str += "}";
      } else {
        fmt_str += "{}";
      }
    }

    Indent();
    out_ << print_fn << "(std::cout, \"" << fmt_str << "\"";
    for (size_t i = first_arg_idx; i < arguments.size(); ++i) {
      out_ << ", ";
      EmitExpression(*arguments[i]);
    }
    out_ << ");\n";
  }
}

auto Codegen::GeneratePackages(
    const std::vector<std::unique_ptr<mir::Package>>& packages) -> std::string {
  out_.str("");
  indent_ = 0;
  used_type_aliases_ = TypeAlias::kNone;
  user_type_aliases_.clear();

  // Generate package namespaces
  for (const auto& pkg : packages) {
    Line(std::format("namespace {} {{", pkg->name));
    indent_++;

    for (const auto& type_decl : pkg->types) {
      // Emit typedef
      std::string cpp_type = ToCppType(type_decl.type);
      Line(std::format("using {} = {};", type_decl.name, cpp_type));

      // If enum, emit enum constants as constexpr values
      for (const auto& member : type_decl.members) {
        Line(
            std::format(
                "inline constexpr {} {}{{{}}};", type_decl.name, member.name,
                member.value));
      }
    }

    // Emit package variables
    for (const auto& var : pkg->variables) {
      std::string type_str = ToCppType(var.variable.type);
      Indent();
      out_ << "inline " << type_str << " " << var.variable.symbol->name;
      if (var.initializer) {
        out_ << "{";
        EmitExpression(*var.initializer);
        out_ << "}";
      } else {
        out_ << "{}";
      }
      out_ << ";\n";
    }

    indent_--;
    Line(std::format("}}  // namespace {}", pkg->name));
    Line("");
  }

  // Generate header with all SDK type aliases at file scope.
  // We emit ALL aliases (not just those used by packages) because modules that
  // include packages.hpp will skip their own SDK aliases, relying on these.
  std::ostringstream header;
  header << "#pragma once\n\n";
  header << "#include <lyra/sdk/sdk.hpp>\n\n";

  // Always emit all SDK type aliases so modules can rely on them
  header << "template <std::size_t N, bool S = false>\n";
  header << "using Bit = lyra::sdk::Bit<N, S>;\n";
  header << "using Int = lyra::sdk::Int;\n";
  header << "using LongInt = lyra::sdk::LongInt;\n";
  header << "using ShortInt = lyra::sdk::ShortInt;\n";
  header << "using Byte = lyra::sdk::Byte;\n";
  header << "using Real = double;\n";
  header << "using ShortReal = float;\n";
  header << "\n";

  return header.str() + out_.str();
}

auto Codegen::GenerateModuleHeader(const mir::Module& module, bool has_packages)
    -> std::string {
  // When packages exist, skip SDK type aliases in the class - they're already
  // at file scope in packages.hpp
  std::string code = Generate(module, has_packages);

  std::ostringstream header;
  header << "#pragma once\n\n";
  if (has_packages) {
    header << "#include \"packages.hpp\"\n\n";
  }
  header << code;
  return header.str();
}

}  // namespace lyra::compiler
