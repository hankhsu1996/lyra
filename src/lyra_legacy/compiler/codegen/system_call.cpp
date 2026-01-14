#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/string_utils.hpp"
#include "lyra/common/sv_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/codegen/format.hpp"
#include "lyra/compiler/codegen/utils.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::compiler {

using codegen::ExtractFormatString;
using codegen::IntegralConstantToString;
using codegen::kPrecEquality;

void Codegen::EmitVoidSystemCall(const mir::SystemCallExpression& syscall) {
  // Simulation control tasks: $finish, $stop, $exit
  if (syscall.name == "$finish" || syscall.name == "$stop" ||
      syscall.name == "$exit") {
    // Get level argument (default 1), $exit has no argument
    std::string level_str = "1";
    if (!syscall.arguments.empty()) {
      if (const auto* lit = dynamic_cast<const mir::ConstantExpression*>(
              syscall.arguments[0].get())) {
        level_str = std::to_string(lit->constant.value.AsInt64());
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
    return;
  }

  // Severity tasks: $fatal, $error, $warning, $info
  // Source location is in syscall metadata.
  // Arguments: $fatal: [finish_num, format_args...], others: [format_args...]
  if (syscall.name == "$fatal" || syscall.name == "$error" ||
      syscall.name == "$warning" || syscall.name == "$info") {
    used_features_ |= CodegenFeature::kDisplay;
    used_features_ |= CodegenFeature::kModuleName;

    // Extract arguments based on task type
    size_t arg_idx = 0;
    std::string finish_num_str = "1";  // Default for $fatal

    if (syscall.name == "$fatal" && !syscall.arguments.empty()) {
      // First argument is finish_number for $fatal
      if (const auto* lit = dynamic_cast<const mir::ConstantExpression*>(
              syscall.arguments[0].get())) {
        finish_num_str = std::to_string(lit->constant.value.AsInt64());
      }
      arg_idx = 1;
    }

    // Get file and line from metadata
    std::string file_str = syscall.source_file.value_or("");
    std::string line_str = std::to_string(syscall.source_line.value_or(0));

    // Determine severity string for non-fatal tasks
    std::string severity;
    if (syscall.name == "$error") {
      severity = "ERROR";
    } else if (syscall.name == "$warning") {
      severity = "WARNING";
    } else if (syscall.name == "$info") {
      severity = "INFO";
    }

    // Start generating the call
    Indent();
    if (syscall.name == "$fatal") {
      out_ << "co_await lyra::sdk::Fatal(" << finish_num_str << ", \""
           << common::EscapeForCppString(file_str) << "\", " << line_str
           << ", kModuleName";
    } else {
      out_ << "lyra::sdk::SeverityMessage(\"" << severity << "\", \""
           << common::EscapeForCppString(file_str) << "\", " << line_str
           << ", kModuleName";
    }

    // Add message argument if present (format_expr has the format string)
    if (syscall.format_expr) {
      out_ << ", ";
      auto fmt_info = ExtractFormatString(**syscall.format_expr);

      if (fmt_info.is_string_literal) {
        if (fmt_info.has_format_specifiers &&
            arg_idx < syscall.arguments.size()) {
          // Format string with arguments
          out_ << "std::format(\""
               << common::TransformToStdFormat(fmt_info.text) << "\"";
          for (size_t i = arg_idx; i < syscall.arguments.size(); ++i) {
            out_ << ", ";
            EmitExpression(*syscall.arguments[i], kPrecEquality);
          }
          out_ << ")";
        } else {
          // Plain string literal
          out_ << "\"" << common::EscapeForCppString(fmt_info.text) << "\"";
        }
      } else {
        // Non-literal expression - convert to string
        out_ << "std::to_string(";
        EmitExpression(**syscall.format_expr, kPrecEquality);
        out_ << ")";
      }
    }

    out_ << ");\n";
    return;
  }

  // Handle strobe variants - schedule to Postponed region
  if (syscall.name == "$strobe" || syscall.name == "$strobeb" ||
      syscall.name == "$strobeo" || syscall.name == "$strobeh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);

    // Wrap print in lambda scheduled to Postponed region
    Line("lyra::sdk::current_scheduler->SchedulePostponed([=, this]() {");
    indent_++;

    // Empty call - just print newline
    if (!syscall.format_expr && syscall.arguments.empty()) {
      Line("std::println(std::cout, \"\");");
      indent_--;
      Line("});");
      return;
    }

    // Extract format string info from format_expr
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }

    // Print prefix (string literal without format specifiers)
    if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
      if (syscall.arguments.empty()) {
        // String-only: emit directly and return
        Indent();
        out_ << "std::println(std::cout, \""
             << common::EscapeForCppString(fmt_info.text) << "\");\n";
        indent_--;
        Line("});");
        return;
      }
      // Has args: print prefix, then args with newline
      Indent();
      out_ << "std::print(std::cout, \""
           << common::EscapeForCppString(fmt_info.text) << "\");\n";
    }

    // Print arguments with format string
    std::string sv_fmt = fmt_info.has_format_specifiers ? fmt_info.text : "";
    if (!syscall.arguments.empty()) {
      EmitFormattedPrint(
          syscall.arguments, 0, sv_fmt, "std::println",
          common::RadixToChar(props->radix));
    } else {
      Line("std::println(std::cout, \"\");");
    }
    indent_--;
    Line("});");
    return;
  }

  // Handle monitor variants - register for value change tracking
  if (syscall.name == "$monitor" || syscall.name == "$monitorb" ||
      syscall.name == "$monitoro" || syscall.name == "$monitorh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);

    // Empty call - print newline once (no values to monitor for changes)
    if (!syscall.format_expr && syscall.arguments.empty()) {
      Line("std::println(std::cout, \"\");");
      return;
    }

    // Extract format string info from format_expr
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }
    std::string sv_fmt = fmt_info.has_format_specifiers ? fmt_info.text : "";
    std::string prefix_str =
        (fmt_info.is_string_literal && !fmt_info.has_format_specifiers)
            ? fmt_info.text
            : "";

    // Generate block scope for previous value captures
    Line("{");
    indent_++;

    // Capture initial values for each monitored argument
    std::vector<std::string> prev_names;
    for (size_t i = 0; i < syscall.arguments.size(); ++i) {
      auto prev_name = std::format("prev_{}", i);
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
      for (size_t i = 0; i < syscall.arguments.size(); ++i) {
        if (i > 0) {
          out_ << " || ";
        }
        // Use kPrecEquality so ternary expressions get parenthesized
        EmitExpression(*syscall.arguments[i], kPrecEquality);
        out_ << " != " << prev_names[i];
      }
    }
    out_ << ") {\n";
    indent_++;

    // Generate print statement inside the if block
    if (!prefix_str.empty() && syscall.arguments.empty()) {
      // String-only: emit directly
      Indent();
      out_ << "std::println(std::cout, \""
           << common::EscapeForCppString(prefix_str) << "\");\n";
    } else {
      if (!prefix_str.empty()) {
        Indent();
        out_ << "std::print(std::cout, \""
             << common::EscapeForCppString(prefix_str) << "\");\n";
      }
      if (!syscall.arguments.empty()) {
        EmitFormattedPrint(
            syscall.arguments, 0, sv_fmt, "std::println", default_format);
      } else {
        Line("std::println(std::cout, \"\");");
      }
    }

    // Update previous values
    for (size_t i = 0; i < syscall.arguments.size(); ++i) {
      Indent();
      out_ << prev_names[i] << " = ";
      EmitExpression(*syscall.arguments[i]);
      out_ << ";\n";
    }

    indent_--;
    Line("}");  // End if
    indent_--;
    Line("});");  // End lambda and SetMonitor call

    // Print immediately on first call (IEEE 1800 §21.2.3)
    if (!prefix_str.empty() && syscall.arguments.empty()) {
      // String-only: emit directly
      Indent();
      out_ << "std::println(std::cout, \""
           << common::EscapeForCppString(prefix_str) << "\");\n";
    } else {
      if (!prefix_str.empty()) {
        Indent();
        out_ << "std::print(std::cout, \""
             << common::EscapeForCppString(prefix_str) << "\");\n";
      }
      if (!syscall.arguments.empty()) {
        EmitFormattedPrint(
            syscall.arguments, 0, sv_fmt, "std::println", default_format);
      } else {
        Line("std::println(std::cout, \"\");");
      }
    }

    indent_--;
    Line("}");  // End block scope
    return;
  }

  // Handle $monitoron and $monitoroff
  if (syscall.name == "$monitoron") {
    Line("lyra::sdk::current_scheduler->SetMonitorEnabled(true);");
    return;
  }
  if (syscall.name == "$monitoroff") {
    Line("lyra::sdk::current_scheduler->SetMonitorEnabled(false);");
    return;
  }

  // Handle file monitor variants ($fmonitor)
  if (syscall.name == "$fmonitor" || syscall.name == "$fmonitorb" ||
      syscall.name == "$fmonitoro" || syscall.name == "$fmonitorh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);

    // arguments[0] = descriptor, arguments[1..] = values to monitor

    // Empty call (descriptor only) - print newline once
    if (!syscall.format_expr && syscall.arguments.size() == 1) {
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(";
      EmitExpression(*syscall.arguments[0]);
      out_ << ")}, \"\\n\");\n";
      return;
    }

    // Extract format string info from format_expr
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }
    std::string sv_fmt = fmt_info.has_format_specifiers ? fmt_info.text : "";
    std::string prefix_str =
        (fmt_info.is_string_literal && !fmt_info.has_format_specifiers)
            ? fmt_info.text
            : "";

    // Generate block scope for descriptor and previous value captures
    Line("{");
    indent_++;

    // Capture descriptor value as int32_t (explicit cast required since
    // Bit's operator T() is explicit, avoiding accidental use of operator bool)
    Indent();
    out_ << "auto fd_ = static_cast<int32_t>(";
    EmitExpression(*syscall.arguments[0]);
    out_ << ");\n";

    // Capture initial values for each monitored argument (starting at index 1)
    std::vector<std::string> prev_names;
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      auto prev_name = std::format("prev_{}", i - 1);
      prev_names.push_back(prev_name);
      Indent();
      out_ << "auto " << prev_name << " = ";
      EmitExpression(*syscall.arguments[i]);
      out_ << ";\n";
    }

    // Generate SetMonitor call with lambda and file descriptor
    Indent();
    out_ << "lyra::sdk::current_scheduler->SetMonitor([this, fd_";
    for (const auto& prev_name : prev_names) {
      out_ << ", " << prev_name;
    }
    out_ << "]() mutable {\n";
    indent_++;

    // Generate comparison: if (arg1 != prev_0 || arg2 != prev_1 || ...)
    Indent();
    out_ << "if (";
    if (prev_names.empty()) {
      out_ << "false";
    } else {
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        if (i > 1) {
          out_ << " || ";
        }
        EmitExpression(*syscall.arguments[i], kPrecEquality);
        out_ << " != " << prev_names[i - 1];
      }
    }
    out_ << ") {\n";
    indent_++;

    // Generate FWrite statement inside the if block
    if (!prefix_str.empty() && syscall.arguments.size() == 1) {
      // String-only
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(fd_)}, \""
           << common::EscapeForCppString(prefix_str) << "\\n\");\n";
    } else {
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(fd_)}, ";
      if (!prefix_str.empty()) {
        out_ << "\"" << common::EscapeForCppString(prefix_str) << "\" + ";
      }
      // Build format string for remaining args
      std::string fmt_str;
      if (!sv_fmt.empty()) {
        fmt_str = common::TransformToStdFormat(sv_fmt) + "\\n";
      } else {
        for (size_t i = 1; i < syscall.arguments.size(); ++i) {
          if (syscall.arguments[i]->type.kind ==
              common::Type::Kind::kIntegral) {
            fmt_str += "{:";
            fmt_str += default_format;
            fmt_str += "}";
          } else {
            fmt_str += "{}";
          }
        }
        fmt_str += "\\n";
      }
      out_ << "std::format(\"" << fmt_str << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
    }

    // Update previous values
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      Indent();
      out_ << prev_names[i - 1] << " = ";
      EmitExpression(*syscall.arguments[i]);
      out_ << ";\n";
    }

    indent_--;
    Line("}");  // End if
    indent_--;
    Line("}, static_cast<uint32_t>(fd_));");  // End lambda with file descriptor

    // Print immediately on first call (IEEE 1800 §21.2.3)
    if (!prefix_str.empty() && syscall.arguments.size() == 1) {
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(fd_)}, \""
           << common::EscapeForCppString(prefix_str) << "\\n\");\n";
    } else {
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(fd_)}, ";
      if (!prefix_str.empty()) {
        out_ << "\"" << common::EscapeForCppString(prefix_str) << "\" + ";
      }
      std::string fmt_str;
      if (!sv_fmt.empty()) {
        fmt_str = common::TransformToStdFormat(sv_fmt) + "\\n";
      } else {
        for (size_t i = 1; i < syscall.arguments.size(); ++i) {
          if (syscall.arguments[i]->type.kind ==
              common::Type::Kind::kIntegral) {
            fmt_str += "{:";
            fmt_str += default_format;
            fmt_str += "}";
          } else {
            fmt_str += "{}";
          }
        }
        fmt_str += "\\n";
      }
      out_ << "std::format(\"" << fmt_str << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
    }

    indent_--;
    Line("}");  // End block scope
    return;
  }

  // Handle file output variants ($fdisplay, $fwrite)
  if (syscall.name == "$fdisplay" || syscall.name == "$fdisplayb" ||
      syscall.name == "$fdisplayo" || syscall.name == "$fdisplayh" ||
      syscall.name == "$fwrite" || syscall.name == "$fwriteb" ||
      syscall.name == "$fwriteo" || syscall.name == "$fwriteh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);
    bool append_newline = props->append_newline;

    // arguments[0] = descriptor, remaining = format args
    // format_expr = optional format string

    // Empty call (descriptor only) - just newline for $fdisplay
    if (!syscall.format_expr && syscall.arguments.size() == 1) {
      if (append_newline) {
        Indent();
        out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<"
                "uint32_t>(";
        EmitExpression(*syscall.arguments[0]);
        out_ << ")}, \"\\n\");\n";
      }
      // $fwrite with no content does nothing
      return;
    }

    // Extract format string info
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }

    // Build the FWrite call with formatted content
    Indent();
    out_
        << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_t>(";
    EmitExpression(*syscall.arguments[0]);
    out_ << ")}, ";

    // Format the content using std::format
    if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
      // Simple string literal (no format specifiers)
      if (syscall.arguments.size() == 1) {
        // String only
        out_ << "\"" << common::EscapeForCppString(fmt_info.text);
        if (append_newline) {
          out_ << "\\n";
        }
        out_ << "\");\n";
        return;
      }
      // String prefix + values - emit prefix + std::format for values
      std::string fmt_str;
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        if (syscall.arguments[i]->type.kind == common::Type::Kind::kIntegral) {
          fmt_str += "{:";
          fmt_str += default_format;
          fmt_str += "}";
        } else {
          fmt_str += "{}";
        }
      }
      if (append_newline) {
        fmt_str += "\\n";
      }
      out_ << "\"" << common::EscapeForCppString(fmt_info.text) << "\" + "
           << "std::format(\"" << fmt_str << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
      return;
    }

    if (fmt_info.has_format_specifiers) {
      // Format string with specifiers
      auto cpp_fmt = common::TransformToStdFormat(fmt_info.text);
      if (append_newline) {
        cpp_fmt += "\\n";
      }
      out_ << "std::format(\"" << cpp_fmt << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
      return;
    }

    // No format string - generate default format from arg types
    std::string fmt_str;
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      if (syscall.arguments[i]->type.kind == common::Type::Kind::kIntegral) {
        fmt_str += "{:";
        fmt_str += default_format;
        fmt_str += "}";
      } else {
        fmt_str += "{}";
      }
    }
    if (append_newline) {
      fmt_str += "\\n";
    }
    out_ << "std::format(\"" << fmt_str << "\"";
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      out_ << ", ";
      EmitExpression(*syscall.arguments[i]);
    }
    out_ << "));\n";
    return;
  }

  // Handle file strobe variants ($fstrobe) - schedule to Postponed region
  if (syscall.name == "$fstrobe" || syscall.name == "$fstrobeb" ||
      syscall.name == "$fstrobeo" || syscall.name == "$fstrobeh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);

    // arguments[0] = descriptor, remaining = format args
    // Wrap in lambda scheduled to Postponed region
    Line("lyra::sdk::current_scheduler->SchedulePostponed([=, this]() {");
    indent_++;

    // Empty call (descriptor only) - just newline for $fstrobe
    if (!syscall.format_expr && syscall.arguments.size() == 1) {
      Indent();
      out_ << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_"
              "t>(";
      EmitExpression(*syscall.arguments[0]);
      out_ << ")}, \"\\n\");\n";
      indent_--;
      Line("});");
      return;
    }

    // Extract format string info
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }

    // Build the FWrite call with formatted content
    Indent();
    out_
        << "lyra::sdk::FWrite(lyra::sdk::FileDescriptor{static_cast<uint32_t>(";
    EmitExpression(*syscall.arguments[0]);
    out_ << ")}, ";

    // Format the content using std::format
    if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
      // Simple string literal (no format specifiers)
      if (syscall.arguments.size() == 1) {
        // String only
        out_ << "\"" << common::EscapeForCppString(fmt_info.text)
             << "\\n\");\n";
        indent_--;
        Line("});");
        return;
      }
      // String prefix + values
      std::string fmt_str;
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        if (syscall.arguments[i]->type.kind == common::Type::Kind::kIntegral) {
          fmt_str += "{:";
          fmt_str += default_format;
          fmt_str += "}";
        } else {
          fmt_str += "{}";
        }
      }
      fmt_str += "\\n";
      out_ << "\"" << common::EscapeForCppString(fmt_info.text) << "\" + "
           << "std::format(\"" << fmt_str << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
      indent_--;
      Line("});");
      return;
    }

    if (fmt_info.has_format_specifiers) {
      // Format string with specifiers
      auto cpp_fmt = common::TransformToStdFormat(fmt_info.text) + "\\n";
      out_ << "std::format(\"" << cpp_fmt << "\"";
      for (size_t i = 1; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        EmitExpression(*syscall.arguments[i]);
      }
      out_ << "));\n";
      indent_--;
      Line("});");
      return;
    }

    // No format string - generate default format from arg types
    std::string fmt_str;
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      if (syscall.arguments[i]->type.kind == common::Type::Kind::kIntegral) {
        fmt_str += "{:";
        fmt_str += default_format;
        fmt_str += "}";
      } else {
        fmt_str += "{}";
      }
    }
    fmt_str += "\\n";
    out_ << "std::format(\"" << fmt_str << "\"";
    for (size_t i = 1; i < syscall.arguments.size(); ++i) {
      out_ << ", ";
      EmitExpression(*syscall.arguments[i]);
    }
    out_ << "));\n";
    indent_--;
    Line("});");
    return;
  }

  // Handle all display/write variants
  if (syscall.name == "$display" || syscall.name == "$displayb" ||
      syscall.name == "$displayo" || syscall.name == "$displayh" ||
      syscall.name == "$write" || syscall.name == "$writeb" ||
      syscall.name == "$writeo" || syscall.name == "$writeh") {
    used_features_ |= CodegenFeature::kDisplay;
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);
    bool append_newline = props->append_newline;
    std::string_view print_fn = append_newline ? "std::println" : "std::print";

    // Empty call - just print newline if needed
    if (!syscall.format_expr && syscall.arguments.empty()) {
      if (append_newline) {
        Line("std::println(std::cout, \"\");");
      }
      // $write with no args does nothing
      return;
    }

    // Extract format string info from format_expr
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
    }

    // Print prefix (string literal without format specifiers)
    if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
      if (syscall.arguments.empty()) {
        // String-only: emit directly and return
        Indent();
        out_ << print_fn << "(std::cout, \""
             << common::EscapeForCppString(fmt_info.text) << "\");\n";
        return;
      }
      // Has args: print prefix, then args with newline
      Indent();
      out_ << "std::print(std::cout, \""
           << common::EscapeForCppString(fmt_info.text) << "\");\n";
    }

    // Print remaining args with format string
    std::string sv_fmt = fmt_info.has_format_specifiers ? fmt_info.text : "";
    if (!syscall.arguments.empty()) {
      EmitFormattedPrint(
          syscall.arguments, 0, sv_fmt, print_fn, default_format);
    } else if (append_newline) {
      // No more args, but need newline
      Line("std::println(std::cout, \"\");");
    }
    return;
  }

  // Handle $sformat/$swrite* - string formatting tasks
  if (syscall.name == "$sformat" || syscall.name == "$swrite" ||
      syscall.name == "$swriteb" || syscall.name == "$swriteo" ||
      syscall.name == "$swriteh") {
    auto props = common::GetDisplayVariantProps(syscall.name);
    char default_format = common::RadixToChar(props->radix);

    if (syscall.output_targets.empty()) {
      throw common::InternalError("codegen", syscall.name + " requires output");
    }

    // Extract format string info if present
    common::FormatStringInfo fmt_info;
    if (syscall.format_expr) {
      fmt_info = ExtractFormatString(**syscall.format_expr);
      if (!fmt_info.is_string_literal) {
        throw common::InternalError(
            "codegen", syscall.name +
                           " requires literal format string; "
                           "use interpreter for runtime format strings");
      }
    }

    // Emit: output_var = std::format(...);
    Indent();
    out_ << Name(syscall.output_targets[0].symbol) << " = ";

    if (fmt_info.has_format_specifiers) {
      // Format string with specifiers
      auto cpp_fmt = common::TransformToStdFormat(fmt_info.text);
      auto needs_cast = common::NeedsIntCast(fmt_info.text);
      out_ << "std::format(\"" << common::EscapeForCppString(cpp_fmt) << "\"";
      for (size_t i = 0; i < syscall.arguments.size(); ++i) {
        out_ << ", ";
        if (i < needs_cast.size() && needs_cast[i]) {
          out_ << "static_cast<int64_t>(";
          EmitExpression(*syscall.arguments[i]);
          out_ << ")";
        } else {
          EmitExpression(*syscall.arguments[i]);
        }
      }
      out_ << ");\n";
    } else if (fmt_info.is_string_literal) {
      // String literal without format specifiers - prefix + formatted args
      if (syscall.arguments.empty()) {
        // Just the prefix string
        out_ << "\"" << common::EscapeForCppString(fmt_info.text) << "\";\n";
      } else {
        // Prefix + default-formatted args
        std::string fmt_str;
        for (size_t i = 0; i < syscall.arguments.size(); ++i) {
          if (syscall.arguments[i]->type.kind ==
              common::Type::Kind::kIntegral) {
            fmt_str += "{:";
            fmt_str += default_format;
            fmt_str += "}";
          } else {
            fmt_str += "{}";
          }
        }
        out_ << "\"" << common::EscapeForCppString(fmt_info.text) << "\" + "
             << "std::format(\"" << fmt_str << "\"";
        for (size_t i = 0; i < syscall.arguments.size(); ++i) {
          out_ << ", ";
          EmitExpression(*syscall.arguments[i]);
        }
        out_ << ");\n";
      }
    } else {
      // No format string - generate default format from arg types
      if (syscall.arguments.empty()) {
        // Empty string (handles $swrite(s) case)
        out_ << "\"\";\n";
      } else {
        std::string fmt_str;
        for (size_t i = 0; i < syscall.arguments.size(); ++i) {
          if (syscall.arguments[i]->type.kind ==
              common::Type::Kind::kIntegral) {
            fmt_str += "{:";
            fmt_str += default_format;
            fmt_str += "}";
          } else {
            fmt_str += "{}";
          }
        }
        out_ << "std::format(\"" << fmt_str << "\"";
        for (size_t i = 0; i < syscall.arguments.size(); ++i) {
          out_ << ", ";
          EmitExpression(*syscall.arguments[i]);
        }
        out_ << ");\n";
      }
    }
    return;
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
    return;
  }

  if (syscall.name == "$printtimescale") {
    // $printtimescale() - print current module's timescale
    used_features_ |= CodegenFeature::kDisplay;
    used_features_ |= CodegenFeature::kModuleName;
    used_features_ |= CodegenFeature::kTimescaleStr;
    Line(
        "std::println(std::cout, \"Time scale of ({}) is {}\", "
        "kModuleName, kTimescaleStr);");
    return;
  }

  if (syscall.name == "$printtimescale_root") {
    // $printtimescale($root) - print global precision (unit = precision)
    used_features_ |= CodegenFeature::kDisplay;
    Indent();
    out_ << "std::println(std::cout, \"Time scale of ($root) is {} / "
            "{}\", lyra::sdk::PowerToString(lyra::sdk::GlobalPrecisionPower"
            "()), lyra::sdk::PowerToString(lyra::sdk::GlobalPrecisionPower"
            "()));\n";
    return;
  }

  if (syscall.name == "$readmemh" || syscall.name == "$readmemb" ||
      syscall.name == "$writememh" || syscall.name == "$writememb") {
    used_features_ |= CodegenFeature::kMemIo;
    bool is_read = syscall.name == "$readmemh" || syscall.name == "$readmemb";
    bool is_hex = syscall.name == "$readmemh" || syscall.name == "$writememh";

    // After AST->MIR lowering refactor:
    // - arguments[0] = filename
    // - arguments[1] = start address (optional)
    // - arguments[2] = end address (optional)
    // - output_targets[0] = target array
    bool has_start = syscall.arguments.size() >= 2;
    bool has_end = syscall.arguments.size() == 3;

    if (syscall.arguments.empty() || syscall.arguments.size() > 3) {
      throw common::InternalError("codegen", "mem I/O expects 1-3 arguments");
    }
    if (syscall.output_targets.size() != 1) {
      throw common::InternalError("codegen", "mem I/O expects 1 output target");
    }

    const auto& target = syscall.output_targets[0];
    assert(target.base_type.has_value());
    const auto& target_type = *target.base_type;

    // Helper to emit filename as C++ string literal
    auto emit_filename = [&]() {
      const auto& filename_arg = *syscall.arguments[0];

      if (filename_arg.kind == mir::Expression::Kind::kConstant) {
        const auto& lit = mir::As<mir::ConstantExpression>(filename_arg);

        // String literal: extract string at codegen time, emit directly
        if (lit.constant.IsStringLiteral()) {
          std::string str;
          if (lit.constant.type.kind == common::Type::Kind::kString) {
            str = lit.constant.value.AsString();
          } else {
            str = IntegralConstantToString(lit.constant);
          }
          out_ << "\"" << common::EscapeForCppString(str) << "\"";
          return;
        }
      }

      // Default: emit expression directly (e.g., string variable)
      EmitExpression(filename_arg);
    };

    if (target_type.kind == common::Type::Kind::kUnpackedArray) {
      const auto& arr = std::get<common::UnpackedArrayData>(target_type.data);
      Indent();
      out_ << "lyra::sdk::" << (is_read ? "ReadMemArray(" : "WriteMemArray(")
           << Name(target.symbol) << ", " << arr.lower_bound << ", ";
      emit_filename();
      out_ << ", " << (has_start ? "true" : "false") << ", ";
      if (has_start) {
        EmitExpression(*syscall.arguments[1]);
      } else {
        out_ << "0";
      }
      out_ << ", " << (has_end ? "true" : "false") << ", ";
      if (has_end) {
        EmitExpression(*syscall.arguments[2]);
      } else {
        out_ << "0";
      }
      out_ << ", " << (is_hex ? "true" : "false") << ");\n";
      return;
    }

    if (target_type.kind == common::Type::Kind::kIntegral) {
      const auto& integral = std::get<common::IntegralData>(target_type.data);
      size_t element_width = integral.element_type != nullptr
                                 ? integral.element_type->GetBitWidth()
                                 : integral.bit_width;
      size_t element_count =
          integral.element_type != nullptr ? integral.element_count : 1;
      int32_t lower_bound = integral.element_lower;
      Indent();
      out_ << "lyra::sdk::" << (is_read ? "ReadMemPacked(" : "WriteMemPacked(")
           << Name(target.symbol) << ", " << element_width << ", "
           << element_count << ", " << lower_bound << ", ";
      emit_filename();
      out_ << ", " << (has_start ? "true" : "false") << ", ";
      if (has_start) {
        EmitExpression(*syscall.arguments[1]);
      } else {
        out_ << "0";
      }
      out_ << ", " << (has_end ? "true" : "false") << ", ";
      if (has_end) {
        EmitExpression(*syscall.arguments[2]);
      } else {
        out_ << "0";
      }
      out_ << ", " << (is_hex ? "true" : "false") << ");\n";
      return;
    }

    throw common::InternalError(
        "codegen", "mem I/O target must be an unpacked or packed array");
  }

  // File I/O
  if (syscall.name == "$fclose") {
    // Capture descriptor to avoid double evaluation
    Indent();
    out_ << "{\n";
    indent_++;
    Indent();
    out_ << "auto __fd = static_cast<uint32_t>(";
    EmitExpression(*syscall.arguments[0]);
    out_ << ");\n";
    Indent();
    out_ << "lyra::sdk::FClose(lyra::sdk::FileDescriptor{__fd});\n";
    // Cancel $fmonitor if its file descriptor matches (IEEE 1800 §21.3.1)
    Indent();
    out_ << "lyra::sdk::current_scheduler->CancelMonitorIfFileDescriptor(__fd);"
            "\n";
    indent_--;
    Indent();
    out_ << "}\n";
    return;
  }

  // Unrecognized system task - should have been rejected in AST-to-MIR lowering
  throw common::InternalError(
      "codegen", "unexpected system task: " + syscall.name);
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

}  // namespace lyra::compiler
