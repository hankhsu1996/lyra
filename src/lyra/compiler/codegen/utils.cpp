#include "lyra/compiler/codegen/utils.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include "lyra/common/builtin_method.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/codegen/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::compiler {

namespace codegen {

namespace {

// C++ reserved keywords (C++23) - derived from cppreference.com/w/cpp/keyword
// and clang's TokenKinds.def.
// clang-format off
const std::unordered_set<std::string_view> kCppKeywords = {
    // C++ keywords
    "alignas", "alignof", "and", "and_eq", "asm", "auto",
    "bitand", "bitor", "bool", "break",
    "case", "catch", "char", "char8_t", "char16_t", "char32_t", "class",
    "compl", "concept", "const", "const_cast", "consteval", "constexpr",
    "constinit", "continue", "co_await", "co_return", "co_yield",
    "decltype", "default", "delete", "do", "double", "dynamic_cast",
    "else", "enum", "explicit", "export", "extern",
    "false", "float", "for", "friend",
    "goto",
    "if", "inline", "int",
    "long",
    "mutable",
    "namespace", "new", "noexcept", "not", "not_eq", "nullptr",
    "operator", "or", "or_eq",
    "private", "protected", "public",
    "register", "reinterpret_cast", "requires", "return",
    "short", "signed", "sizeof", "static", "static_assert", "static_cast",
    "struct", "switch",
    "template", "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename",
    "union", "unsigned", "using",
    "virtual", "void", "volatile",
    "wchar_t", "while",
    "xor", "xor_eq",
    // Identifiers with special meaning (context-sensitive keywords)
    "final", "override", "import", "module",
};
// clang-format on

}  // namespace

auto IsCppKeyword(std::string_view name) -> bool {
  return kCppKeywords.contains(name);
}

auto EscapeIdentifier(std::string_view name) -> std::string {
  // Appending underscore suffix follows Google Protocol Buffers convention.
  if (kCppKeywords.contains(name)) {
    return std::string(name) + "_";
  }
  return std::string(name);
}

auto EscapeIdentifier(
    std::string_view name,
    const std::unordered_set<std::string_view>& existing_names) -> std::string {
  if (!kCppKeywords.contains(name)) {
    return std::string(name);
  }

  // Try simple suffix first
  std::string escaped = std::string(name) + "_";
  if (!existing_names.contains(escaped)) {
    return escaped;
  }

  // Try double underscore
  escaped = std::string(name) + "__";
  if (!existing_names.contains(escaped)) {
    return escaped;
  }

  // Try numbered suffixes
  for (int i = 0; i < 100; ++i) {
    escaped = std::format("{}_{}_", name, i);
    if (!existing_names.contains(escaped)) {
      return escaped;
    }
  }

  // Should never happen in practice
  return std::format("{}_escaped_", name);
}

auto EscapeQualifiedName(std::string_view qualified_name) -> std::string {
  // Find the last "::" separator
  auto pos = qualified_name.rfind("::");
  if (pos == std::string_view::npos) {
    // No qualifier - escape the whole name
    return EscapeIdentifier(qualified_name);
  }
  // Has qualifier - escape only the function name part
  std::string_view qualifier =
      qualified_name.substr(0, pos + 2);  // Include "::"
  std::string_view name = qualified_name.substr(pos + 2);
  return std::string(qualifier) + EscapeIdentifier(name);
}

}  // namespace codegen

using codegen::GetElementWidthAfterIndices;
using codegen::kPrecLowest;

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
    const std::vector<mir::HierarchicalPathElement>& instance_path,
    mir::SymbolRef target_symbol) {
  // Emit hierarchical path: gen_block[0].signal -> gen_block_[0].signal
  // Instance names get _ suffix, target variable does not.
  //
  // Generate blocks have two path elements: the array (name="gen_block") and
  // the block instance (name="", index=0). We emit:
  // - name_ for non-empty symbol names (underscore suffix)
  // - [index] for array indices
  // - single dot before target
  for (const auto& elem : instance_path) {
    if (!elem.symbol->name.empty()) {
      out_ << Escape(elem.symbol->name) << "_";
    }
    if (elem.array_index) {
      out_ << "[" << *elem.array_index << "]";
    }
  }
  if (!instance_path.empty()) {
    out_ << ".";
  }
  out_ << Escape(target_symbol->name);
}

void Codegen::EmitHierarchicalPath(const std::vector<std::string>& path) {
  for (size_t i = 0; i < path.size(); ++i) {
    if (i > 0) {
      out_ << ".";
    }
    out_ << path[i];
  }
}

void Codegen::EmitMethodCall(const mir::MethodCallExpression& mc) {
  // Dispatch by receiver type using the builtin method registry
  const auto& receiver_type = mc.receiver->type;

  // Determine builtin type kind for registry lookup
  common::BuiltinTypeKind type_kind{};
  if (!mc.enum_members.empty()) {
    type_kind = common::BuiltinTypeKind::kEnum;
  } else if (receiver_type.IsDynamicArray()) {
    type_kind = common::BuiltinTypeKind::kDynamicArray;
  } else if (receiver_type.IsQueue()) {
    type_kind = common::BuiltinTypeKind::kQueue;
  } else {
    throw common::InternalError(
        "Codegen::EmitMethodCall",
        std::format(
            "unsupported receiver type '{}'", receiver_type.ToString()));
  }

  // Look up method in registry
  const auto* method_info =
      common::FindBuiltinMethod(type_kind, mc.method_name);
  if (method_info == nullptr) {
    throw common::InternalError(
        "Codegen::EmitMethodCall",
        std::format(
            "unknown method '{}' on type '{}'", mc.method_name,
            receiver_type.ToString()));
  }

  // Dispatch based on category from registry
  using Cat = common::BuiltinMethodCategory;
  switch (method_info->category) {
    case Cat::kEnumNext:
      EmitEnumNavMethod(mc, true);
      return;
    case Cat::kEnumPrev:
      EmitEnumNavMethod(mc, false);
      return;
    case Cat::kEnumName:
      EmitEnumNameMethod(mc);
      return;
    case Cat::kArrayQuery:
      // Use return_type to decide if cast is needed
      if (method_info->return_type == common::BuiltinMethodReturnType::kInt) {
        out_ << "static_cast<Int>(";
        EmitExpression(*mc.receiver, kPrecLowest);
        out_ << method_info->cpp_expr << ")";
      } else {
        EmitExpression(*mc.receiver, kPrecLowest);
        out_ << method_info->cpp_expr;
      }
      return;
    case Cat::kArrayMutate:
      // For queue delete with optional index
      if (type_kind == common::BuiltinTypeKind::kQueue) {
        if (mc.args.empty()) {
          // delete() - clear all
          EmitExpression(*mc.receiver, kPrecLowest);
          out_ << ".clear()";
        } else {
          // delete(index) - erase at position
          EmitExpression(*mc.receiver, kPrecLowest);
          out_ << ".erase(";
          EmitExpression(*mc.receiver, kPrecLowest);
          out_ << ".begin() + ";
          EmitExpression(*mc.args[0], kPrecLowest);
          out_ << ")";
        }
      } else {
        // Dynamic array delete - always .clear()
        EmitExpression(*mc.receiver, kPrecLowest);
        out_ << method_info->cpp_expr;
      }
      return;
    case Cat::kQueuePush:
      // push_back(item) or push_front(item)
      EmitExpression(*mc.receiver, kPrecLowest);
      out_ << "." << mc.method_name << "(";
      EmitExpression(*mc.args[0], kPrecLowest);
      out_ << ")";
      return;
    case Cat::kQueuePop:
      // pop_front() or pop_back() - need to emit a lambda that saves, pops,
      // returns
      out_ << "[&]() { auto& _q_ = ";
      EmitExpression(*mc.receiver, kPrecLowest);
      out_ << "; auto _v_ = _q_."
           << (mc.method_name == "pop_front" ? "front" : "back") << "(); _q_."
           << mc.method_name << "(); return _v_; }()";
      return;
    case Cat::kQueueInsert:
      // insert(index, item)
      EmitExpression(*mc.receiver, kPrecLowest);
      out_ << ".insert(";
      EmitExpression(*mc.receiver, kPrecLowest);
      out_ << ".begin() + ";
      EmitExpression(*mc.args[0], kPrecLowest);
      out_ << ", ";
      EmitExpression(*mc.args[1], kPrecLowest);
      out_ << ")";
      return;
  }
}

void Codegen::EmitEnumNavMethod(
    const mir::MethodCallExpression& mc, bool is_next) {
  // Get step from args (default 1 if no args)
  int64_t step = 1;
  if (!mc.args.empty()) {
    const auto& step_expr = mir::As<mir::LiteralExpression>(*mc.args[0]);
    step = step_expr.literal.value.AsInt64();
  }

  // Generate inline lambda with switch
  out_ << "[&]() -> " << ToCppType(mc.type)
       << " { switch (static_cast<int64_t>(";
  EmitExpression(*mc.receiver, kPrecLowest);
  out_ << ")) {";
  for (size_t i = 0; i < mc.enum_members.size(); ++i) {
    size_t target_idx = 0;
    if (is_next) {
      target_idx = (i + static_cast<size_t>(step)) % mc.enum_members.size();
    } else {
      target_idx = (i + mc.enum_members.size() -
                    (static_cast<size_t>(step) % mc.enum_members.size())) %
                   mc.enum_members.size();
    }
    out_ << " case " << mc.enum_members[i].value << ": return "
         << ToCppType(mc.type) << "{" << mc.enum_members[target_idx].value
         << "};";
  }
  out_ << " default: return " << ToCppType(mc.type) << "{};";
  out_ << " } }()";
}

void Codegen::EmitEnumNameMethod(const mir::MethodCallExpression& mc) {
  out_ << "[&]() -> std::string { switch (static_cast<int64_t>(";
  EmitExpression(*mc.receiver, kPrecLowest);
  out_ << ")) {";
  for (const auto& m : mc.enum_members) {
    out_ << " case " << m.value << ": return \"" << m.name << "\";";
  }
  out_ << " default: return \"\";";
  out_ << " } }()";
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
    path += Escape(inst_sym->name) + "_.";
  }
  // Emit variable name
  path += Escape(trigger.variable->name);
  // Append _ for port reference members
  if (port_symbols_.contains(trigger.variable)) {
    path += "_";
  }
  return path;
}

}  // namespace lyra::compiler
