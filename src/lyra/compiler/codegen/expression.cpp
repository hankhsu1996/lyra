#include "lyra/mir/expression.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/codegen/type.hpp"
#include "lyra/compiler/codegen/utils.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::compiler {

using codegen::GetBinaryPrecedence;
using codegen::GetElementWidthAfterIndices;
using codegen::IsWideWidth;
using codegen::kPrecAssign;
using codegen::kPrecBitwiseXor;
using codegen::kPrecLowest;
using codegen::kPrecPrimary;
using codegen::kPrecShift;
using codegen::kPrecTernary;
using codegen::kPrecUnary;
using codegen::ToCppOperator;

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
    case mir::Expression::Kind::kMethodCall: {
      const auto& mc = mir::As<mir::MethodCallExpression>(expr);
      EmitMethodCall(mc);
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
        if (assign.target.IsPacked() && assign.target.IsElementSelect()) {
          throw common::InternalError(
              "Codegen", "NBA to packed array element not yet supported");
        }
        out_ << "this->ScheduleNba(&";
        EmitAssignmentTarget(assign.target);
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
        // Simple assignment: target = value
        // Check for procedural continuous assignment (assign/deassign)
        // If target is under procedural assign, the write is blocked
        out_ << "(HasProceduralAssign(&";
        EmitAssignmentTarget(assign.target);
        out_ << ") ? ";
        EmitAssignmentTarget(assign.target);
        out_ << " : (";
        EmitAssignmentTarget(assign.target);
        out_ << " = ";
        EmitExpression(*assign.value, kPrecAssign);
        out_ << "))";
      }
      break;
    }
    case mir::Expression::Kind::kElementSelect: {
      const auto& select = mir::As<mir::ElementSelectExpression>(expr);

      // Check if this is bit/element selection (bitvector type) or array access
      if (select.value->type.IsBitvector()) {
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
      // Packed structs return 0 from GetElementLower() (always 0-based)
      if (range.value->type.IsBitvector()) {
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

      // Invariant check: operand widths must sum to result width
      size_t operand_width_sum = 0;
      for (const auto& op : concat.operands) {
        operand_width_sum += op->type.GetBitWidth();
      }
      if (operand_width_sum != result_width) {
        throw common::InternalError(
            "codegen",
            fmt::format(
                "concatenation operand widths don't sum to result width: "
                "result={}, sum={}",
                result_width, operand_width_sum));
      }

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
      // function_name is already qualified for package functions (e.g.,
      // "MyPkg::add")
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

    case mir::Expression::Kind::kNewArray: {
      const auto& new_arr = mir::As<mir::NewArrayExpression>(expr);
      const auto& arr_data =
          std::get<common::DynamicArrayData>(new_arr.type.data);
      auto elem_type = ToCppType(*arr_data.element_type);

      if (new_arr.init_expr) {
        // new[size](init) - resize with copy semantics
        out_ << "lyra::sdk::DynArrayResize<" << elem_type
             << ">(static_cast<size_t>(";
        EmitExpression(*new_arr.size_expr, kPrecLowest);
        out_ << "), ";
        EmitExpression(*new_arr.init_expr, kPrecLowest);
        out_ << ")";
      } else {
        // new[size] - default initialized
        // Cast size to size_t to ensure we call the vector(size_type)
        // constructor
        out_ << "std::vector<" << elem_type << ">(static_cast<size_t>(";
        EmitExpression(*new_arr.size_expr, kPrecLowest);
        out_ << "))";
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

}  // namespace lyra::compiler
