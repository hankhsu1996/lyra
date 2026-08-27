#include "lyra/backend/llvm/runtime_abi.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::backend::llvm_backend {

auto ValueDomainName(ValueDomain domain) -> std::string_view {
  switch (domain) {
    case ValueDomain::kPacked:
      return "packed";
    case ValueDomain::kString:
      return "string";
    case ValueDomain::kReal:
      return "real";
    case ValueDomain::kShortReal:
      return "shortreal";
    case ValueDomain::kChandle:
      return "chandle";
    case ValueDomain::kTuple:
      return "tuple";
    case ValueDomain::kDynArray:
      return "dynarray";
    case ValueDomain::kUnpackedArray:
      return "unpackedarray";
  }
  throw InternalError("llvm codegen: unknown value domain");
}

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<ValueDomain> {
  using Domain = std::optional<ValueDomain>;
  return std::visit(
      Overloaded{
          [](const lir::PackedArrayType&) -> Domain {
            return ValueDomain::kPacked;
          },
          // An enumeration is a packed value at runtime; only its own entries,
          // which read its declared members, need more than that.
          [](const lir::EnumType&) -> Domain { return ValueDomain::kPacked; },
          [](const lir::StringType&) -> Domain { return ValueDomain::kString; },
          // `real` and `realtime` are one host-precision value (LRM 6.12.1);
          // `shortreal` is the single-precision one.
          [](const lir::RealType&) -> Domain { return ValueDomain::kReal; },
          [](const lir::RealTimeType&) -> Domain { return ValueDomain::kReal; },
          [](const lir::ShortRealType&) -> Domain {
            return ValueDomain::kShortReal;
          },
          // A chandle (LRM 6.14) is a pointer-sized value carried inline: the
          // domain's handle is the chandle value itself, not a reference to a
          // runtime-owned value object.
          [](const lir::ChandleType&) -> Domain {
            return ValueDomain::kChandle;
          },
          // An unpacked struct (LRM 7.2) is MIR's product type; its runtime
          // realization is a type-erased product value carried inline behind an
          // opaque handle, like every other value domain.
          [](const lir::TupleType&) -> Domain { return ValueDomain::kTuple; },
          // A dynamic array (LRM 7.5) is MIR's `DynamicArrayType`; its runtime
          // realization is a type-erased container carried behind an opaque
          // handle, like every other value domain.
          [](const lir::DynamicArrayType&) -> Domain {
            return ValueDomain::kDynArray;
          },
          // A fixed-size unpacked array (LRM 7.4.2) is MIR's
          // `UnpackedArrayType`; its runtime realization is a type-erased
          // container carried behind an opaque handle, like every other value
          // domain. The declared range is not part of it -- the coordinate
          // system is the receiver's static type and arrives at a select as an
          // operand, so the payload is ordinal-only.
          [](const lir::UnpackedArrayType&) -> Domain {
            return ValueDomain::kUnpackedArray;
          },
          [](const auto&) -> Domain { return std::nullopt; }},
      unit.types.Get(type).data);
}

RuntimeAbi::RuntimeAbi(
    llvm::Module& module, llvm::LLVMContext& ctx, CodeGenTypes& types)
    : module_(&module), ctx_(&ctx), types_(&types) {
}

auto RuntimeAbi::Get(
    const char* name, llvm::Type* result, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return module_->getOrInsertFunction(
      name, llvm::FunctionType::get(result, params, false));
}

auto RuntimeAbi::Get(
    const std::string& name, llvm::Type* result,
    llvm::ArrayRef<llvm::Type*> params) -> llvm::FunctionCallee {
  return Get(name.c_str(), result, params);
}

auto RuntimeAbi::CurrentServices() -> llvm::FunctionCallee {
  return Get("lyra_rt_current_runtime", types_->Ptr(), {});
}

auto RuntimeAbi::Files() -> llvm::FunctionCallee {
  return Get("lyra_rt_files", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::TimeFormat() -> llvm::FunctionCallee {
  return Get("lyra_rt_time_format", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::Format() -> llvm::FunctionCallee {
  return Get("lyra_rt_format", types_->Ptr(), {types_->Span(), types_->Ptr()});
}

auto RuntimeAbi::Writeln() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_writeln", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Write() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_write", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Diagnostic() -> llvm::FunctionCallee {
  return Get("lyra_rt_diagnostic", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::EmitInfo() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_emit_info", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::EmitWarning() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_emit_warning", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::EmitError() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_emit_error", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::EmitFatal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_emit_fatal", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RegisterInitial() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_initial", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RegisterFinal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_final", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeCoroutine() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_coroutine", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Delay() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_delay", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::WaitAny() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_wait_any", types_->Void(), {types_->Ptr(), types_->Span()});
}

auto RuntimeAbi::Finish() -> llvm::FunctionCallee {
  return Get("lyra_rt_finish", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FatalFinish() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_fatal_finish", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RunHostCommand(std::size_t argument_count)
    -> llvm::FunctionCallee {
  if (argument_count == 0) {
    return Get("lyra_rt_run_null_host_command", types_->Ptr(), {});
  }
  if (argument_count == 2) {
    return Get(
        "lyra_rt_run_host_command", types_->Ptr(),
        {types_->Ptr(), types_->Ptr()});
  }
  throw InternalError(
      "llvm codegen: a host command call carries a command line or nothing");
}

auto RuntimeAbi::Urandom() -> llvm::FunctionCallee {
  return Get("lyra_rt_urandom", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::UrandomSeeded() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_urandom_seeded", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::UrandomRange() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_urandom_range", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeTrigger() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_trigger", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeString() -> llvm::FunctionCallee {
  return Get("lyra_rt_make_string", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::MakePrintLiteralItem() -> llvm::FunctionCallee {
  return Get("lyra_rt_make_print_literal_item", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::PackedConst() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_packed_const", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt64Ty(*ctx_), types_->Ptr(),
       llvm::Type::getInt64Ty(*ctx_), types_->Ptr(),
       llvm::Type::getInt64Ty(*ctx_), llvm::Type::getInt1Ty(*ctx_),
       llvm::Type::getInt1Ty(*ctx_)});
}

auto RuntimeAbi::RealConst(ValueDomain domain) -> llvm::FunctionCallee {
  llvm::Type* host = domain == ValueDomain::kShortReal
                         ? llvm::Type::getFloatTy(*ctx_)
                         : llvm::Type::getDoubleTy(*ctx_);
  return Get(
      std::format("lyra_rt_{}_const", ValueDomainName(domain)), types_->Ptr(),
      {host});
}

auto RuntimeAbi::RealFromInt(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_from_int64", ValueDomainName(domain)),
      types_->Ptr(), {llvm::Type::getInt64Ty(*ctx_)});
}

auto RuntimeAbi::RealReshape(ValueDomain dst, ValueDomain src)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_from_{}", ValueDomainName(dst), ValueDomainName(src)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::MakeSegment() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_segment", types_->Ptr(), {types_->Ptr(), types_->Span()});
}

auto RuntimeAbi::MakeScope() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_scope", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::HierarchicalPath() -> llvm::FunctionCallee {
  return Get("lyra_rt_hierarchical_path", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::AddOwnedChild() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_add_owned_child", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MemberAddress() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_member_addr", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt32Ty(*ctx_)});
}

auto RuntimeAbi::CellGet(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_cell_{}_get", ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::CellInitialize(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_cell_{}_initialize", ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::CellSet(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_cell_{}_set", ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::ActivationFrameAlloc(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_activation_frame_alloc_{}", ValueDomainName(domain)),
      types_->Ptr(), {});
}

auto RuntimeAbi::ActivationFrameStore(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_activation_frame_store_{}", ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::ActivationFrameLoad(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_activation_frame_load_{}", ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::RegisterSignal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_signal", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Binary(ValueDomain domain, lir::BinaryOp op)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", ValueDomainName(domain), lir::BinaryOpName(op)),
      types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Unary(ValueDomain domain, lir::UnaryOp op)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", ValueDomainName(domain), lir::UnaryOpName(op)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::ValueBuiltin(
    ValueDomain domain, lyra::support::BuiltinFn fn, llvm::Type* result,
    llvm::ArrayRef<llvm::Type*> params) -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", ValueDomainName(domain),
          lyra::support::BuiltinFnName(fn)),
      result, params);
}

auto RuntimeAbi::ToBool(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_to_bool", ValueDomainName(domain)),
      llvm::Type::getInt1Ty(*ctx_), {types_->Ptr()});
}

auto RuntimeAbi::ValueBox(ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_value_box_{}", ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::TupleMake() -> llvm::FunctionCallee {
  return Get("lyra_rt_tuple_make", types_->Ptr(), {types_->Span()});
}

auto RuntimeAbi::TupleExtract() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_tuple_extract", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt64Ty(*ctx_)});
}

// The entries a descent step resolves to. The value domain of the aggregate
// being descended into names each one, as it does for every value operation.
// The write halves realize a step's update, so no source-level construct names
// them and they are not part of the builtin namespace HIR and MIR share.
auto RuntimeAbi::ElementExtract(
    ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return ValueBuiltin(
      domain, lyra::support::BuiltinFn::kElement, types_->Ptr(), params);
}

auto RuntimeAbi::SliceExtract(
    ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return ValueBuiltin(
      domain, lyra::support::BuiltinFn::kSlice, types_->Ptr(), params);
}

auto RuntimeAbi::ElementUpdate(
    ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_with_element", ValueDomainName(domain)),
      types_->Ptr(), params);
}

auto RuntimeAbi::SliceUpdate(
    ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_with_slice", ValueDomainName(domain)),
      types_->Ptr(), params);
}

auto RuntimeAbi::TupleUpdate() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_tuple_update", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt64Ty(*ctx_), types_->Ptr()});
}

auto RuntimeAbi::MakeDynamicArrayDefault() -> llvm::FunctionCallee {
  return Get("lyra_rt_dynarray_default", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::MakeDynamicArrayNew() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dynarray_new", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeDynamicArrayNewCopy() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dynarray_new_copy", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeDynamicArrayFromLiteral(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_dynarray_from_literal_{}", ValueDomainName(domain)),
      types_->Ptr(),
      {types_->Ptr(), types_->Span(), llvm::Type::getInt64Ty(*ctx_)});
}

// A fixed-size array is built from a repeat unit and a count (LRM 10.9.1 /
// Table 7-1), so its literal entry takes one more argument than the dynamic
// array's: the unit's storage, then how many times it is laid down.
auto RuntimeAbi::MakeUnpackedArrayFromLiteral(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_unpackedarray_from_literal_{}", ValueDomainName(domain)),
      types_->Ptr(),
      {types_->Ptr(), types_->Span(), llvm::Type::getInt64Ty(*ctx_)});
}

auto RuntimeAbi::MakeFormatSpec(std::size_t field_count)
    -> llvm::FunctionCallee {
  if (field_count == 1) {
    return Get(
        "lyra_rt_make_format_spec_of_kind", types_->Ptr(), {types_->Ptr()});
  }
  if (field_count == 6) {
    return Get(
        "lyra_rt_make_format_spec", types_->Ptr(),
        {types_->Ptr(), types_->Ptr(), types_->Ptr(), types_->Ptr(),
         types_->Ptr(), types_->Ptr()});
  }
  throw InternalError(
      "llvm codegen: a format specification is built from a kind or from every "
      "field");
}

auto RuntimeAbi::MakePrintValueItem(ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_make_print_value_item_{}", ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

}  // namespace lyra::backend::llvm_backend
