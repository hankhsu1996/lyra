#include "lyra/backend/llvm/runtime_abi.hpp"

#include <format>
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
  }
  throw InternalError("llvm codegen: unknown value domain");
}

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> ValueDomain {
  return std::visit(
      Overloaded{
          [](const lir::PackedArrayType&) { return ValueDomain::kPacked; },
          // An enumeration is a packed value at runtime; only its own entries,
          // which read its declared members, need more than that.
          [](const lir::EnumType&) { return ValueDomain::kPacked; },
          [](const lir::StringType&) { return ValueDomain::kString; },
          // `real` and `realtime` are one host-precision value (LRM 6.12.1);
          // `shortreal` is the single-precision one.
          [](const lir::RealType&) { return ValueDomain::kReal; },
          [](const lir::RealTimeType&) { return ValueDomain::kReal; },
          [](const lir::ShortRealType&) { return ValueDomain::kShortReal; },
          // A chandle (LRM 6.14) is a pointer-sized value carried inline: the
          // domain's handle is the chandle value itself, not a reference to a
          // runtime-owned value object.
          [](const lir::ChandleType&) { return ValueDomain::kChandle; },
          [](const auto&) -> ValueDomain {
            throw InternalError(
                "llvm codegen: value type has no runtime library domain");
          }},
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

auto RuntimeAbi::Services() -> llvm::FunctionCallee {
  return Get("lyra_rt_services", types_->Ptr(), {types_->Ptr()});
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

auto RuntimeAbi::RegisterInitial() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_initial", types_->Void(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RegisterFinal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_final", types_->Void(), {types_->Ptr(), types_->Ptr()});
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
      {llvm::Type::getInt64Ty(*ctx_), llvm::Type::getInt32Ty(*ctx_),
       llvm::Type::getInt1Ty(*ctx_), llvm::Type::getInt1Ty(*ctx_)});
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

auto RuntimeAbi::MakeUnit() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_unit", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr(), types_->Ptr()});
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
      types_->Void(), {types_->Ptr(), types_->Ptr(), types_->Ptr()});
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
