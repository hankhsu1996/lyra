#include "lyra/backend/llvm/runtime_abi.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>

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
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain> {
  using Domain = std::optional<support::ValueDomain>;
  return std::visit(
      Overloaded{
          [](const lir::PackedArrayType&) -> Domain {
            return support::ValueDomain::kPacked;
          },
          // An enumeration is a packed value at runtime; only its own entries,
          // which read its declared members, need more than that.
          [](const lir::EnumType&) -> Domain {
            return support::ValueDomain::kPacked;
          },
          [](const lir::StringType&) -> Domain {
            return support::ValueDomain::kString;
          },
          // `real` and `realtime` are one host-precision value (LRM 6.12.1);
          // `shortreal` is the single-precision one.
          [](const lir::RealType&) -> Domain {
            return support::ValueDomain::kReal;
          },
          [](const lir::RealTimeType&) -> Domain {
            return support::ValueDomain::kReal;
          },
          [](const lir::ShortRealType&) -> Domain {
            return support::ValueDomain::kShortReal;
          },
          // A chandle (LRM 6.14) is a pointer-sized value carried inline: the
          // domain's handle is the chandle value itself, not a reference to a
          // runtime-owned value object.
          [](const lir::ChandleType&) -> Domain {
            return support::ValueDomain::kChandle;
          },
          // An unpacked struct (LRM 7.2) is MIR's product type; its runtime
          // realization is a type-erased product value carried inline behind an
          // opaque handle, like every other value domain.
          [](const lir::TupleType&) -> Domain {
            return support::ValueDomain::kTuple;
          },
          // A dynamic array (LRM 7.5) is MIR's `DynamicArrayType`; its runtime
          // realization is a type-erased container carried behind an opaque
          // handle, like every other value domain.
          [](const lir::DynamicArrayType&) -> Domain {
            return support::ValueDomain::kDynArray;
          },
          // A fixed-size unpacked array (LRM 7.4.2) is MIR's
          // `UnpackedArrayType`; its runtime realization is a type-erased
          // container carried behind an opaque handle, like every other value
          // domain. The declared range is not part of it -- the coordinate
          // system is the receiver's static type and arrives at a select as an
          // operand, so the payload is ordinal-only.
          [](const lir::UnpackedArrayType&) -> Domain {
            return support::ValueDomain::kUnpackedArray;
          },
          // A queue (LRM 7.10) is MIR's `QueueType`; its runtime realization is
          // a type-erased container carried behind an opaque handle, like every
          // other value domain. Its declared bound is a fact of the type that
          // reaches a construction and a store as an operand, so the payload
          // carries it rather than the domain distinguishing a bounded queue
          // from an unbounded one.
          [](const lir::QueueType&) -> Domain {
            return support::ValueDomain::kQueue;
          },
          // An associative array (LRM 7.8) is MIR's `AssociativeArrayType`;
          // its runtime realization is a type-erased keyed container carried
          // behind an opaque handle. Its index type is not part of the domain:
          // an index reaches every operation as a value of its own, and the
          // order two indices sit in is read from the indices themselves.
          [](const lir::AssociativeArrayType&) -> Domain {
            return support::ValueDomain::kAssocArray;
          },
          [](const auto&) -> Domain { return std::nullopt; }},
      unit.types.Get(type).data);
}

auto DeclaredIndexType(const lir::CompilationUnit& unit, lir::TypeId container)
    -> std::optional<lir::TypeId> {
  const auto* associative =
      std::get_if<lir::AssociativeArrayType>(&unit.types.Get(container).data);
  if (associative == nullptr) {
    return std::nullopt;
  }
  return associative->key_type;
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

auto RuntimeAbi::SetTimeFormat() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_set_time_format", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr(), types_->Ptr(),
       types_->Ptr()});
}

auto RuntimeAbi::ResetTimeFormat() -> llvm::FunctionCallee {
  return Get("lyra_rt_reset_time_format", types_->Void(), {types_->Ptr()});
}

auto RuntimeAbi::FileOpen(std::size_t argument_count) -> llvm::FunctionCallee {
  if (argument_count == 2) {
    return Get(
        "lyra_rt_file_open", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
  }
  if (argument_count == 3) {
    return Get(
        "lyra_rt_file_open_mode", types_->Ptr(),
        {types_->Ptr(), types_->Ptr(), types_->Ptr()});
  }
  throw InternalError(
      "llvm codegen: a file open carries a mode or nothing beside its name");
}

auto RuntimeAbi::FileClose() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_close", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileGetc() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_getc", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileUngetc() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_ungetc", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileSeek() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_seek", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileRewind() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_rewind", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileTell() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_file_tell", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileEof() -> llvm::FunctionCallee {
  return Get("lyra_rt_file_eof", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::FileFlush(std::size_t argument_count) -> llvm::FunctionCallee {
  if (argument_count == 1) {
    return Get("lyra_rt_file_flush_all", types_->Void(), {types_->Ptr()});
  }
  if (argument_count == 2) {
    return Get(
        "lyra_rt_file_flush", types_->Void(), {types_->Ptr(), types_->Ptr()});
  }
  throw InternalError(
      "llvm codegen: a file flush carries a descriptor or nothing");
}

auto RuntimeAbi::CancellationFor() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_cancellation_for", types_->Ptr(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::IsCancelled() -> llvm::FunctionCallee {
  return Get("lyra_rt_is_cancelled", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::EnterTarget() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_enter_target", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::LeaveTarget() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_leave_target", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Disable() -> llvm::FunctionCallee {
  return Get("lyra_rt_disable", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::EffectNamesTarget() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_effect_names_target", types_->Ptr(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::InvalidatedTarget() -> llvm::FunctionCallee {
  return Get("lyra_rt_invalidated_target", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::HasInvalidatedTarget() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_has_invalidated_target", llvm::Type::getInt1Ty(*ctx_),
      {types_->Ptr()});
}

auto RuntimeAbi::SettleCancelled() -> llvm::FunctionCallee {
  return Get("lyra_rt_settle_cancelled", types_->Void(), {types_->Ptr()});
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

auto RuntimeAbi::MakeClosure() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_closure_make", types_->Ptr(), {types_->Ptr(), types_->Span()});
}

auto RuntimeAbi::ClosureCapture() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_closure_capture", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt32Ty(*ctx_)});
}

auto RuntimeAbi::SubmitNba() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_submit_nba", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::SubmitPostponed() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_submit_postponed", types_->Void(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::SubmitObserved() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_submit_observed", types_->Void(),
      {types_->Ptr(), types_->Ptr()});
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

auto RuntimeAbi::TestPlusargs() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_test_plusargs", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::SimTime() -> llvm::FunctionCallee {
  return Get("lyra_rt_sim_time", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::STime() -> llvm::FunctionCallee {
  return Get("lyra_rt_stime", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RealTime() -> llvm::FunctionCallee {
  return Get("lyra_rt_realtime", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
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

auto RuntimeAbi::Random() -> llvm::FunctionCallee {
  return Get("lyra_rt_random", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::DistUniform() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_uniform", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistNormal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_normal", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistExponential() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_exponential", types_->Ptr(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistPoisson() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_poisson", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistChiSquare() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_chi_square", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistT() -> llvm::FunctionCallee {
  return Get("lyra_rt_dist_t", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::DistErlang() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dist_erlang", types_->Ptr(),
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

auto RuntimeAbi::PackedFromWords() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_packed_from_words", types_->Ptr(),
      {types_->Span(), types_->Span(), types_->Ptr()});
}

auto RuntimeAbi::MakePackedRange() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_packed_range", types_->Ptr(),
      {llvm::Type::getInt64Ty(*ctx_), llvm::Type::getInt64Ty(*ctx_)});
}

auto RuntimeAbi::MakePackedType() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_packed_type", types_->Ptr(),
      {types_->Span(), llvm::Type::getInt1Ty(*ctx_),
       llvm::Type::getInt1Ty(*ctx_)});
}

auto RuntimeAbi::RealConst(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  llvm::Type* host = domain == support::ValueDomain::kShortReal
                         ? llvm::Type::getFloatTy(*ctx_)
                         : llvm::Type::getDoubleTy(*ctx_);
  return Get(
      std::format("lyra_rt_{}_const", support::ValueDomainName(domain)),
      types_->Ptr(), {host});
}

auto RuntimeAbi::RealFromInt(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_from_int64", support::ValueDomainName(domain)),
      types_->Ptr(), {llvm::Type::getInt64Ty(*ctx_)});
}

auto RuntimeAbi::RealReshape(support::ValueDomain dst, support::ValueDomain src)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_from_{}", support::ValueDomainName(dst),
          support::ValueDomainName(src)),
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

auto RuntimeAbi::ResolveVisibleChild() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_resolve_visible_child", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Span()});
}

auto RuntimeAbi::GetChild() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_get_child", types_->Ptr(),
      {types_->Ptr(), types_->Ptr(), types_->Span()});
}

auto RuntimeAbi::MemberAddress() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_member_addr", types_->Ptr(),
      {types_->Ptr(), llvm::Type::getInt32Ty(*ctx_)});
}

auto RuntimeAbi::CellGet(support::ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_cell_{}_get", support::ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::CellInitialize(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_cell_{}_initialize", support::ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::CellSet(support::ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_cell_{}_set", support::ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::ActivationFrameAlloc(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_activation_frame_alloc_{}",
          support::ValueDomainName(domain)),
      types_->Ptr(), {});
}

auto RuntimeAbi::ActivationFrameStore(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_activation_frame_store_{}",
          support::ValueDomainName(domain)),
      types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::ActivationFrameLoad(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_activation_frame_load_{}", support::ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::RegisterSignal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_signal", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::GetSignal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_get_signal", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Binary(support::ValueDomain domain, lir::BinaryOp op)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", support::ValueDomainName(domain),
          lir::BinaryOpName(op)),
      types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Unary(support::ValueDomain domain, lir::UnaryOp op)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", support::ValueDomainName(domain),
          lir::UnaryOpName(op)),
      types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::ValueBuiltin(
    support::ValueDomain domain, lyra::support::BuiltinFn fn,
    llvm::Type* result, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_{}_{}", support::ValueDomainName(domain),
          lyra::support::BuiltinFnName(fn)),
      result, params);
}

auto RuntimeAbi::ToBool(support::ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_to_bool", support::ValueDomainName(domain)),
      llvm::Type::getInt1Ty(*ctx_), {types_->Ptr()});
}

auto RuntimeAbi::ValueBox(support::ValueDomain domain) -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_value_box_{}", support::ValueDomainName(domain)),
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
    support::ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return ValueBuiltin(
      domain, lyra::support::BuiltinFn::kElement, types_->Ptr(), params);
}

auto RuntimeAbi::SliceExtract(
    support::ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return ValueBuiltin(
      domain, lyra::support::BuiltinFn::kSlice, types_->Ptr(), params);
}

auto RuntimeAbi::ElementUpdate(
    support::ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_with_element", support::ValueDomainName(domain)),
      types_->Ptr(), params);
}

auto RuntimeAbi::SliceUpdate(
    support::ValueDomain domain, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return Get(
      std::format("lyra_rt_{}_with_slice", support::ValueDomainName(domain)),
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

auto RuntimeAbi::MakeDynamicArrayFromLiteral() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_dynarray_from_literal", types_->Ptr(),
      {types_->Ptr(), types_->Span(), llvm::Type::getInt64Ty(*ctx_)});
}

// A fixed-size array is built from a repeat unit and a count (LRM 10.9.1 /
// Table 7-1): the unit's storage, then how many times it is laid down.
auto RuntimeAbi::MakeUnpackedArrayFromLiteral() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_unpackedarray_from_literal", types_->Ptr(),
      {types_->Ptr(), types_->Span(), llvm::Type::getInt64Ty(*ctx_)});
}

auto RuntimeAbi::MakeQueue(std::size_t argument_count) -> llvm::FunctionCallee {
  llvm::Type* count = llvm::Type::getInt64Ty(*ctx_);
  switch (argument_count) {
    case 1:
      return Get("lyra_rt_queue_default", types_->Ptr(), {types_->Ptr()});
    case 2:
      return Get(
          "lyra_rt_queue_default_bounded", types_->Ptr(),
          {types_->Ptr(), types_->Ptr()});
    case 3:
      return Get(
          "lyra_rt_queue_from_literal", types_->Ptr(),
          {types_->Ptr(), types_->Span(), count});
    case 4:
      return Get(
          "lyra_rt_queue_from_literal_bounded", types_->Ptr(),
          {types_->Ptr(), types_->Span(), count, types_->Ptr()});
    default:
      break;
  }
  throw InternalError(
      "llvm codegen: a queue is built empty or over an element list, either "
      "way with a declared bound or without one");
}

auto RuntimeAbi::MakeAssociativeArray(std::size_t argument_count)
    -> llvm::FunctionCallee {
  switch (argument_count) {
    case 1:
      return Get("lyra_rt_assocarray_default", types_->Ptr(), {types_->Ptr()});
    case 2:
      return Get(
          "lyra_rt_assocarray_from_entries", types_->Ptr(),
          {types_->Ptr(), types_->Span()});
    case 3:
      return Get(
          "lyra_rt_assocarray_from_entries_default", types_->Ptr(),
          {types_->Ptr(), types_->Span(), types_->Ptr()});
    default:
      break;
  }
  throw InternalError(
      "llvm codegen: an associative array is built empty or over a list of "
      "entries, either way with a stated miss value or without one");
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

auto RuntimeAbi::MakePrintValueItem(support::ValueDomain domain)
    -> llvm::FunctionCallee {
  return Get(
      std::format(
          "lyra_rt_make_print_value_item_{}", support::ValueDomainName(domain)),
      types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

}  // namespace lyra::backend::llvm_backend
