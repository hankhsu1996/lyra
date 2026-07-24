#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The type a value has once it is marshaled to a DPI-C carrier (LRM 35.5.6,
// Table H.1). The carrier classifies the *formal*'s ABI; the value that crosses
// is an ordinary machine value, so it is typed as one -- an integer of the
// declared C width, a machine float, a borrowed C string, a raw pointer. A
// packed vector crosses in a canonical chunk buffer, which is a runtime library
// value. No type exists solely to mark a value as being at the boundary.
auto CarrierTypeId(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier)
    -> mir::TypeId {
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    return unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicBuffer
                                    : mir::RuntimeLibraryKind::kDpiBitBuffer}});
  }
  const auto machine_int = [&](std::uint32_t bits, mir::Signedness sign) {
    return unit.types.Intern(
        mir::TypeData{
            mir::MachineIntType{.bit_width = bits, .signedness = sign}});
  };
  switch (std::get<support::ScalarCarrier>(carrier).abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kLogicScalar:
      return machine_int(8, mir::Signedness::kUnsigned);
    case support::DpiScalarAbi::kByte:
      return machine_int(8, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kShortInt:
      return machine_int(16, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kInt:
      return machine_int(32, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kLongInt:
      return machine_int(64, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kReal:
      return unit.types.Intern(
          mir::TypeData{mir::MachineFloatType{.bit_width = 64}});
    case support::DpiScalarAbi::kString:
      return unit.types.Intern(mir::TypeData{mir::MachineCStringType{}});
    case support::DpiScalarAbi::kChandle:
      return unit.types.PointerTo(
          unit.builtins.void_type, mir::PointerOwnership::kBorrowed);
    case support::DpiScalarAbi::kVoid:
      return unit.builtins.void_type;
  }
  throw InternalError("CarrierTypeId: unknown DPI-C scalar ABI");
}

// SV value -> foreign ABI carrier (LRM 35.5.6). An integral value yields its
// widest machine integer, narrowed to the carrier's C width by a machine cast;
// a real yields its native floating value; a string borrows a NUL-terminated C
// string that stays valid for the call. Reuses the ordinary value accessors;
// the boundary conversion is a plain expression, not a DPI-specific primitive.
// Feeds both an input argument (crossed by value) and the copy-in of an inout /
// output argument (seeding its boundary temp).
auto MarshalSvToCarrier(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId sv_id,
    const support::DpiCarrier& carrier_desc) -> mir::ExprId {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalSvToCarrier: a canonical-vector carrier is not yet "
        "implemented");
  }
  const mir::TypeId carrier = CarrierTypeId(unit, carrier_desc);
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      const mir::ExprId machine_int = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kToInt64},
                      .arguments = {sv_id}},
              .type = unit.builtins.machine_int64});
      return block.exprs.Add(
          mir::Expr{
              .data = mir::IntCastExpr{.operand = machine_int},
              .type = carrier});
    }
    case support::DpiScalarAbi::kReal:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kRealValue},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kString:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kStringCStr},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kChandle:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kChandlePtr},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kLogicScalar:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kToSvLogic},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kVoid:
      throw InternalError(
          "MarshalSvToCarrier: void is not an argument carrier");
  }
  throw InternalError("MarshalSvToCarrier: unknown DpiScalarAbi");
}

// Foreign ABI carrier -> SV value into a declared SV type's canonical shape. An
// integral carrier is landed into the type's representation by the packed
// factory (the prototype carries that shape, so width / signedness / state
// domain follow the declared type); a real / string / chandle carrier
// constructs the SV value directly. Feeds both a function's marshaled return
// and the copy-back of an output / inout argument into its actual.
auto MarshalCarrierToSv(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId call_id,
    const support::DpiCarrier& carrier_desc, mir::TypeId result_type)
    -> mir::Expr {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalCarrierToSv: a canonical-vector carrier is not yet "
        "implemented");
  }
  mir::Block& block = *frame.current_block;
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      // The carrier is the formal's declared C width; the packed factory takes
      // the widest machine integer, so widening here keeps one runtime entry
      // serving every carrier width instead of one per width.
      const mir::ExprId machine_int = block.exprs.Add(
          mir::Expr{
              .data = mir::IntCastExpr{.operand = call_id},
              .type = unit_lowerer.Unit().builtins.machine_int64});
      const mir::ExprId prototype = block.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, frame, result_type));
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFromInt,
                          .qualification =
                              mir::TypeQualifier{.type = result_type}},
                  .arguments = {machine_int, prototype}},
          .type = result_type};
    }
    case support::DpiScalarAbi::kReal:
    case support::DpiScalarAbi::kString:
    case support::DpiScalarAbi::kChandle:
      return mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {call_id}},
          .type = result_type};
    case support::DpiScalarAbi::kLogicScalar: {
      const mir::ExprId prototype = block.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, frame, result_type));
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kFromSvLogic},
                  .arguments = {call_id, prototype}},
          .type = result_type};
    }
    case support::DpiScalarAbi::kVoid:
      throw InternalError("MarshalCarrierToSv: void has no marshal-out");
  }
  throw InternalError("MarshalCarrierToSv: unknown DpiScalarAbi");
}

// A canonical-vector actual crosses by pointer to a boundary buffer, regardless
// of direction -- even an input, since the C ABI is `const svLogicVecVal*`. The
// buffer is a `DpiBitBuffer` / `DpiLogicBuffer` value, so it needs a boundary
// temp and the sequenced closure path.
[[nodiscard]] auto NeedsBoundaryTemp(const mir::ForeignParam& p) -> bool {
  return support::DpiDirectionWritesBack(p.direction) ||
         std::holds_alternative<support::VectorCarrier>(p.carrier);
}

// The read-back builtin for a canonical-vector carrier: 4-state reads both
// planes, 2-state the value plane only.
[[nodiscard]] auto VectorReadBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kReadCanonicalLogicVec
                      : support::BuiltinFn::kReadCanonicalBitVec;
}

// The writable canonical chunk pointer of a boundary buffer, `(buf).Data()`. It
// feeds both the foreign call (which writes through it) and the copy-back read.
// The result type is a borrowed pointer for bookkeeping only: value emission
// renders the `Data()` call and passes it as an argument, never spelling the
// pointer type itself.
auto BuildBufferDataCall(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId buffer_ref,
    mir::TypeId carrier_type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kDpiBufferData},
                  .arguments = {buffer_ref}},
          .type = unit.types.PointerTo(
              carrier_type, mir::PointerOwnership::kBorrowed)});
}

// The MIR type of one foreign wrapper parameter, the ABI carrier realized as a
// concrete type so a backend spells it through ordinary type mapping. A scalar
// `input` is its by-value carrier; a scalar `output` / `inout` is a borrowed
// pointer to it. A packed vector is a borrowed pointer to its canonical chunk,
// read-only for an `input` the wrapper only reads (rendering `const
// svBitVecVal*` through the pointer's mutability) and mutable for a writeback
// direction.
auto ExportParamType(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier,
    support::DpiDirection direction) -> mir::TypeId {
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    const mir::TypeId chunk = unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicChunk
                                    : mir::RuntimeLibraryKind::kDpiBitChunk}});
    const mir::Mutability mutability =
        direction == support::DpiDirection::kInput ? mir::Mutability::kReadOnly
                                                   : mir::Mutability::kMutable;
    return unit.types.PointerTo(
        chunk, mir::PointerOwnership::kBorrowed, mutability);
  }
  const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
  if (support::DpiDirectionWritesBack(direction)) {
    return unit.types.PointerTo(carrier_type, mir::PointerOwnership::kBorrowed);
  }
  return carrier_type;
}

// The builtin that writes an SV value out into a foreign-owned canonical
// buffer, the write direction that pairs with the vector read builtin.
[[nodiscard]] auto VectorWriteBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kWriteCanonicalLogicVec
                      : support::BuiltinFn::kWriteCanonicalBitVec;
}

// The all-input function import call: every actual crosses by value, so the
// call is a plain expression -- no statement sequencing, no boundary temps.
// Each actual is marshaled to its ABI carrier, the foreign symbol is called
// over the carriers, and a non-void result is marshaled back to the declared SV
// type. A task never reaches here; its await needs a coroutine, so it always
// sequences.
template <ExprLowerer Lowerer>
auto LowerForeignImportInputsOnly(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  auto& block = *frame.current_block;
  const auto& hir_exprs = lowerer.HirExprs();

  std::vector<mir::ExprId> carrier_args;
  carrier_args.reserve(c.arguments.size());
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("DPI-C import call argument unexpectedly elided");
    }
    auto sv_or = lowerer.LowerExpr(hir_exprs.Get(*c.arguments[i]), frame);
    if (!sv_or) return std::unexpected(std::move(sv_or.error()));
    const mir::ExprId sv_id = block.exprs.Add(*std::move(sv_or));
    carrier_args.push_back(
        MarshalSvToCarrier(unit, block, sv_id, decl.params[i].carrier));
  }

  const bool is_void = decl.ret_abi == support::DpiScalarAbi::kVoid;
  const mir::TypeId call_type =
      is_void ? unit.builtins.void_type
              : CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
  mir::Expr foreign_call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = std::move(carrier_args)},
      .type = call_type};
  if (is_void) {
    return foreign_call;
  }
  const mir::ExprId call_id = block.exprs.Add(std::move(foreign_call));
  return MarshalCarrierToSv(
      unit_lowerer, frame, call_id, support::ScalarCarrier{decl.ret_abi},
      result_type);
}

// Populates `closure`'s body with the DPI import boundary: each actual
// marshaled to its carrier -- a by-value scalar input crossing by value, a
// writeback direction or a canonical vector of any direction seeded into a
// boundary temp passed by pointer -- then the foreign call, then the copy-back
// of each writeback into its actual's cell. A valued call captures its carrier
// result in a temp, returned so the caller marshals it after the copy-backs; a
// void call (including a task, whose foreign symbol's disable-ack `int` is
// discarded) is a bare statement and returns no temp. Shared by the function
// and task import lowerings, which differ only in how they finish the closure.
template <ExprLowerer Lowerer>
auto PopulateForeignImportBoundary(
    Lowerer& lowerer, ClosureBuilder& closure, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::EnclosingHops hops) -> diag::Result<std::optional<mir::LocalId>> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  const mir::TypeId void_t = unit.builtins.void_type;

  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();

  // A context import (LRM 35.5.3) observes the scope of its declaration and may
  // call an exported subroutine back, so the boundary opens with an RAII guard
  // that pushes the declaration scope (the enclosing instance `hops` levels up)
  // on the calling process's DPI scope chain for the foreign call's duration.
  // Declared first so its scope-exit pop is the last effect, on a normal return
  // or an unwind.
  if (decl.external.is_context) {
    const mir::TypeId guard_type = unit.types.Intern(
        mir::RuntimeLibraryType{
            .kind = mir::RuntimeLibraryKind::kDpiScopeGuard});
    const mir::LocalId guard = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_dpi_scope", .type = guard_type});
    const mir::ExprId services_id =
        body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
    const mir::ExprId decl_scope_id =
        BuildEnclosingScopeReceiver(cframe, unit, hops);
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = guard,
            .init = body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = mir::Construct{},
                            .arguments = {services_id, decl_scope_id}},
                    .type = guard_type})});
  }

  // One output / inout argument to copy back after the call.
  struct Writeback {
    mir::LocalId temp{};
    mir::TypeId carrier_type{};
    hir::ExprId actual{};
    support::DpiCarrier carrier{};
    mir::TypeId sv_type{};
  };
  std::vector<Writeback> writebacks;

  std::vector<mir::ExprId> call_args;
  call_args.reserve(c.arguments.size());
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("DPI-C import call argument unexpectedly elided");
    }
    const hir::ExprId actual = *c.arguments[i];
    const support::DpiCarrier& carrier = decl.params[i].carrier;
    const support::DpiDirection direction = decl.params[i].direction;
    const bool writes_back = support::DpiDirectionWritesBack(direction);
    const auto* vector = std::get_if<support::VectorCarrier>(&carrier);

    // A by-value scalar input crosses by value: no boundary temp, no
    // sequencing.
    if (vector == nullptr && !writes_back) {
      auto sv_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
      if (!sv_or) return std::unexpected(std::move(sv_or.error()));
      const mir::ExprId sv_id = body.exprs.Add(*std::move(sv_or));
      call_args.push_back(MarshalSvToCarrier(unit, body, sv_id, carrier));
      continue;
    }

    // Every other actual seeds a boundary temp from the actual's current value.
    // inout requires that initial value (LRM 35.5.1.2); for output the initial
    // carrier value is implementation-defined, so seeding from the actual is a
    // legal, uniform choice; a vector input seeds the same way. The foreign
    // side writes through the pointer (for writeback directions); the copy-back
    // below lands the result back in the actual's cell.
    const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
    const mir::LocalId temp = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{
            .name = "_lyra_dpi_arg" + std::to_string(i), .type = carrier_type});
    auto seed_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
    if (!seed_or) return std::unexpected(std::move(seed_or.error()));
    const mir::ExprId seed_sv = body.exprs.Add(*std::move(seed_or));

    if (vector != nullptr) {
      // A canonical vector's boundary temp is a buffer value constructed from
      // the seed (its constructor fills it). The foreign call and the copy-back
      // read reach the chunks through its writable data pointer.
      body.AppendStmt(
          mir::LocalDeclStmt{
              .target = temp,
              .init = body.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee = mir::Construct{},
                              .arguments = {seed_sv}},
                      .type = carrier_type})});
      const mir::ExprId buffer_ref =
          body.exprs.Add(mir::MakeLocalRefExpr(temp, carrier_type));
      call_args.push_back(
          BuildBufferDataCall(unit, body, buffer_ref, carrier_type));
    } else {
      // A scalar output / inout's boundary temp is the by-value carrier, seeded
      // by the marshal-in rvalue and passed by pointer.
      body.AppendStmt(
          mir::LocalDeclStmt{
              .target = temp,
              .init = MarshalSvToCarrier(unit, body, seed_sv, carrier)});
      const mir::TypeId ptr_type =
          unit.types.PointerTo(carrier_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId temp_ref =
          body.exprs.Add(mir::MakeLocalRefExpr(temp, carrier_type));
      call_args.push_back(
          body.exprs.Add(mir::MakeAddressOfExpr(temp_ref, ptr_type)));
    }

    if (writes_back) {
      writebacks.push_back(
          Writeback{
              .temp = temp,
              .carrier_type = carrier_type,
              .actual = actual,
              .carrier = carrier,
              .sv_type = decl.params[i].sv_type});
    }
  }

  const bool is_void = decl.ret_abi == support::DpiScalarAbi::kVoid;
  const mir::TypeId call_type =
      is_void ? void_t
              : CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
  mir::Expr foreign_call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = std::move(call_args)},
      .type = call_type};

  // A valued call captures its carrier result in a temp so the copy-backs run
  // before it is marshaled and returned; a void call is a bare statement.
  std::optional<mir::LocalId> ret_temp;
  if (!is_void) {
    ret_temp = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_dpi_ret", .type = call_type});
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = *ret_temp,
            .init = body.exprs.Add(std::move(foreign_call))});
  } else {
    body.AppendStmt(
        mir::ExprStmt{.expr = body.exprs.Add(std::move(foreign_call))});
  }

  for (const Writeback& wb : writebacks) {
    auto lhs_or = lowerer.LowerLhsExpr(hir_exprs.Get(wb.actual), cframe);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const mir::ExprId lhs_id = body.exprs.Add(*std::move(lhs_or));
    const mir::ExprId temp_ref =
        body.exprs.Add(mir::MakeLocalRefExpr(wb.temp, wb.carrier_type));

    // A scalar carrier marshals its by-value temp back; a vector carrier reads
    // its buffer's chunks back through the buffer's data pointer.
    mir::ExprId rhs_id{};
    if (const auto* vector = std::get_if<support::VectorCarrier>(&wb.carrier)) {
      const mir::ExprId data =
          BuildBufferDataCall(unit, body, temp_ref, wb.carrier_type);
      const mir::ExprId prototype = body.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, cframe, wb.sv_type));
      rhs_id = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = VectorReadBuiltin(*vector)},
                      .arguments = {data, prototype}},
              .type = wb.sv_type});
    } else {
      rhs_id = body.exprs.Add(MarshalCarrierToSv(
          unit_lowerer, cframe, temp_ref, wb.carrier, wb.sv_type));
    }
    const mir::ExprId runtime_id =
        body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
    const mir::Expr assign = BuildObservableAssignExpr(
        unit, body, runtime_id, lhs_id, rhs_id, std::nullopt, wb.sv_type,
        void_t);
    body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign)});
  }

  return ret_temp;
}

// The general function import call: at least one actual needs a boundary temp
// -- an output / inout of any carrier, or a canonical vector of any direction
// (a vector crosses by pointer even as an input). The boundary is a statement
// sequence yielding a value, so it lowers to an immediately-invoked closure,
// uniform for void / valued and statement / expression position. A by-value
// scalar input still crosses by value with no temp.
template <ExprLowerer Lowerer>
auto LowerForeignImportSequenced(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::EnclosingHops hops, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  ClosureBuilder closure(unit, frame);
  auto ret_temp =
      PopulateForeignImportBoundary(lowerer, closure, c, decl, target, hops);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  if (!ret_temp->has_value()) {
    return BuildClosureCallExpr(
        unit, *frame.current_block, closure.BuildVoid());
  }
  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();
  const mir::TypeId call_type =
      CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
  const mir::ExprId ret_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(**ret_temp, call_type));
  const mir::ExprId result_id = body.exprs.Add(MarshalCarrierToSv(
      unit_lowerer, cframe, ret_ref, support::ScalarCarrier{decl.ret_abi},
      result_type));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(result_id));
}

// The task import call (LRM 35.5.2): the same boundary as a function, but a
// task has no SV return, so the closure finishes as a coroutine -- the
// awaitable the caller drives, the same call protocol as a native task enable
// (LRM 35.8), uniform whether or not the foreign side consumes time. The
// boundary always sequences through the closure, even all-input, because the
// await needs a coroutine to drive. The coroutine closure is returned directly,
// not called: it renders self-invoking to a `Coroutine`, and the statement
// lowering awaits it.
template <ExprLowerer Lowerer>
auto LowerForeignImportTask(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::EnclosingHops hops) -> diag::Result<mir::Expr> {
  auto& unit = lowerer.Owner().Unit();
  ClosureBuilder closure(unit, frame);
  auto ret_temp =
      PopulateForeignImportBoundary(lowerer, closure, c, decl, target, hops);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));
  return closure.BuildCoroutine();
}

// The compilation-unit identity of the class `hops` enclosing levels up. A
// receiver-less callable names its owner in the call target, so that owner must
// be a unit-stable class identity rather than a position in the lowering walk;
// a class carries its own identity in its self-pointer type, so recover it from
// there.
auto EnclosingClassIdAtHops(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ClassId {
  const mir::TypeId self_ptr =
      frame.EnclosingClassAtHops(hops).self_pointer_type;
  const auto& ptr = std::get<mir::PointerType>(unit.types.Get(self_ptr).data);
  return std::get<mir::ObjectType>(unit.types.Get(ptr.pointee).data).class_id;
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerForeignImportCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  // An import is a receiver-less associated callable of the scope that declares
  // it. `hops` is how far out that scope sits from the caller -- a declaration
  // lookup distance, resolved here and then gone; the callable has no body and
  // no receiver, so nothing is reached at run time and the call names its owner
  // directly.
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::EnclosingHops hops{.value = ref.hops.value};
  const mir::CallableTarget target{
      .owner = EnclosingClassIdAtHops(frame, unit, hops),
      .slot = mir::CallableId{ref.id.value}};
  const mir::CallableDecl& callable =
      frame.EnclosingClassAtHops(hops).callables.Get(target.slot);
  // A DPI import always resolves to the external implementation form; its ABI
  // projection drives the marshaling below.
  const auto& decl = std::get<mir::ExternalCallable>(callable.impl);

  if (decl.is_task) {
    return LowerForeignImportTask(lowerer, frame, c, decl, target, hops);
  }
  // A context import sequences through a closure even when every actual is a
  // by-value input, because its scope guard is a scoped body local the plain
  // single-expression form has no room for.
  const bool needs_temp = std::ranges::any_of(decl.params, NeedsBoundaryTemp);
  if (needs_temp || decl.external.is_context) {
    return LowerForeignImportSequenced(
        lowerer, frame, c, decl, target, hops, result_type);
  }
  return LowerForeignImportInputsOnly(
      lowerer, frame, c, decl, target, result_type);
}

template auto LowerForeignImportCall(
    ProcessLowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template auto LowerForeignImportCall(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::CallExpr& c, const hir::ForeignImportRef& ref,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

auto SynthesizeForeignExportWrapper(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> mir::ForeignExportWrapper {
  mir::CompilationUnit& unit = module.Unit();
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code;
  CallableBindings bindings(unit, code);
  mir::Block& body = code.body;

  const WalkFrame body_frame =
      context_frame.WithBlock(&body).WithBindings(&bindings);

  // The wrapper recovers its leading context argument from the running design
  // (LRM 35.5.3), not from a foreign caller, so it is a body local initialized
  // as the wrapper's first statement -- keeping the whole body (recovery,
  // marshaling, call, writeback) MIR a backend renders mechanically. The
  // callable model gives the two export forms different leading arguments, so
  // the wrapper mirrors whichever the target expects and recovers its value: a
  // module method (LRM 8.6) takes a `self` receiver, recovered as the current
  // DPI scope pointer narrowed to the exported subroutine's instance type; a
  // receiver-less package function (LRM 26.3) takes the run's effects,
  // recovered as the current runtime like any other package call.
  mir::TypeId context_type{};
  mir::LocalId context_local{};
  mir::ExprId context_init{};
  if (std::holds_alternative<mir::CallableTarget>(target)) {
    context_type = context_frame.current_class->self_pointer_type;
    context_local = bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = context_type});
    const mir::ExprId scope = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kCurrentExportScope},
                    .arguments = {}},
            .type = unit.builtins.scope_ptr});
    context_init = body.exprs.Add(
        mir::Expr{
            .data = mir::PointerCastExpr{.operand = scope},
            .type = context_type});
  } else {
    context_type = unit.builtins.effects;
    context_local = bindings.Declare(
        BindingOriginId::Runtime(),
        mir::LocalDecl{.name = "runtime", .type = context_type});
    context_init =
        body.exprs.Add(mir::MakeCurrentRuntimeCallExpr(context_type));
  }
  body.AppendStmt(
      mir::LocalDeclStmt{.target = context_local, .init = context_init});

  // One foreign parameter per SV formal, in declaration order (the C ABI
  // order). The parameter's MIR type is the ABI carrier realized concretely, so
  // nothing downstream re-derives the C signature from the direction / carrier.
  std::vector<mir::LocalId> params;
  params.reserve(export_decl.params.size());
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    params.push_back(bindings.DeclareAnonymous(
        mir::LocalDecl{
            .name = "arg" + std::to_string(i),
            .type = ExportParamType(unit, p.carrier, p.direction)}));
  }

  const auto param_ref = [&](std::size_t i) -> mir::ExprId {
    return body.exprs.Add(
        mir::MakeLocalRefExpr(params[i], code.locals.Get(params[i]).type));
  };

  // Marshal each `input` / `inout` argument to an explicit SV-typed temporary
  // before the call, so the read of an `inout`'s incoming value is sequenced
  // ahead of the copy-back that later overwrites it, and multiple arguments do
  // not alias inside one nested call expression. An `output` formal is not a
  // method parameter -- it rides the completion payload (LRM 13.5). A vector
  // reads its SV value from the incoming canonical buffer; a scalar `input`
  // crosses by value; a scalar `inout` reads through its pointer.
  std::vector<mir::ExprId> call_args;
  call_args.push_back(
      body.exprs.Add(mir::MakeLocalRefExpr(context_local, context_type)));
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    if (p.direction == support::DpiDirection::kOutput) {
      continue;
    }
    const mir::TypeId sv_type = module.TranslateType(p.sv_type);
    mir::ExprId sv_init{};
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      const mir::ExprId prototype =
          body.exprs.Add(BuildDefaultValueExpr(module, body_frame, sv_type));
      sv_init = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee = mir::Direct{.target = VectorReadBuiltin(*vec)},
                      .arguments = {param_ref(i), prototype}},
              .type = sv_type});
    } else {
      const mir::ExprId carrier =
          p.direction == support::DpiDirection::kInout
              ? body.exprs.Add(
                    mir::Expr{
                        .data = mir::DerefExpr{.pointer = param_ref(i)},
                        .type = CarrierTypeId(unit, p.carrier)})
              : param_ref(i);
      sv_init = body.exprs.Add(
          MarshalCarrierToSv(module, body_frame, carrier, p.carrier, sv_type));
    }
    const mir::LocalId sv_in = bindings.DeclareAnonymous(
        mir::LocalDecl{.name = "in" + std::to_string(i), .type = sv_type});
    body.AppendStmt(mir::LocalDeclStmt{.target = sv_in, .init = sv_init});
    call_args.push_back(body.exprs.Add(mir::MakeLocalRefExpr(sv_in, sv_type)));
  }

  // An exported task lowers to a coroutine -- the result type is the call
  // protocol -- while a function's result is its payload directly. The
  // completion payload the writeback loop below destructures is that payload
  // either way; for a task it is the coroutine's payload, reached past the
  // protocol.
  const mir::TypeId method_result_type = result_type;
  const bool is_task = unit.types.IsCoroutine(method_result_type);
  const mir::TypeId payload_type =
      is_task ? unit.types.CoroutinePayload(method_result_type)
              : method_result_type;
  const mir::ExprId method_call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = std::move(target)},
                  .arguments = std::move(call_args)},
          .type = method_result_type});

  // The foreign caller is synchronous C and cannot await the task body, so the
  // wrapper drives the coroutine to completion here; a function call already
  // yields its payload.
  const mir::ExprId completion_source =
      is_task ? body.exprs.Add(
                    mir::Expr{
                        .data =
                            mir::CallExpr{
                                .callee =
                                    mir::Direct{
                                        .target = support::BuiltinFn::
                                            kRunExportedTaskToCompletion},
                                .arguments = {method_call}},
                        .type = payload_type})
              : method_call;

  // The completion payload's components, in the callee's payload order: the
  // function return (when non-void) first, then each `output` / `inout` formal
  // in declaration order. Built in that exact order so each component's index
  // lines up with the payload the callee returns. `param_index` is unused for
  // the return component.
  struct Component {
    mir::TypeId sv_type;
    bool is_return;
    std::size_t param_index;
  };
  std::vector<Component> components;
  const bool has_return = export_decl.ret_abi != support::DpiScalarAbi::kVoid;
  if (has_return) {
    components.push_back(
        Component{
            .sv_type = module.TranslateType(export_decl.ret_sv_type),
            .is_return = true,
            .param_index = 0});
  }
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    if (support::DpiDirectionWritesBack(export_decl.params[i].direction)) {
      components.push_back(
          Component{
              .sv_type = module.TranslateType(export_decl.params[i].sv_type),
              .is_return = false,
              .param_index = i});
    }
  }

  // Bind the completion value to a local every component projects out of; the
  // projection encoding (bare value vs tuple) lives in one shared place. An
  // empty payload has nothing to bind, so the call is a bare statement.
  std::optional<mir::LocalId> completion;
  if (!components.empty()) {
    completion = bindings.DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
    body.AppendStmt(
        mir::LocalDeclStmt{.target = *completion, .init = completion_source});
  } else {
    body.AppendStmt(mir::ExprStmt{.expr = completion_source});
  }
  const auto component_value = [&](std::size_t k) -> mir::ExprId {
    return ProjectCompletionComponent(
        body, *completion, payload_type, components.size(), k,
        components[k].sv_type);
  };

  // Copy each `output` / `inout` component back through its foreign pointer: a
  // scalar stores its marshaled carrier through the pointer; a vector reshapes
  // the SV value into the foreign-owned canonical buffer.
  for (std::size_t k = 0; k < components.size(); ++k) {
    const Component& c = components[k];
    if (c.is_return) {
      continue;
    }
    const hir::DpiParamAbi& p = export_decl.params[c.param_index];
    const mir::ExprId value = component_value(k);
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target = VectorWriteBuiltin(*vec)},
                              .arguments = {param_ref(c.param_index), value}},
                      .type = void_type})});
      continue;
    }
    const mir::TypeId carrier_type = CarrierTypeId(unit, p.carrier);
    const mir::ExprId carrier =
        MarshalSvToCarrier(unit, body, value, p.carrier);
    const mir::ExprId place = body.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = param_ref(c.param_index)},
            .type = carrier_type});
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(
                mir::MakeAssignExpr(place, carrier, void_type))});
  }

  if (is_task) {
    // An exported task carries no SV return; its foreign entry returns the DPI
    // disable-acknowledgment int (LRM 35.8), 0 while no disable is active on
    // the thread (LRM 35.9). The disable protocol is not yet modeled, so it is
    // 0.
    const mir::TypeId int_carrier = CarrierTypeId(
        unit, support::ScalarCarrier{support::DpiScalarAbi::kInt});
    body.AppendStmt(
        mir::ReturnStmt{
            .value = body.exprs.Add(
                mir::Expr{
                    .data = mir::MachineIntLiteral{.value = 0},
                    .type = int_carrier})});
    code.result_type = int_carrier;
  } else if (has_return) {
    const mir::ExprId ret_carrier = MarshalSvToCarrier(
        unit, body, component_value(0),
        support::ScalarCarrier{export_decl.ret_abi});
    body.AppendStmt(mir::ReturnStmt{.value = ret_carrier});
    code.result_type =
        CarrierTypeId(unit, support::ScalarCarrier{export_decl.ret_abi});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    code.result_type = void_type;
  }

  code.params = std::move(params);

  return mir::ForeignExportWrapper{
      .foreign_name = export_decl.foreign_name, .code = std::move(code)};
}

}  // namespace lyra::lowering::hir_to_mir
