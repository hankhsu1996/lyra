#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The type of the object one argument crosses the DPI-C boundary in (LRM
// 35.5.6, Table H.1). The carrier classifies the *formal*'s ABI; a by-value
// scalar is an ordinary machine value and is typed as one -- an integer of the
// declared C width, a machine float, a borrowed C string, a raw pointer. A
// packed vector crosses in a canonical chunk buffer and an open array in a
// canonical image of the whole actual, both runtime library values. No type
// exists solely to mark a value as being at the boundary.
auto CarrierTypeId(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier)
    -> mir::TypeId {
  if (std::holds_alternative<support::OpenArrayCarrier>(carrier)) {
    return unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = mir::RuntimeLibraryKind::kDpiOpenArray}});
  }
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

// SV value -> by-value machine carrier (LRM 35.5.6). An integral value yields
// its widest machine integer, narrowed to the carrier's C width by a machine
// cast; a real yields its native floating value; a string borrows a
// NUL-terminated C string that stays valid for the call. Reuses the ordinary
// value accessors; the boundary conversion is a plain expression, not a
// DPI-specific primitive. Feeds an argument that crosses by value and the seed
// of a scalar boundary object the callee writes back through.
auto MarshalSvToCarrier(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId sv_id,
    const support::DpiCarrier& carrier_desc) -> mir::ExprId {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalSvToCarrier: only a by-value scalar converts to a machine "
        "value; every other carrier crosses through a boundary object");
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
        "MarshalCarrierToSv: only a by-value scalar converts back from a "
        "machine value; every other carrier reads back from its boundary "
        "object");
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

// An argument crosses the boundary in one of two shapes (LRM 35.5.6). It
// crosses **by value** when it is a scalar the callee only reads: the SV value
// converts to a machine value the call passes directly, with no local and no
// sequencing. Every other argument crosses **through a boundary object** -- a
// local the call site builds from the actual, whose address or handle the call
// receives and which a writeback direction reads back afterwards. A canonical
// vector and an open array are the second shape in either direction, because
// the C ABI reaches them by pointer and by handle respectively even to read.
[[nodiscard]] auto CrossesByValue(const hir::DpiParamAbi& p) -> bool {
  return std::holds_alternative<support::ScalarCarrier>(p.carrier) &&
         !support::DpiDirectionWritesBack(p.direction);
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

// The builtin that writes an SV value out into a foreign-owned canonical
// buffer, the write direction that pairs with the vector read builtin.
[[nodiscard]] auto VectorWriteBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kWriteCanonicalLogicVec
                      : support::BuiltinFn::kWriteCanonicalBitVec;
}

// The declared coordinate system each unpacked dimension of an open array
// reports to the foreign side (LRM Annex H.7.6): the range the declaration
// fixes, or the actual's own where the declaration left the dimension unsized.
// The actual's ranges come from its static type, the only place an unsized
// extent is fixed (LRM 35.6.1.1). The pairs flatten outermost-first into one
// array literal, the shape a runtime entry takes a bounds list in.
auto BuildOpenArrayBounds(
    mir::CompilationUnit& unit, mir::Block& block,
    const support::OpenArrayCarrier& open, mir::TypeId actual_type)
    -> mir::ExprId {
  const mir::TypeId int_type = unit.builtins.int_type;
  std::vector<mir::ExprId> bounds;
  bounds.reserve(open.unpacked.size() * 2);
  mir::TypeId cursor = actual_type;
  for (const std::optional<support::DpiRange>& declared : open.unpacked) {
    const auto* layer =
        std::get_if<mir::UnpackedArrayType>(&unit.types.Get(cursor).data);
    if (layer == nullptr) {
      throw InternalError(
          "BuildOpenArrayBounds: the actual of an open-array formal has fewer "
          "unpacked dimensions than the declaration");
    }
    const support::DpiRange range = declared.value_or(
        support::DpiRange{.left = layer->dim.left, .right = layer->dim.right});
    bounds.push_back(
        block.exprs.Add(mir::MakeIntLiteral(int_type, range.left)));
    bounds.push_back(
        block.exprs.Add(mir::MakeIntLiteral(int_type, range.right)));
    cursor = layer->element_type;
  }
  const mir::TypeId bounds_type = unit.types.Intern(
      mir::UnpackedArrayType{
          .element_type = int_type,
          .dim = mir::UnpackedRange::ZeroBased(bounds.size())});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(bounds)},
          .type = bounds_type});
}

// The initializer of one argument's boundary object, seeded from the actual's
// current value. A scalar's object is the by-value carrier itself; a canonical
// vector's is a buffer its constructor fills; an open array's is the canonical
// image of the whole actual, which additionally takes the coordinate system of
// each dimension and whether an element's canonical form is how an individual
// value of its type crosses (LRM Annex H.12.4).
auto BuildBoundaryInit(
    mir::CompilationUnit& unit, mir::Block& block,
    const support::DpiCarrier& carrier, mir::ExprId seed_sv,
    mir::TypeId actual_type, mir::TypeId carrier_type) -> mir::ExprId {
  const auto construct = [&](std::vector<mir::ExprId> arguments) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{},
                    .arguments = std::move(arguments)},
            .type = carrier_type});
  };
  return std::visit(
      Overloaded{
          [&](const support::ScalarCarrier&) {
            return MarshalSvToCarrier(unit, block, seed_sv, carrier);
          },
          [&](const support::VectorCarrier&) { return construct({seed_sv}); },
          [&](const support::OpenArrayCarrier& open) {
            const mir::ExprId addressable = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::MachineIntLiteral{
                            .value = static_cast<std::int64_t>(
                                open.element_crosses_as_canonical_vector)},
                    .type = unit.builtins.machine_int64});
            return construct(
                {seed_sv, BuildOpenArrayBounds(unit, block, open, actual_type),
                 addressable});
          }},
      carrier);
}

// The argument the foreign call receives for one boundary object: a scalar's
// address, a buffer's canonical chunk pointer, or an open array's handle.
auto BuildBoundaryArgument(
    mir::CompilationUnit& unit, mir::Block& block,
    const support::DpiCarrier& carrier, mir::LocalId temp,
    mir::TypeId carrier_type) -> mir::ExprId {
  const mir::ExprId object =
      block.exprs.Add(mir::MakeLocalRefExpr(temp, carrier_type));
  return std::visit(
      Overloaded{
          [&](const support::ScalarCarrier&) {
            return block.exprs.Add(
                mir::MakeAddressOfExpr(
                    object,
                    unit.types.PointerTo(
                        carrier_type, mir::PointerOwnership::kBorrowed)));
          },
          [&](const support::VectorCarrier&) {
            return BuildBufferDataCall(unit, block, object, carrier_type);
          },
          [&](const support::OpenArrayCarrier&) {
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target = support::BuiltinFn::
                                        kDpiOpenArrayHandle},
                            .arguments = {object}},
                    .type = unit.types.Intern(
                        mir::TypeData{mir::RuntimeLibraryType{
                            .kind = mir::RuntimeLibraryKind::
                                kDpiOpenArrayHandle}})});
          }},
      carrier);
}

// The SV value one boundary object holds once the call returns, the read-back
// that pairs with its build: a scalar marshals its by-value carrier, a vector
// reads its buffer's canonical chunks, an open array reads its whole image.
// Each yields one value, so the store into the actual is the ordinary one
// whatever the carrier. Reading through a prototype of the destination's type
// is what gives the value its declared width, signedness, and state domain,
// which no carrier carries.
auto BuildBoundaryReadback(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const support::DpiCarrier& carrier, mir::ExprId object,
    mir::TypeId carrier_type, mir::TypeId sv_type) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const auto prototype = [&] {
    return block.exprs.Add(BuildDefaultValueExpr(unit_lowerer, frame, sv_type));
  };
  const auto read = [&](support::BuiltinFn target, mir::ExprId source) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = target},
                    .arguments = {source, prototype()}},
            .type = sv_type});
  };
  return std::visit(
      Overloaded{
          [&](const support::ScalarCarrier&) {
            return block.exprs.Add(MarshalCarrierToSv(
                unit_lowerer, frame, object, carrier, sv_type));
          },
          [&](const support::VectorCarrier& vector) {
            return read(
                VectorReadBuiltin(vector),
                BuildBufferDataCall(unit, block, object, carrier_type));
          },
          [&](const support::OpenArrayCarrier&) {
            return read(support::BuiltinFn::kDpiOpenArrayValue, object);
          }},
      carrier);
}

// Whether the foreign symbol hands a value back that the call marshals into the
// SV result. A function result is restricted to a small value (LRM 35.5.5), so
// it is always a by-value scalar; a `void` function and a task both yield none,
// and a task's disable-acknowledgment `int` (LRM 35.8) is not an SV result.
[[nodiscard]] auto ReturnsValue(const hir::ForeignImportDecl& import) -> bool {
  return import.ret_abi != support::DpiScalarAbi::kVoid;
}

// The call to the foreign symbol itself, over already-marshaled arguments. The
// callee is the linkage name in the DPI-C name space (LRM 35.4), reached by
// name and never through a class or another unit's namespace.
auto BuildForeignSymbolCall(
    mir::CompilationUnit& unit, const hir::ForeignImportDecl& import,
    std::vector<mir::ExprId> carrier_args) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target =
                          mir::ForeignSymbolTarget{
                              .linkage_name = import.foreign_name}},
              .arguments = std::move(carrier_args)},
      .type = ReturnsValue(import)
                  ? CarrierTypeId(unit, support::ScalarCarrier{import.ret_abi})
                  : unit.builtins.void_type};
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
    const hir::ForeignImportDecl& import, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
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
        MarshalSvToCarrier(unit, block, sv_id, import.params[i].carrier));
  }

  mir::Expr foreign_call =
      BuildForeignSymbolCall(unit, import, std::move(carrier_args));
  if (!ReturnsValue(import)) {
    return foreign_call;
  }
  const mir::ExprId call_id = block.exprs.Add(std::move(foreign_call));
  return MarshalCarrierToSv(
      unit_lowerer, frame, call_id, support::ScalarCarrier{import.ret_abi},
      result_type);
}

// Populates `closure`'s body with the DPI import boundary: each actual crossed
// in its own shape -- by value, or through a boundary object the foreign call
// reaches by pointer or by handle -- then the foreign call, then the read-back
// of each writeback direction into its actual's cell. A valued call captures
// its carrier result in a temp, returned so the caller marshals it after the
// read-backs; a void call (including a task, whose foreign symbol's disable-ack
// `int` is discarded) is a bare statement and returns no temp. Shared by the
// function and task import lowerings, which differ only in what they wrap the
// finished boundary in.
template <ExprLowerer Lowerer>
auto PopulateForeignImportBoundary(
    Lowerer& lowerer, ClosureBuilder& closure, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import)
    -> diag::Result<std::optional<mir::LocalId>> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();

  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();

  // One output / inout argument, and what its boundary object needs to be read
  // back into the actual once the call returns.
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
    const hir::DpiParamAbi& param = import.params[i];
    const support::DpiCarrier& carrier = param.carrier;

    auto sv_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
    if (!sv_or) return std::unexpected(std::move(sv_or.error()));
    const mir::TypeId actual_type = sv_or->type;
    const mir::ExprId seed_sv = body.exprs.Add(*std::move(sv_or));

    if (CrossesByValue(param)) {
      call_args.push_back(MarshalSvToCarrier(unit, body, seed_sv, carrier));
      continue;
    }

    // Every other actual seeds a boundary object from the actual's current
    // value. inout requires that initial value (LRM 35.5.1.2); for output the
    // initial carrier value is implementation-defined, so seeding from the
    // actual is a legal, uniform choice; an input that crosses this way seeds
    // the same way. The foreign side reads and writes the object, and the
    // read-back below lands the result in the actual's cell.
    const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
    const mir::LocalId temp = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{
            .name = "_lyra_dpi_arg" + std::to_string(i), .type = carrier_type});
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = temp,
            .init = BuildBoundaryInit(
                unit, body, carrier, seed_sv, actual_type, carrier_type)});
    call_args.push_back(
        BuildBoundaryArgument(unit, body, carrier, temp, carrier_type));

    if (support::DpiDirectionWritesBack(param.direction)) {
      writebacks.push_back(
          Writeback{
              .temp = temp,
              .carrier_type = carrier_type,
              .actual = actual,
              .carrier = carrier,
              // An open array's shape is the actual's, fixed only at the call
              // (LRM 35.6.1.1), where every other carrier reads back into the
              // shape the declaration fixed.
              .sv_type =
                  std::holds_alternative<support::OpenArrayCarrier>(carrier)
                      ? actual_type
                      : unit_lowerer.TranslateType(param.sv_type)});
    }
  }

  mir::Expr foreign_call =
      BuildForeignSymbolCall(unit, import, std::move(call_args));
  const mir::TypeId call_type = foreign_call.type;

  // A valued call captures its carrier result in a temp so the copy-backs run
  // before it is marshaled and returned.
  std::optional<mir::LocalId> ret_temp;
  if (ReturnsValue(import)) {
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

    const mir::ExprId rhs_id = BuildBoundaryReadback(
        unit_lowerer, cframe, wb.carrier, temp_ref, wb.carrier_type,
        wb.sv_type);
    const mir::Expr assign =
        BuildStoreExpr(unit, body, lhs_id, rhs_id, std::nullopt, wb.sv_type);
    body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign)});
  }

  return ret_temp;
}

// The DPI scope a `context` import observes (LRM 35.5.3): an RAII guard pushing
// the declaration's own instantiated scope on the calling process's DPI scope
// chain, so an export the foreign side calls back resolves against that scope.
// It opens the boundary, so its scope-exit pop is the last effect, on a normal
// return or an unwind. A declaration in a namespace that is never instantiated
// -- a package or `$unit` scope -- observes no scope and pushes a null one; the
// push still happens, so the call never inherits an enclosing chain entry that
// is not its declaration's.
auto BuildDpiScopeGuard(
    UnitLowerer& unit_lowerer, ClosureBuilder& closure,
    std::optional<hir::StructuralHops> declaring_scope) -> mir::LocalDeclStmt {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& body = closure.Body();
  const mir::TypeId guard_type = unit.types.Intern(
      mir::RuntimeLibraryType{.kind = mir::RuntimeLibraryKind::kDpiScopeGuard});
  const mir::LocalId guard = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_dpi_scope", .type = guard_type});
  const mir::ExprId services_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId decl_scope_id =
      declaring_scope.has_value()
          ? BuildEnclosingScopeReceiver(
                closure.Frame(), unit,
                mir::EnclosingHops{.value = declaring_scope->value})
          : body.exprs.Add(
                mir::Expr{
                    .data = mir::NullLiteral{},
                    .type = unit.builtins.scope_ptr});
  return mir::LocalDeclStmt{
      .target = guard,
      .init = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee = mir::Construct{},
                      .arguments = {services_id, decl_scope_id}},
              .type = guard_type})};
}

// The general function import call: at least one actual crosses through a
// boundary object rather than by value, so the boundary is a statement sequence
// yielding a value. It lowers to an immediately-invoked closure, uniform for
// void / valued and statement / expression position. A by-value scalar input in
// the same call still crosses by value, with no object of its own.
template <ExprLowerer Lowerer>
auto LowerForeignImportSequenced(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import,
    std::optional<hir::StructuralHops> declaring_scope, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  ClosureBuilder closure(unit, frame);
  if (import.is_context) {
    closure.Body().AppendStmt(
        BuildDpiScopeGuard(unit_lowerer, closure, declaring_scope));
  }
  auto ret_temp = PopulateForeignImportBoundary(lowerer, closure, c, import);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  if (!ret_temp->has_value()) {
    return BuildClosureCallExpr(
        unit, *frame.current_block, closure.BuildVoid());
  }
  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();
  const mir::TypeId call_type =
      CarrierTypeId(unit, support::ScalarCarrier{import.ret_abi});
  const mir::ExprId ret_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(**ret_temp, call_type));
  const mir::ExprId result_id = body.exprs.Add(MarshalCarrierToSv(
      unit_lowerer, cframe, ret_ref, support::ScalarCarrier{import.ret_abi},
      result_type));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(result_id));
}

// The task import call (LRM 35.5.2): a task has no SV return, so the call
// finishes as a coroutine -- the awaitable the caller drives, the same call
// protocol as a native task enable (LRM 35.8), uniform whether or not the
// foreign side consumes time. The boundary always sequences through the
// closure, even all-input, because the await needs a coroutine to drive. The
// closure is returned directly, not called: building a coroutine closure is
// starting it, which is what its type says, and the statement lowering awaits
// it.
//
// A foreign task may consume simulation time, which it does by calling back an
// exported task that suspends. The foreign call therefore runs on a fiber whose
// native stack can be parked and resumed, and awaiting that fiber is what
// suspends the caller. Both facts are stated here as ordinary MIR -- a call to
// the runtime's fiber entry over a closure of the whole boundary, awaited --
// rather than left for a backend to infer from the callee being a foreign task.
// The scope guard stays outside the fiber, on the awaiting coroutine, so the
// scope is pushed on the process before the native stack is entered.
template <ExprLowerer Lowerer>
auto LowerForeignImportTask(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import,
    std::optional<hir::StructuralHops> declaring_scope)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  ClosureBuilder outer(unit, frame);
  if (import.is_context) {
    outer.Body().AppendStmt(
        BuildDpiScopeGuard(unit_lowerer, outer, declaring_scope));
  }

  ClosureBuilder fiber_body(unit, outer.Frame());
  auto ret_temp = PopulateForeignImportBoundary(lowerer, fiber_body, c, import);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  mir::Block& body = outer.Body();
  const mir::TypeId awaitable = unit.types.Intern(
      mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kForeignTaskAwaitable});
  const mir::ExprId fiber_id = body.exprs.Add(fiber_body.BuildVoid());
  const mir::ExprId run_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kRunForeignTaskOnFiber},
                  .arguments = {fiber_id}},
          .type = awaitable});
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::Expr{
                  .data = mir::AwaitExpr{.awaitable = run_id},
                  .type = unit.builtins.void_type})});
  return outer.BuildCoroutine();
}

// The MIR type one foreign formal is declared with, the ABI carrier realized as
// a concrete type so every consumer spells it through ordinary type mapping. A
// scalar `input` is its by-value carrier; a scalar `output` / `inout` is a
// borrowed pointer to it. A packed vector is a borrowed pointer to its
// canonical chunk, read-only for an `input` the boundary only reads (rendering
// `const svBitVecVal*` through the pointer's mutability) and mutable for a
// writeback direction. An open array is an opaque handle either way.
auto ForeignBoundaryType(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier,
    support::DpiDirection direction) -> mir::TypeId {
  // An open array crosses by handle in either direction (LRM Annex H.8.6), so
  // its boundary type does not read the direction at all.
  if (std::holds_alternative<support::OpenArrayCarrier>(carrier)) {
    return unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = mir::RuntimeLibraryKind::kDpiOpenArrayHandle}});
  }
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

// The MIR type a foreign entry point returns. A task carries no SV return
// value; its C entry returns the disable-acknowledgment int instead (LRM 35.8).
auto ForeignBoundaryReturnType(
    mir::CompilationUnit& unit, support::DpiScalarAbi ret_abi, bool is_task)
    -> mir::TypeId {
  if (is_task) {
    return CarrierTypeId(
        unit, support::ScalarCarrier{support::DpiScalarAbi::kInt});
  }
  if (ret_abi == support::DpiScalarAbi::kVoid) {
    return unit.builtins.void_type;
  }
  return CarrierTypeId(unit, support::ScalarCarrier{ret_abi});
}

}  // namespace

auto MakeForeignSignature(
    mir::CompilationUnit& unit, std::span<const hir::DpiParamAbi> params,
    support::DpiScalarAbi ret_abi, bool is_task) -> mir::CallableCode {
  mir::CallableCode code;
  CallableBindings bindings(unit, code);
  code.params.reserve(params.size());
  for (std::size_t i = 0; i < params.size(); ++i) {
    code.params.push_back(bindings.DeclareAnonymous(
        mir::LocalDecl{
            .name = "arg" + std::to_string(i),
            .type = ForeignBoundaryType(
                unit, params[i].carrier, params[i].direction)}));
  }
  code.result_type = ForeignBoundaryReturnType(unit, ret_abi, is_task);
  return code;
}

auto MakeForeignImportDecl(
    mir::CompilationUnit& unit, const hir::ForeignImportDecl& import)
    -> mir::CallableDecl {
  return mir::CallableDecl{
      .name = import.name,
      .code = MakeForeignSignature(
          unit, import.params, import.ret_abi, import.is_task),
      .foreign = mir::ForeignLinkage{.foreign_name = import.foreign_name},
      .virtual_dispatch = std::nullopt};
}

template <ExprLowerer Lowerer>
auto LowerForeignImportCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const hir::ForeignImportDecl& import =
      lowerer.Owner().Hir().foreign_imports.Get(ref.id);

  if (import.is_task) {
    return LowerForeignImportTask(
        lowerer, frame, c, import, ref.declaring_scope);
  }
  // The boundary needs a statement sequence when anything in it is a scoped
  // body local: an actual crossing through a boundary object, or a context
  // import's scope guard, which a plain single-expression call has no room for.
  const bool sequences =
      import.is_context || !std::ranges::all_of(import.params, CrossesByValue);
  if (sequences) {
    return LowerForeignImportSequenced(
        lowerer, frame, c, import, ref.declaring_scope, result_type);
  }
  return LowerForeignImportInputsOnly(lowerer, frame, c, import, result_type);
}

template auto LowerForeignImportCall(
    ProcessLowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template auto LowerForeignImportCall(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::CallExpr& c, const hir::ForeignImportRef& ref,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

auto SynthesizeForeignExportEntry(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> mir::CallableDecl {
  mir::CompilationUnit& unit = module.Unit();
  const mir::TypeId void_type = unit.builtins.void_type;

  // An exported subroutine cannot take an open array (LRM 35.5.6.1, Annex
  // H.8.2), and its declaration is rejected before any of this runs, so the
  // marshaling below reads only the two fixed-shape carriers.
  const auto crosses_by_handle = [](const hir::DpiParamAbi& p) {
    return std::holds_alternative<support::OpenArrayCarrier>(p.carrier);
  };
  if (std::ranges::any_of(export_decl.params, crosses_by_handle)) {
    throw InternalError(
        "SynthesizeForeignExportEntry: an export declared an open-array "
        "formal");
  }

  // An exported task lowers to a coroutine -- the result type is the call
  // protocol -- while a function's result is its payload directly.
  const bool is_task = unit.types.IsCoroutine(result_type);

  // The entry point publishes the same signature an import of the same shape
  // would, and then defines it -- which is the whole difference between the two
  // directions of the boundary.
  mir::CallableCode code = MakeForeignSignature(
      unit, export_decl.params, export_decl.ret_abi, is_task);
  code.body.emplace();
  CallableBindings bindings(unit, code);
  mir::Block& body = code.Body();
  const std::vector<mir::LocalId> params = code.params;

  const WalkFrame body_frame =
      context_frame.WithBlock(&body).WithBindings(&bindings);

  // The entry point recovers its leading context argument from the running
  // design (LRM 35.5.3), not from a foreign caller, so it is a body local
  // initialized first -- keeping the whole body (recovery, marshaling, call,
  // writeback) MIR a backend renders mechanically. The callable model gives the
  // two export forms different leading arguments, so the body mirrors whichever
  // the target expects and recovers its value: a
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

  // The completion payload the writeback loop below destructures is the
  // exported subroutine's result either way; for a task it is the coroutine's
  // payload, reached past the protocol.
  const mir::TypeId method_result_type = result_type;
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
  // entry point drives the coroutine to completion here; a function call
  // already yields its payload.
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

  // Bind the completion value to a local every component projects out of. A
  // completion is a product at every count, so it binds the same way whether or
  // not anything is projected back out of it.
  const mir::LocalId completion = bindings.DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
  body.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_source});
  const auto component_value = [&](std::size_t k) -> mir::ExprId {
    return ProjectCompletionComponent(
        body, completion, payload_type, k, components[k].sv_type);
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
    body.AppendStmt(
        mir::ReturnStmt{
            .value = body.exprs.Add(
                mir::Expr{
                    .data = mir::MachineIntLiteral{.value = 0},
                    .type = code.result_type})});
  } else if (has_return) {
    const mir::ExprId ret_carrier = MarshalSvToCarrier(
        unit, body, component_value(0),
        support::ScalarCarrier{export_decl.ret_abi});
    body.AppendStmt(mir::ReturnStmt{.value = ret_carrier});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  }

  // The entry point's identity is its linkage name: it is a program-global
  // symbol in the DPI name space, not an SV declaration the source can call
  // (LRM 35.4, 35.7), so it shares no name with the subroutine it dispatches
  // into.
  return mir::CallableDecl{
      .name = export_decl.foreign_name,
      .code = std::move(code),
      .foreign = mir::ForeignLinkage{.foreign_name = export_decl.foreign_name},
      .virtual_dispatch = std::nullopt};
}

}  // namespace lyra::lowering::hir_to_mir
