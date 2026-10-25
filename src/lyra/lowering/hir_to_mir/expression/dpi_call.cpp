#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
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
        mir::Type{mir::RuntimeLibraryType{
            .kind = mir::RuntimeLibraryKind::kDpiOpenArray}});
  }
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    return unit.types.Intern(
        mir::Type{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicBuffer
                                    : mir::RuntimeLibraryKind::kDpiBitBuffer}});
  }
  const auto machine_int = [&](mir::MachineIntWidth width,
                               mir::Signedness sign) {
    return unit.types.Intern(
        mir::Type{mir::MachineIntType{.width = width, .signedness = sign}});
  };
  switch (std::get<support::ScalarCarrier>(carrier).abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kLogicScalar:
      return machine_int(mir::MachineIntWidth::k8, mir::Signedness::kUnsigned);
    case support::DpiScalarAbi::kByte:
      return machine_int(mir::MachineIntWidth::k8, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kShortInt:
      return machine_int(mir::MachineIntWidth::k16, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kInt:
      return machine_int(mir::MachineIntWidth::k32, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kLongInt:
      return machine_int(mir::MachineIntWidth::k64, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kReal:
      return unit.types.Intern(
          mir::Type{
              mir::MachineFloatType{.width = mir::MachineFloatWidth::k64}});
    case support::DpiScalarAbi::kString:
      return unit.builtins.machine_c_string;
    case support::DpiScalarAbi::kChandle:
      return mir::ErasedPointer(unit.types);
    case support::DpiScalarAbi::kVoid:
      return unit.builtins.void_type;
  }
  throw InternalError("CarrierTypeId: unknown DPI-C scalar ABI");
}

// How long the foreign side may read what a value crosses as. Only a string
// crosses as storage of its own -- every other scalar is the number or the
// pointer itself -- so only a string's crossing reads it (LRM Annex H.8.10).
// An import's argument is lent for the foreign call, which is as long as the
// value it borrows from lives. A string an export hands back to its caller
// outlives the entry that made it, so the run keeps it until the import call
// that let the caller in returns.
enum class ForeignHold : std::uint8_t {
  kForTheCall,
  kUntilTheImportReturns,
};

// SV value -> by-value machine carrier (LRM 35.5.6). An integral value yields
// its widest machine integer, narrowed to the carrier's C width by a machine
// cast; a real yields its native floating value; a string yields a
// NUL-terminated C string the foreign side may read for as long as `hold`
// says. Reuses the ordinary value accessors; the boundary conversion is a
// plain expression, not a DPI-specific primitive. Feeds an argument that
// crosses by value, the seed of a scalar boundary object the callee writes
// back through, and what an export hands back.
auto MarshalSvToCarrier(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId sv_id,
    const support::DpiCarrier& carrier_desc, ForeignHold hold) -> mir::ExprId {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalSvToCarrier: only a by-value scalar converts to a machine "
        "value; every other carrier crosses through a boundary object");
  }
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId carrier = CarrierTypeId(unit, carrier_desc);
  sv_id = block.AsValue(unit.types, sv_id);
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      const mir::ExprId machine_int = BuildToInt64Call(unit, block, sv_id);
      return block.exprs.Add(
          mir::Expr{
              .data = mir::CastExpr{.operand = machine_int}, .type = carrier});
    }
    case support::DpiScalarAbi::kReal:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRealValue,
                              .receiver = sv_id},
                      .arguments = {}},
              .type = carrier});
    case support::DpiScalarAbi::kString:
      switch (hold) {
        case ForeignHold::kForTheCall:
          return block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kStringCStr,
                                  .receiver = sv_id},
                          .arguments = {}},
                  .type = carrier});
        case ForeignHold::kUntilTheImportReturns:
          return block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::
                                      kHandStringToForeignCaller},
                          .arguments =
                              {block.exprs.Add(
                                   BuildCurrentRuntimeCallExpr(unit_lowerer)),
                               sv_id}},
                  .type = carrier});
      }
      throw InternalError("MarshalSvToCarrier: unknown ForeignHold");
    case support::DpiScalarAbi::kChandle:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kChandlePtr,
                              .receiver = sv_id},
                      .arguments = {}},
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
// integral carrier is landed by the conversion from a machine integer, which is
// told the declared type so width / signedness / state domain follow it; a real
// / string / chandle carrier constructs the SV value directly. Feeds both a
// function's marshaled return and the copy-back of an output / inout argument
// into its actual.
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
  // The carrier is read as the machine value it holds.
  call_id = block.AsValue(unit_lowerer.Unit().types, call_id);
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      // The carrier is the formal's declared C width; the conversion takes
      // the widest machine integer, so widening here keeps one runtime entry
      // serving every carrier width instead of one per width.
      const mir::ExprId machine_int = block.exprs.Add(
          mir::Expr{
              .data = mir::CastExpr{.operand = call_id},
              .type = unit_lowerer.Unit().builtins.machine_int64});
      return mir::MakeBuiltinCall(
          unit_lowerer.Unit(), block, support::BuiltinFn::kIntegralFromInt,
          std::nullopt, {machine_int}, result_type);
    }
    case support::DpiScalarAbi::kReal:
    case support::DpiScalarAbi::kString:
    case support::DpiScalarAbi::kChandle:
      return mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {call_id}},
          .type = result_type};
    case support::DpiScalarAbi::kLogicScalar:
      return mir::MakeBuiltinCall(
          unit_lowerer.Unit(), block, support::BuiltinFn::kFromSvLogic,
          std::nullopt, {call_id}, result_type);
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

// The chunk-pointer builtin for a canonical-vector carrier: 4-state and 2-state
// are separate buffer types, reached through separate C pointer types (Annex
// H.10.2).
[[nodiscard]] auto VectorDataBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kDpiLogicBufferData
                      : support::BuiltinFn::kDpiBitBufferData;
}

// The writable canonical chunk pointer of a boundary buffer. It feeds both the
// foreign call (which writes through it) and the copy-back read, and is only
// ever handed on as an argument.
auto BuildBufferDataCall(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId buffer_ref,
    mir::TypeId carrier_type, const support::VectorCarrier& carrier)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = VectorDataBuiltin(carrier),
                          .receiver = buffer_ref},
                  .arguments = {}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = carrier_type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

// The builtin that writes an SV value out into a foreign-owned canonical
// buffer, the write direction that pairs with the vector read builtin.
[[nodiscard]] auto VectorWriteBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kWriteCanonicalLogicVec
                      : support::BuiltinFn::kWriteCanonicalBitVec;
}

// What one element of the actual is, under the unpacked dimensions the
// open-array formal declares. Every leaf of the actual is a value of it, so it
// is what fixes the image's element shape (Annex H.7.7) -- a fact of the
// declaration, which an actual holding no elements could not have supplied
// from a value.
//
// Walking to it is also where an actual this boundary cannot image is caught.
// Annex H.7.6 reports each unsized dimension with the actual's own range, and
// the image is built before the call from ranges this lowering states, so a
// layer whose extent the program only fixes while it runs -- a dynamic array
// or a queue (LRM 7.5, 7.10) -- has no range to state. The clause admits such
// an actual, so it is refused by name rather than being a program the standard
// forbids. A layer that is neither is an actual the front end should have
// rejected against the formal.
auto OpenArrayElementType(
    const mir::CompilationUnit& unit, const support::OpenArrayCarrier& open,
    mir::TypeId actual_type) -> diag::Result<mir::TypeId> {
  mir::TypeId cursor = actual_type;
  for (std::size_t d = 0; d < open.unpacked.size(); ++d) {
    const mir::Type& layer = unit.types.Get(cursor);
    if (layer.Is<mir::DynamicArrayType>() || layer.Is<mir::QueueType>()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedDpi,
          "a dynamic array or a queue passed to an open array formal of a "
          "DPI-C import is not yet supported");
    }
    const auto* fixed = layer.As<mir::UnpackedArrayType>();
    if (fixed == nullptr) {
      throw InternalError(
          "OpenArrayElementType: the actual of an open-array formal has fewer "
          "unpacked dimensions than the declaration");
    }
    cursor = fixed->element_type;
  }
  return cursor;
}

// The declared coordinate system each unpacked dimension of an open array
// reports to the foreign side (LRM Annex H.7.6): the range the declaration
// fixes, or the actual's own where the declaration left the dimension unsized.
// The actual's ranges come from its static type, the only place an unsized
// extent is fixed (LRM 35.6.1.1). It is stated at the call rather than taken
// from the actual's type, because a declaration that sizes a dimension
// overrides what the actual declares.
auto BuildOpenArrayBounds(
    mir::CompilationUnit& unit, mir::Block& block,
    const support::OpenArrayCarrier& open, const hir::TypePool& hir_types,
    hir::TypeId actual_type) -> mir::ExprId {
  std::vector<support::DpiRange> ranges;
  ranges.reserve(open.unpacked.size());
  hir::TypeId cursor = actual_type;
  for (const std::optional<support::DpiRange>& declared : open.unpacked) {
    const auto* layer = hir_types.Get(cursor).As<hir::UnpackedArrayType>();
    if (layer == nullptr) {
      throw InternalError(
          "BuildOpenArrayBounds: the actual of an open-array formal has fewer "
          "unpacked dimensions than the declaration");
    }
    ranges.push_back(declared.value_or(
        support::DpiRange{.left = layer->dim.left, .right = layer->dim.right}));
    cursor = layer->element_type;
  }
  return mir::BuildDeclaredRanges(unit, block, ranges);
}

// The initializer of one argument's boundary object, seeded from the actual's
// current value. A scalar's object is the by-value carrier itself; a canonical
// vector's is a buffer its constructor fills; an open array's is the canonical
// image of the whole actual, which additionally takes the coordinate system of
// each dimension and whether an element's canonical form is how an individual
// value of its type crosses (LRM Annex H.12.4).
auto BuildBoundaryInit(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const support::DpiCarrier& carrier, mir::ExprId seed_sv,
    mir::TypeId actual_type, hir::TypeId actual_hir_type,
    mir::TypeId carrier_type) -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
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
          [&](const support::ScalarCarrier&) -> diag::Result<mir::ExprId> {
            return MarshalSvToCarrier(
                unit_lowerer, block, seed_sv, carrier,
                ForeignHold::kForTheCall);
          },
          [&](const support::VectorCarrier&) -> diag::Result<mir::ExprId> {
            return construct({seed_sv});
          },
          [&](const support::OpenArrayCarrier& open)
              -> diag::Result<mir::ExprId> {
            auto element_or = OpenArrayElementType(unit, open, actual_type);
            if (!element_or) {
              return std::unexpected(std::move(element_or.error()));
            }
            if (!unit.types.Get(*element_or).IsIntegral()) {
              throw InternalError(
                  "BuildBoundaryInit: an open-array formal's element is not "
                  "integral, which its declaration is refused for -- please "
                  "report this as a bug");
            }
            // The image holds each element in its canonical form (LRM Annex
            // H.7.7), which the element's own type fixes, and the actual's
            // type states that type under its unpacked layers. What the
            // formal's declaration adds to it is the operands beside the seed.
            const mir::ExprId bounds = BuildOpenArrayBounds(
                unit, block, open, unit_lowerer.Hir().types, actual_hir_type);
            return construct(
                {seed_sv, bounds,
                 block.exprs.Add(
                     mir::Expr{
                         .data =
                             mir::MachineBoolLiteral{
                                 .value =
                                     open.element_crosses_as_canonical_vector},
                         .type = unit.builtins.machine_bool})});
          }},
      carrier);
}

// The argument the foreign call receives for one boundary object: a scalar's
// address, a buffer's canonical chunk pointer, or an open array's handle.
auto BuildBoundaryArgument(
    mir::CompilationUnit& unit, mir::Block& block,
    const support::DpiCarrier& carrier, mir::LocalId temp,
    mir::TypeId carrier_type) -> mir::ExprId {
  const mir::ExprId object = block.LocalPlace(temp, carrier_type);
  return std::visit(
      Overloaded{
          [&](const support::ScalarCarrier&) {
            return block.exprs.Add(
                mir::MakeAddressOfExpr(
                    object,
                    unit.types.Intern(
                        mir::Type{mir::PointerType{
                            .pointee = carrier_type,
                            .ownership = mir::PointerOwnership::kBorrowed}})));
          },
          [&](const support::VectorCarrier& vector) {
            return BuildBufferDataCall(
                unit, block, object, carrier_type, vector);
          },
          [&](const support::OpenArrayCarrier&) {
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kDpiOpenArrayHandle,
                                    .receiver = object},
                            .arguments = {}},
                    .type = unit.types.Intern(
                        mir::Type{mir::RuntimeLibraryType{
                            .kind = mir::RuntimeLibraryKind::
                                kDpiOpenArrayHandle}})});
          }},
      carrier);
}

// The SV value one boundary object holds once the call returns, the read-back
// that pairs with its build: a scalar marshals its by-value carrier, a vector
// reads its buffer's canonical chunks, an open array reads its whole image.
// Each yields one value, so the store into the actual is the ordinary one
// whatever the carrier.
//
// What no carrier carries is the destination's declared representation, so
// each read is told that type: a vector's destination is packed, and an open
// array's is a nest of fixed-size arrays whose counts and leaves its type
// states.
auto BuildBoundaryReadback(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const support::DpiCarrier& carrier, mir::ExprId object,
    mir::TypeId carrier_type, mir::TypeId sv_type) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  // A canonical chunk buffer is read by an entry over two operands, while an
  // open array's image is read by an entry the image itself answers, so each
  // arm states its own callee and the shared step is only the interning.
  const auto read = [&](mir::Direct callee,
                        std::vector<mir::ExprId> arguments) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = std::move(callee),
                    .arguments = std::move(arguments)},
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
                mir::Direct{
                    .target = VectorReadBuiltin(vector),
                    .type_arguments = {sv_type}},
                {BuildBufferDataCall(
                    unit, block, object, carrier_type, vector)});
          },
          [&](const support::OpenArrayCarrier&) {
            return read(
                mir::Direct{
                    .target = support::BuiltinFn::kDpiOpenArrayValue,
                    .receiver = object,
                    .type_arguments = {sv_type}},
                {});
          }},
      carrier);
}

// The MIR type a foreign entry point returns. A task carries no SV return
// value; its C entry returns the disable-acknowledgment int instead (LRM 35.8),
// and both directions of the boundary read that from here, so a call and the
// entry it reaches can never disagree about what crosses back.
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
auto MakeForeignSymbolCall(
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
      .type = ForeignBoundaryReturnType(unit, import.ret_abi, import.is_task)};
}

// The function import call the boundary needs no statements for: every actual
// crosses by value and no callback has to be arranged, so the call is one
// expression -- no boundary objects, no extent. Each actual is marshaled
// to its ABI carrier, the foreign symbol is called over the carriers, and a
// non-void result is marshaled back to the declared SV type. A task never
// reaches here; its await needs a coroutine.
template <ExprLowerer Lowerer>
auto LowerForeignImportDirect(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  auto& block = *frame.current_block;
  const auto& hir_exprs = lowerer.HirExprs();

  // A DPI-C import declares every formal (LRM 35.5), so the call carries an
  // actual for each: nothing here is elidable.
  const std::vector<hir::ExprId> operands = RequiredOperands(c);
  std::vector<mir::ExprId> carrier_args;
  carrier_args.reserve(operands.size());
  for (std::size_t i = 0; i < operands.size(); ++i) {
    auto sv_or = lowerer.LowerExpr(hir_exprs.Get(operands[i]), frame);
    if (!sv_or) return std::unexpected(std::move(sv_or.error()));
    const mir::ExprId sv_id = block.exprs.Add(*std::move(sv_or));
    carrier_args.push_back(MarshalSvToCarrier(
        unit_lowerer, block, sv_id, import.params[i].carrier,
        ForeignHold::kForTheCall));
  }

  mir::Expr foreign_call =
      MakeForeignSymbolCall(unit, import, std::move(carrier_args));
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
    Lowerer& lowerer, const WalkFrame& cframe, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import)
    -> diag::Result<std::optional<mir::LocalId>> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();

  mir::Block& body = *cframe.current_block;

  // One output / inout argument, and what its boundary object needs to be read
  // back into the actual once the call returns.
  struct Writeback {
    mir::LocalId temp{};
    mir::TypeId carrier_type{};
    Lvalue actual;
    support::DpiCarrier carrier{};
    mir::TypeId sv_type{};
  };
  std::vector<Writeback> writebacks;

  const std::vector<hir::ExprId> operands = RequiredOperands(c);
  std::vector<mir::ExprId> call_args;
  call_args.reserve(operands.size());
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const hir::ExprId actual = operands[i];
    const hir::DpiParamAbi& param = import.params[i];
    const support::DpiCarrier& carrier = param.carrier;

    // An actual the foreign side writes back to is read to seed its boundary
    // object and written once the call returns, and the source writes it once.
    std::optional<Lvalue> written_place;
    mir::TypeId crossing_type{};
    mir::ExprId seed_sv{};
    if (support::DpiDirectionWritesBack(param.direction)) {
      const hir::Expr& actual_hir = hir_exprs.Get(actual);
      auto place_or = LowerLvalue(lowerer, actual_hir, cframe);
      if (!place_or) return std::unexpected(std::move(place_or.error()));
      auto taken = ReadThenWrite(unit_lowerer, cframe, *std::move(place_or));
      if (!taken) return std::unexpected(std::move(taken.error()));
      // What crosses is a value of the formal's type, which the actual's is
      // brought to as an assignment to the formal would bring it (LRM 13.5).
      // An open array has no type of its own to bring it to: its shape is the
      // actual's, fixed only at the call (LRM 35.6.1.1). The read-back is into
      // the same type.
      crossing_type = std::holds_alternative<support::OpenArrayCarrier>(carrier)
                          ? unit_lowerer.TranslateType(actual_hir.type)
                          : unit_lowerer.TranslateType(param.sv_type);
      seed_sv = ConvertToType(unit, body, taken->incoming, crossing_type);
      written_place = std::move(taken->lvalue);
    } else {
      auto sv_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
      if (!sv_or) return std::unexpected(std::move(sv_or.error()));
      crossing_type = sv_or->type;
      seed_sv = body.exprs.Add(*std::move(sv_or));
    }

    if (CrossesByValue(param)) {
      call_args.push_back(MarshalSvToCarrier(
          unit_lowerer, body, seed_sv, carrier, ForeignHold::kForTheCall));
      continue;
    }

    // Every other actual seeds a boundary object from the actual's current
    // value. inout requires that initial value (LRM 35.5.1.2); for output the
    // initial carrier value is implementation-defined, so seeding from the
    // actual is a legal, uniform choice; an input that crosses this way seeds
    // the same way. The foreign side reads and writes the object, and the
    // read-back below lands the result in the actual's cell.
    const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
    const mir::LocalId temp = cframe.bindings->DeclareAnonymous(carrier_type);
    auto init_or = BuildBoundaryInit(
        unit_lowerer, body, carrier, seed_sv, crossing_type,
        hir_exprs.Get(actual).type, carrier_type);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    body.AppendStmt(mir::LocalDeclStmt{.target = temp, .init = *init_or});
    call_args.push_back(
        BuildBoundaryArgument(unit, body, carrier, temp, carrier_type));

    if (written_place.has_value()) {
      writebacks.push_back(
          Writeback{
              .temp = temp,
              .carrier_type = carrier_type,
              .actual = *std::move(written_place),
              .carrier = carrier,
              .sv_type = crossing_type});
    }
  }

  mir::Expr foreign_call =
      MakeForeignSymbolCall(unit, import, std::move(call_args));
  const mir::TypeId call_type = foreign_call.type;

  // A call that hands anything back captures it in a temp so the copy-backs run
  // before it is read. A task hands back no SV value and still hands back the
  // protocol's int (LRM 35.8), which is read only to hold the foreign side to
  // what the protocol asks of it.
  std::optional<mir::LocalId> ret_temp;
  if (call_type != unit.builtins.void_type) {
    ret_temp = cframe.bindings->DeclareAnonymous(call_type);
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = *ret_temp,
            .init = body.exprs.Add(std::move(foreign_call))});
  } else {
    body.AppendStmt(
        mir::ExprStmt{.expr = body.exprs.Add(std::move(foreign_call))});
  }

  for (const Writeback& wb : writebacks) {
    const mir::ExprId temp_ref = body.LocalPlace(wb.temp, wb.carrier_type);

    const mir::ExprId rhs_id = BuildBoundaryReadback(
        unit_lowerer, cframe, wb.carrier, temp_ref, wb.carrier_type,
        wb.sv_type);
    auto stored = AppendStores(unit_lowerer, cframe, wb.actual, rhs_id);
    if (!stored) return std::unexpected(std::move(stored.error()));
  }

  // LRM 35.9 items b and c, checked where the foreign frame has just returned
  // and its evidence is still in hand. Only a `context` import may reach an
  // exported subroutine (LRM 35.5.3), and a plain one that does is refused at
  // the export, so a plain import's boundary carries no check and stays as
  // cheap as any call.
  if (import.is_context) {
    if (import.is_task) {
      AppendRuntimeEffectStmt(
          unit_lowerer, body, support::BuiltinFn::kCheckImportTaskAcknowledged,
          {body.ReadLocal(*ret_temp, call_type)});
    } else {
      AppendRuntimeEffectStmt(
          unit_lowerer, body,
          support::BuiltinFn::kCheckImportFunctionAcknowledged, {});
    }
  }

  return ret_temp;
}

// The scope a `context` import makes current (LRM 35.5.3): the instantiated
// scope of its own declaration, reached over the distance the walk already
// knows. A declaration in a namespace that is never instantiated -- a package
// or `$unit` scope -- has none, and enters a null one rather than leaving the
// call to report an enclosing chain entry that is not its declaration's.
auto BuildDeclaringScopeExpr(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    std::optional<hir::StructuralHops> declaring_scope) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  if (declaring_scope.has_value()) {
    return BuildEnclosingScopeReceiver(
        frame, unit, mir::EnclosingHops{.value = declaring_scope->value});
  }
  return frame.current_block->exprs.Add(
      mir::Expr{.data = mir::NullLiteral{}, .type = unit.builtins.scope_ptr});
}

// Closes the extent a `context` import's foreign call runs inside (LRM 35.5.3)
// around `boundary`, whose statements the caller has already lowered into it.
// Entering names the declaration's scope; the cleanup gives back whatever was
// current before, on every way out -- a return, a raised effect, or falling off
// the end.
//
// Only a `context` import may reach an exported subroutine (LRM 35.5.3), so it
// is the only one whose execution the standard lets be told to stop while the
// foreign side holds it; a plain import that reaches one anyway is refused
// there, and its execution leaves at its next suspension. The extent is
// therefore also where that is answered for: the
// foreign frame's own obligation is checked where it returns, and this
// execution leaves if it has been told to, which is the point it regains
// control at when the foreign side consumed no simulation time and suspended
// nothing.
void CloseDpiScopeExtent(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Block&& boundary,
    std::optional<hir::StructuralHops> declaring_scope) {
  mir::Block& block = *frame.current_block;
  AppendRuntimeEffectStmt(
      unit_lowerer, block, support::BuiltinFn::kEnterDpiScope,
      {BuildDeclaringScopeExpr(unit_lowerer, frame, declaring_scope)});

  mir::Block cleanup;
  AppendRuntimeEffectStmt(
      unit_lowerer, cleanup, support::BuiltinFn::kLeaveDpiScope, {});

  block.AppendFinally(std::move(boundary), std::move(cleanup));
  AppendRuntimeEffectStmt(
      unit_lowerer, block, support::BuiltinFn::kTakeDepartureIfDue, {});
}

// Whether crossing the boundary takes statements rather than one expression.
// Two things ask for them. An actual the foreign side writes through needs an
// object of its own, declared before the call and read back after it. And a
// context import (LRM 35.5.3) may call back into SystemVerilog, so the scope
// the callback resolves against is pushed before the call and popped after.
// Neither fits in an expression, while a call with no actual and no callback
// to arrange is one.
//
// Settled from the declaration, before the first actual is lowered, because an
// expression belongs to the block it was lowered into and cannot move after.
[[nodiscard]] auto BoundaryTakesStatements(const hir::ForeignImportDecl& import)
    -> bool {
  return import.is_context ||
         !std::ranges::all_of(import.params, CrossesByValue);
}

// The function import call whose boundary takes statements and whose foreign
// side hands a value back: the steps marshal, call, and marshal the result, and
// the block expression yields it.
template <ExprLowerer Lowerer>
auto LowerForeignImportSequenced(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import,
    std::optional<hir::StructuralHops> declaring_scope, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  BlockBuilder steps(frame);
  const WalkFrame& cframe = steps.Frame();

  // LRM 35.5.3 instruments a call only where the import is `context`, so a
  // plain import's boundary is the sequence itself and stands inside no extent.
  mir::Block extent_body;
  const WalkFrame bframe =
      import.is_context ? cframe.WithBlock(&extent_body) : cframe;

  auto ret_temp = PopulateForeignImportBoundary(lowerer, bframe, c, import);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  if (!ret_temp->has_value()) {
    throw InternalError(
        "LowerForeignImportSequenced: a boundary that returns nothing has no "
        "expression to stand as -- please report this as a bug");
  }
  mir::Block& body = *bframe.current_block;
  const mir::TypeId call_type =
      CarrierTypeId(unit, support::ScalarCarrier{import.ret_abi});
  const mir::ExprId ret_ref = body.LocalPlace(**ret_temp, call_type);
  const mir::ExprId result_id = body.exprs.Add(MarshalCarrierToSv(
      unit_lowerer, bframe, ret_ref, support::ScalarCarrier{import.ret_abi},
      result_type));
  if (!import.is_context) return steps.Build(result_id);

  // The value is settled inside the extent and read after it, so the binding
  // holding it is declared ahead of the extent rather than within it.
  const mir::LocalId result = cframe.bindings->DeclareAnonymous(result_type);
  steps.Body().AppendStmt(
      mir::LocalDeclStmt{
          .target = result,
          .init = steps.Body().exprs.Add(
              BuildDefaultValueExpr(unit, steps.Body(), result_type))});
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, body.LocalPlace(result, result_type),
                  result_id))});
  CloseDpiScopeExtent(
      unit_lowerer, cframe, std::move(extent_body), declaring_scope);
  return steps.Build(steps.Body().LocalPlace(result, result_type));
}

// The task import call (LRM 35.5.2): a task has no SV return, so the call
// finishes as a coroutine -- the awaitable the caller drives, the same call
// protocol as a native task enable (LRM 35.8), uniform whether or not the
// foreign side consumes time. The boundary always crosses inside the closure,
// even where every actual crosses by value, because the await needs a coroutine
// to drive rather than a value to read. Calling the closure starts it: the
// call hands the body the box the closure is made in, and answers the
// coroutine the statement lowering awaits.
//
// A foreign task may consume simulation time, which it does by calling back an
// exported task that suspends. The foreign call therefore runs on a fiber whose
// native stack can be parked and resumed, and awaiting that fiber is what
// suspends the caller. Both facts are stated here as ordinary MIR -- a call to
// the runtime's fiber entry over a closure of the whole boundary, awaited --
// rather than left for a backend to infer from the callee being a foreign task.
template <ExprLowerer Lowerer>
auto LowerForeignImportTask(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportDecl& import,
    std::optional<hir::StructuralHops> declaring_scope)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  ClosureBuilder outer(unit, frame, support::ClosureKind::kFnOnce);

  // What hands control to the foreign side here is the await, not the call
  // inside the fiber: the scope has to be current before the native stack is
  // entered and still current when the fiber resumes, so the extent is the
  // awaiting coroutine's. The whole await is lowered through the block that
  // becomes the extent's body, so the closure and every operand it captures
  // resolve against one arena.
  mir::Block extent_body;
  const WalkFrame await_frame =
      import.is_context ? outer.Frame().WithBlock(&extent_body) : outer.Frame();

  ClosureBuilder fiber_body(unit, await_frame, support::ClosureKind::kFn);
  auto ret_temp =
      PopulateForeignImportBoundary(lowerer, fiber_body.Frame(), c, import);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  mir::Block& body = *await_frame.current_block;
  const mir::ExprId fiber_id =
      BuildKeptClosure(unit, body, fiber_body.BuildVoid());
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  // A foreign task may consume simulation time (LRM 35.5.1.1), so its call is
  // a suspending construct like a delay: the runtime carries it on a stack of
  // its own and answers whether this execution must park at all, and the
  // suspension that follows waits for nothing else.
  const mir::ExprId run_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kRunForeignTaskOnFiber},
                  .arguments = {runtime_id, fiber_id}},
          .type = unit.builtins.machine_bool});
  body.AppendStmt(BuildWaitStmt(unit_lowerer, body, run_id));
  if (import.is_context) {
    CloseDpiScopeExtent(
        unit_lowerer, outer.Frame(), std::move(extent_body), declaring_scope);
  }
  return outer.BuildCallOnce();
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
        mir::Type{mir::RuntimeLibraryType{
            .kind = mir::RuntimeLibraryKind::kDpiOpenArrayHandle}});
  }
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    const mir::TypeId chunk = unit.types.Intern(
        mir::Type{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicChunk
                                    : mir::RuntimeLibraryKind::kDpiBitChunk}});
    const mir::Mutability mutability =
        direction == support::DpiDirection::kInput ? mir::Mutability::kReadOnly
                                                   : mir::Mutability::kMutable;
    return unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = chunk,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mutability}});
  }
  const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
  if (support::DpiDirectionWritesBack(direction)) {
    return unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = carrier_type,
            .ownership = mir::PointerOwnership::kBorrowed}});
  }
  return carrier_type;
}

}  // namespace

auto MakeForeignSignature(
    mir::CompilationUnit& unit, std::span<const hir::DpiParamAbi> params,
    support::DpiScalarAbi ret_abi, bool is_task) -> mir::CallableCode {
  mir::CallableCode code{
      .params = {},
      .receiver = std::nullopt,
      .result_type = ForeignBoundaryReturnType(unit, ret_abi, is_task),
      .locals = {},
      .named_locals = {},
      .body = std::nullopt};
  CallableBindings bindings(unit, code);
  code.params.reserve(params.size());
  for (const auto& param : params) {
    code.params.push_back(bindings.DeclareAnonymous(
        ForeignBoundaryType(unit, param.carrier, param.direction)));
  }
  return code;
}

auto MakeForeignImportDecl(
    mir::CompilationUnit& unit, const hir::ForeignImportDecl& import)
    -> mir::CallableDecl {
  return mir::CallableDecl{
      .code = MakeForeignSignature(
          unit, import.params, import.ret_abi, import.is_task),
      .foreign = mir::ForeignLinkage{.foreign_name = import.foreign_name},
      .virtual_dispatch = std::nullopt,
      .type_parameters = {},
      .defining_unit = std::nullopt};
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
  if (BoundaryTakesStatements(import)) {
    return LowerForeignImportSequenced(
        lowerer, frame, c, import, ref.declaring_scope, result_type);
  }
  return LowerForeignImportDirect(lowerer, frame, c, import, result_type);
}

template auto LowerForeignImportCall(
    ProcessLowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template auto LowerForeignImportCall(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::CallExpr& c, const hir::ForeignImportRef& ref,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

auto LowerForeignImportCallStmtForm(
    ProcessLowerer& lowerer, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref)
    -> std::optional<diag::Result<mir::Stmt>> {
  const hir::ForeignImportDecl& import =
      lowerer.Owner().Hir().foreign_imports.Get(ref.id);

  // A task hands its enabler a coroutine to await, and a returning function
  // hands it the marshaled result, so both are expressions the caller consumes.
  // A boundary taking no statements is the foreign call itself.
  if (import.is_task || ReturnsValue(import) ||
      !BoundaryTakesStatements(import)) {
    return std::nullopt;
  }

  BlockBuilder steps(frame);
  const WalkFrame& cframe = steps.Frame();

  mir::Block extent_body;
  const WalkFrame bframe =
      import.is_context ? cframe.WithBlock(&extent_body) : cframe;

  auto crossed = PopulateForeignImportBoundary(lowerer, bframe, c, import);
  if (!crossed) {
    return diag::Result<mir::Stmt>{std::unexpected(std::move(crossed.error()))};
  }
  if (import.is_context) {
    CloseDpiScopeExtent(
        lowerer.Owner(), cframe, std::move(extent_body), ref.declaring_scope);
  }
  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = label;
  return diag::Result<mir::Stmt>{std::move(stmt)};
}

// Closes, in `block`, the region a foreign caller's call into the design runs
// inside around `body`, whose statements the caller has already built. The
// frame above belongs to another language, which no departure or error may
// unwind into (LRM 35.9): the region claims every departure and every error,
// the design's or the tool's -- which is reported and stops the execution where
// it is received -- and consumes it. Only the release of the whole stack once
// the run is over, which receiving raises again, passes through it. Whether a
// departure is due is answered again where control comes back to the design --
// at once at a `context` import's boundary, and after a plain import at the
// execution's next suspension -- so landing it here loses nothing.
void CloseForeignCallerRegion(
    mir::CompilationUnit& unit, CallableBindings& bindings, mir::Block& block,
    mir::Block&& body) {
  const mir::TypeId effect_type = unit.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kControlEffect}});
  const mir::BlockId region = block.child_scopes.Add(std::move(body));
  const mir::LocalId caught = bindings.DeclareAnonymous(effect_type);
  const mir::BlockId consumed = block.child_scopes.Add(mir::Block{});
  block.AppendStmt(
      mir::TryStmt{.body = region, .caught = caught, .handler = consumed});
}

// Appends the return a foreign caller reaches where the call's region landed
// a departure or an error instead of answering. An exported name states it
// from the kind of subroutine it is and what its result crosses as, so its
// symbol and its entry answer alike. A task always returns the protocol's int
// (LRM 35.8), which says whether a disable is active, so a task's two ways out
// meet here, and a function returning nothing answers nothing. LRM 35.9 fixes
// that value for a task alone, so a function returning a value answers the
// zero of its result's C type, which needs no SystemVerilog type to state and,
// for a string, no storage to keep.
void ReturnTheFallbackAnswer(
    UnitLowerer& module, mir::Block& block, hir::SubroutineKind kind,
    support::DpiScalarAbi ret_abi) {
  mir::CompilationUnit& unit = module.Unit();
  switch (kind) {
    case hir::SubroutineKind::kTask:
      block.AppendStmt(
          mir::ReturnStmt{
              .value = block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target =
                                          support::BuiltinFn::kDisableIsActive},
                              .arguments = {block.exprs.Add(
                                  BuildCurrentRuntimeCallExpr(module))}},
                      .type =
                          ForeignBoundaryReturnType(unit, ret_abi, true)})});
      return;
    case hir::SubroutineKind::kFunction:
      break;
    case hir::SubroutineKind::kConstraintBlock:
    case hir::SubroutineKind::kStaticConstraintBlock:
      throw InternalError(
          "ReturnTheFallbackAnswer: a constraint block is exported to C -- "
          "please report this as a bug");
  }
  const mir::TypeId carrier =
      CarrierTypeId(unit, support::ScalarCarrier{ret_abi});
  const auto zero = [&]() -> std::optional<mir::ExprId> {
    switch (ret_abi) {
      case support::DpiScalarAbi::kVoid:
        return std::nullopt;
      case support::DpiScalarAbi::kBitScalar:
      case support::DpiScalarAbi::kLogicScalar:
      case support::DpiScalarAbi::kByte:
      case support::DpiScalarAbi::kShortInt:
      case support::DpiScalarAbi::kInt:
      case support::DpiScalarAbi::kLongInt:
        return block.exprs.Add(
            mir::Expr{
                .data =
                    mir::CastExpr{
                        .operand = mir::BuildMachineIntLiteral(unit, block, 0)},
                .type = carrier});
      case support::DpiScalarAbi::kReal:
        return block.exprs.Add(
            mir::Expr{
                .data = mir::MachineFloatLiteral{.value = 0.0},
                .type = carrier});
      case support::DpiScalarAbi::kString:
        return block.exprs.Add(mir::MakeStringLiteral(carrier, ""));
      case support::DpiScalarAbi::kChandle:
        return block.exprs.Add(
            mir::Expr{.data = mir::NullLiteral{}, .type = carrier});
    }
    throw InternalError("ReturnTheFallbackAnswer: unknown DpiScalarAbi");
  };
  block.AppendStmt(mir::ReturnStmt{.value = zero()});
}

// Closes an export entry around the boundary `body`: the checks of LRM 35.9
// item d and of LRM 35.5.3 on where the call came from, then the departure an
// execution that must stop owes before any statement of the body runs -- the
// foreign side may call in again after a disable or after the run ended, and
// nothing of the design runs then -- all inside the region that lands whatever
// no region of the body claimed, then the fallback answer. The entry is that
// region whether or not a symbol stands in front of it: where the name belongs
// to a scope, one does, and where it belongs to a namespace, the entry is the
// symbol and the foreign caller's frame is directly above it.
void CloseForeignExportEntry(
    UnitLowerer& module, CallableBindings& bindings, mir::Block& entry,
    mir::Block&& body, hir::SubroutineKind kind,
    support::DpiScalarAbi ret_abi) {
  mir::Block ran;
  AppendRuntimeEffectStmt(
      module, ran, support::BuiltinFn::kCheckExportReachable, {});
  AppendRuntimeEffectStmt(
      module, ran, support::BuiltinFn::kTakeDepartureIfDue, {});
  ran.AppendStmt(
      mir::BlockStmt{.scope = ran.child_scopes.Add(std::move(body))});
  CloseForeignCallerRegion(module.Unit(), bindings, entry, std::move(ran));
  ReturnTheFallbackAnswer(module, entry, kind, ret_abi);
}

auto SynthesizeForeignExportEntry(
    UnitLowerer& module, const WalkFrame& context_frame,
    mir::DirectTarget target, mir::TypeId result_type,
    const hir::ForeignExportDecl& export_decl) -> ForeignExportEntry {
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
  const bool is_task = unit.types.Get(result_type).Is<mir::CoroutineType>();
  const hir::SubroutineKind kind =
      is_task ? hir::SubroutineKind::kTask : hir::SubroutineKind::kFunction;

  // A subroutine of a scope is compiled once per specialization of that scope
  // while its DPI-C name is one program-global symbol, so its entry is reached
  // through the scope rather than being the symbol: the entry leads with the
  // scope it runs against, and the symbol resolves it against the scope in
  // effect. A package subroutine has no receiver and a package has one form, so
  // its entry is the symbol and publishes the prototype directly.
  const bool through_scope =
      std::holds_alternative<mir::CallableTarget>(target);

  // The entry publishes the same signature an import of the same shape would,
  // and then defines it -- which is the whole difference between the two
  // directions of the boundary.
  mir::CallableCode code = MakeForeignSignature(
      unit, export_decl.params, export_decl.ret_abi, is_task);
  const std::vector<mir::LocalId> params = code.params;

  // The prototype the program-global symbol publishes, which is the entry's own
  // formals and result. An entry reached through a scope leads with that scope,
  // which the symbol supplies and the prototype never mentions.
  std::vector<mir::TypeId> prototype_params;
  prototype_params.reserve(params.size());
  for (const mir::LocalId param : params) {
    prototype_params.push_back(code.locals.Get(param).type);
  }
  const mir::TypeId signature = unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(prototype_params), .result = code.result_type}});

  code.body.emplace();
  CallableBindings bindings(unit, code);
  mir::Block& entry = code.Body();

  // The boundary's own statements, which the entry's region closes around once
  // they are built.
  mir::Block body;

  std::optional<mir::LocalId> scope_param;
  if (through_scope) {
    scope_param = bindings.DeclareAnonymous(unit.builtins.scope_ptr);
    code.params.insert(code.params.begin(), *scope_param);
  }

  const WalkFrame body_frame =
      context_frame.WithBlock(&body).WithBindings(&bindings);

  // A subroutine of a scope (LRM 8.6) dispatches on a `self`, narrowed from the
  // generic scope this entry was reached on -- valid because the lookup that
  // found the entry found it on that very scope -- and bound as a body local
  // first so the whole body (recovery, marshaling, call, writeback) is MIR a
  // backend renders mechanically. A package subroutine (LRM 26.3) takes nothing
  // ahead of the formals the source wrote.
  std::optional<mir::ExprId> receiver;
  if (scope_param.has_value()) {
    const mir::TypeId self_type =
        context_frame.current_class.cls->self_pointer_type;
    const mir::LocalId self_local =
        bindings.Declare(ReceiverOrigin{}, self_type);
    const mir::ExprId narrowed = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CastExpr{
                    .operand =
                        body.ReadLocal(*scope_param, unit.builtins.scope_ptr)},
            .type = self_type});
    body.AppendStmt(mir::LocalDeclStmt{.target = self_local, .init = narrowed});
    receiver =
        BuildObjectDeref(unit, body, body.LocalPlace(self_local, self_type));
  }

  const auto param_ref = [&](mir::Block& block, std::size_t i) -> mir::ExprId {
    return block.ReadLocal(params[i], code.locals.Get(params[i]).type);
  };

  // The completion payload's components, in the callee's payload order: the
  // function return (when non-void) first, then each `output` / `inout` formal
  // in declaration order, so each component's index lines up with the payload
  // the callee returns. An `inout` is written back against what it held on the
  // way in, which an `output` has none of.
  struct ReturnedValue {};
  struct OutputFormal {
    std::size_t param_index;
  };
  struct InoutFormal {
    std::size_t param_index;
    mir::LocalId held_before;
  };
  struct Component {
    mir::TypeId sv_type;
    std::variant<ReturnedValue, OutputFormal, InoutFormal> role;
  };
  std::vector<Component> components;
  const bool has_return = export_decl.ret_abi != support::DpiScalarAbi::kVoid;
  if (has_return) {
    components.push_back(
        Component{
            .sv_type = module.TranslateType(export_decl.ret_sv_type),
            .role = ReturnedValue{}});
  }

  // Marshal each `input` / `inout` argument to an explicit SV-typed temporary
  // before the call, so the read of an `inout`'s incoming value is sequenced
  // ahead of the copy-back that later overwrites it, and multiple arguments do
  // not alias inside one nested call expression. An `output` formal is not a
  // method parameter -- it rides the completion payload (LRM 13.5).
  std::vector<mir::ExprId> call_args;
  // Reads formal `i` into an SV-typed local the call is handed. A vector reads
  // its incoming canonical buffer; a scalar converts the carrier `carrier`
  // reaches, which is the formal itself or what it points at.
  const auto read_in = [&](std::size_t i, mir::TypeId sv_type,
                           const auto& carrier) -> mir::LocalId {
    const hir::DpiParamAbi& p = export_decl.params[i];
    mir::ExprId sv_init{};
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      sv_init = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = VectorReadBuiltin(*vec),
                              .type_arguments = {sv_type}},
                      .arguments = {param_ref(body, i)}},
              .type = sv_type});
    } else {
      sv_init = body.exprs.Add(MarshalCarrierToSv(
          module, body_frame, carrier(), p.carrier, sv_type));
    }
    const mir::LocalId sv_in = bindings.DeclareAnonymous(sv_type);
    body.AppendStmt(mir::LocalDeclStmt{.target = sv_in, .init = sv_init});
    call_args.push_back(body.ReadLocal(sv_in, sv_type));
    return sv_in;
  };
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    const mir::TypeId sv_type = module.TranslateType(p.sv_type);
    switch (p.direction) {
      case support::DpiDirection::kInput:
        read_in(i, sv_type, [&] { return param_ref(body, i); });
        break;
      case support::DpiDirection::kInout: {
        const mir::LocalId sv_in = read_in(i, sv_type, [&] {
          return body.Deref(
              unit.types, param_ref(body, i), CarrierTypeId(unit, p.carrier));
        });
        components.push_back(
            Component{
                .sv_type = sv_type,
                .role = InoutFormal{.param_index = i, .held_before = sv_in}});
        break;
      }
      case support::DpiDirection::kOutput:
        components.push_back(
            Component{
                .sv_type = sv_type, .role = OutputFormal{.param_index = i}});
        break;
    }
  }
  // Foreign code reads SystemVerilog data only through the exports it calls
  // (LRM 35.5.3), so an evaluation of a waited expression that called into it
  // is what the function reports into; with none in progress, it reports
  // nowhere and runs.
  if (const std::optional<mir::TypeId> report = ReportParamTypeOf(unit, kind)) {
    call_args.push_back(body.exprs.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kEvaluationInProgress},
            {body.exprs.Add(BuildCurrentRuntimeCallExpr(module))}, *report)));
  }

  // The completion payload the writeback loop below destructures is the
  // exported subroutine's result either way; for a task it is the coroutine's
  // payload, reached past the protocol.
  const mir::TypeId method_result_type = result_type;
  const mir::TypeId payload_type =
      is_task
          ? unit.types.Get(method_result_type).Get<mir::CoroutineType>().payload
          : method_result_type;
  const mir::ExprId method_call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = std::move(target), .receiver = receiver},
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

  // Bind the completion value to a local every component projects out of. A
  // completion is a product at every count, so it binds the same way whether or
  // not anything is projected back out of it.
  const mir::LocalId completion = bindings.DeclareAnonymous(payload_type);
  body.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_source});
  const auto component_value = [&](mir::Block& block,
                                   std::size_t k) -> mir::ExprId {
    return ProjectCompletionComponent(
        block, completion, payload_type,
        base::ComponentIndex{static_cast<std::uint32_t>(k)},
        components[k].sv_type);
  };

  // Writes component `k`, formal `i`, back through its foreign pointer, in
  // `block`: a scalar stores its marshaled carrier through the pointer; a
  // vector reshapes the SV value into the foreign-owned canonical buffer.
  const auto write_back = [&](mir::Block& block, std::size_t k, std::size_t i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    const mir::ExprId value = component_value(block, k);
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target = VectorWriteBuiltin(*vec)},
                              .arguments = {param_ref(block, i), value}},
                      .type = void_type})});
      return;
    }
    const mir::TypeId carrier_type = CarrierTypeId(unit, p.carrier);
    const mir::ExprId carrier = MarshalSvToCarrier(
        module, block, value, p.carrier, ForeignHold::kUntilTheImportReturns);
    const mir::ExprId place =
        block.Deref(unit.types, param_ref(block, i), carrier_type);
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeAssignExpr(unit.builtins, place, carrier))});
  };

  // An `output` is always written. An `inout` is written only where the
  // subroutine changed it: SystemVerilog writes an address into an inout string
  // only where it effects a change, so the caller keeps its own pointer and the
  // storage behind it (LRM Annex H.8.10), and for every other carrier writing
  // back what was there is the same as not writing. A change is any changed
  // bit, so an x or a z, and a real's sign, each count as one.
  for (std::size_t k = 0; k < components.size(); ++k) {
    std::visit(
        Overloaded{
            [&](const ReturnedValue&) {},
            [&](const OutputFormal& output) {
              write_back(body, k, output.param_index);
            },
            [&](const InoutFormal& inout) {
              mir::Block changed;
              write_back(changed, k, inout.param_index);
              const mir::ExprId same = BuildBitIdentity(
                  unit, body, component_value(body, k),
                  body.ReadLocal(inout.held_before, components[k].sv_type));
              body.AppendStmt(
                  mir::IfStmt{
                      .condition = BuildConditionNot(unit, body, same),
                      .then_scope = body.child_scopes.Add(std::move(changed)),
                      .else_scope = std::nullopt});
            }},
        components[k].role);
  }

  if (has_return) {
    const mir::ExprId ret_carrier = MarshalSvToCarrier(
        module, body, component_value(body, 0),
        support::ScalarCarrier{export_decl.ret_abi},
        ForeignHold::kUntilTheImportReturns);
    body.AppendStmt(mir::ReturnStmt{.value = ret_carrier});
  }

  CloseForeignExportEntry(
      module, bindings, entry, std::move(body), kind, export_decl.ret_abi);

  // The entry's identity is its linkage name: it is a program-global symbol in
  // the DPI name space, not an SV declaration the source can call (LRM 35.4,
  // 35.7), so it shares no name with the subroutine it dispatches into.
  return ForeignExportEntry{
      .code = std::move(code),
      .linkage = mir::ForeignLinkage{.foreign_name = export_decl.foreign_name},
      .signature = signature,
      .kind = kind,
      .ret_abi = export_decl.ret_abi};
}

void PublishForeignScopeName(
    UnitLowerer& module, const ForeignExportEntry& exported) {
  mir::CompilationUnit& unit = module.Unit();
  const mir::ForeignLinkage& linkage = exported.linkage;
  // Two scopes of one unit may export one name (LRM 35.4), and both the
  // prototype and the definition are the same either way, so the second states
  // nothing new. Across units the repeat is what this symbol's linkage is for;
  // within one artifact it would simply be a name stated twice.
  const bool already = std::ranges::any_of(
      unit.foreign_scope_entries, [&](const mir::ForeignScopeEntry& stated) {
        return stated.linkage.foreign_name == linkage.foreign_name;
      });
  if (already) {
    return;
  }

  const mir::MachineFunctionType prototype =
      unit.types.Get(exported.signature).Get<mir::MachineFunctionType>();

  mir::CallableCode code = mir::CallableCode::Defined();
  code.body.emplace();
  CallableBindings bindings(unit, code);

  // Resolving the entry is the design's own work and can fail as the design's
  // error -- a call from outside a `context` import's call, a scope that
  // declares no such export -- so it runs inside the same region the entry
  // stands in, and answers the same fallback where that landed.
  mir::Block body;

  std::vector<mir::TypeId> entry_params{unit.builtins.scope_ptr};
  for (const mir::TypeId type : prototype.params) {
    code.params.push_back(bindings.DeclareAnonymous(type));
    entry_params.push_back(type);
  }
  code.result_type = prototype.result;

  const mir::LocalId scope = bindings.DeclareAnonymous(unit.builtins.scope_ptr);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = scope,
          .init = body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target =
                                      support::BuiltinFn::kCurrentExportScope},
                          .arguments = {}},
                  .type = unit.builtins.scope_ptr})});

  const mir::ExprId scope_ref = body.ReadLocal(scope, unit.builtins.scope_ptr);
  const mir::ExprId name = body.exprs.Add(
      mir::MakeStringLiteral(
          unit.builtins.machine_c_string, linkage.foreign_name));
  const mir::ExprId entry = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindExportEntry},
                  .arguments = {scope_ref, name}},
          .type = mir::ErasedFunction(unit.types)});

  const mir::ExprId restored = body.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = entry},
          .type = unit.types.Intern(
              mir::Type{mir::MachineFunctionType{
                  .params = std::move(entry_params),
                  .result = prototype.result}})});
  std::vector<mir::ExprId> call_args;
  call_args.reserve(code.params.size() + 1);
  call_args.push_back(body.ReadLocal(scope, unit.builtins.scope_ptr));
  for (const mir::LocalId param : code.params) {
    call_args.push_back(body.ReadLocal(param, code.locals.Get(param).type));
  }
  const mir::ExprId call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Indirect{.code = restored},
                  .arguments = std::move(call_args)},
          .type = prototype.result});
  if (unit.types.Get(prototype.result).Is<mir::VoidType>()) {
    body.AppendStmt(mir::ExprStmt{.expr = call});
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = call});
  }
  CloseForeignCallerRegion(unit, bindings, code.Body(), std::move(body));
  ReturnTheFallbackAnswer(module, code.Body(), exported.kind, exported.ret_abi);

  unit.foreign_scope_entries.push_back(
      mir::ForeignScopeEntry{
          .linkage = linkage,
          .signature = exported.signature,
          .definition = std::move(code)});
}

}  // namespace lyra::lowering::hir_to_mir
