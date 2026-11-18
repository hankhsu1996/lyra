#include "lyra/lowering/hir_to_mir/part_bodies.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/entry_walk.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What every body of a part's type is handed besides the part: the selection,
// the depth of the step it takes, and where it hands what it comes to.
struct BodyParams {
  Handed selection;
  Handed depth;
  Handed sink;
};

// Whether a part of type `type` holds a value a reader reads out whole (LRM
// 37.17 detail 11): an integral value, a real, a string or an event variable.
// No step goes further into one; which bits of a packed value a reader names
// is the reader's.
auto HoldsReadValue(const hir::Type& type) -> bool {
  return type.IsIntegral() || type.Is<hir::RealType>() ||
         type.Is<hir::ShortRealType>() || type.Is<hir::RealTimeType>() ||
         type.Is<hir::StringType>() || type.Is<hir::EventType>();
}

// Whether a part of type `type` holding a value a reader reads takes one a
// write lands: every such value but an event's, which names an object rather
// than holding a value (LRM 37.27).
auto TakesAWrittenValue(const hir::Type& type) -> bool {
  return HoldsReadValue(type) && !type.Is<hir::EventType>();
}

// Whether `step` reaches a member of a union, tagged or not.
auto IsUnionMemberStep(const DescentStep& step) -> bool {
  return std::holds_alternative<StepToUnionMember>(step.to) ||
         std::holds_alternative<StepToTaggedMember>(step.to);
}

// The call of `entry` on `receiver`, answering `result`.
auto CallOn(
    mir::Builder block, support::BuiltinFn entry, mir::ExprId receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
  return block.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry, .receiver = receiver},
          std::move(arguments), result));
}

void Perform(mir::Builder block, mir::ExprId expr) {
  block.AppendStmt(mir::ExprStmt{.expr = expr});
}

// The entry handing a reader a value of type `held`.
auto SerializeEntryOf(const mir::CompilationUnit& unit, mir::TypeId held)
    -> support::BuiltinFn {
  const mir::Type& type = unit.types.Get(held);
  if (type.IsIntegral()) {
    return support::BuiltinFn::kSerializeBits;
  }
  if (type.Is<mir::RealType>()) {
    return support::BuiltinFn::kSerializeReal;
  }
  if (type.IsDeclaredBy(support::LibraryDecl::kString)) {
    return support::BuiltinFn::kSerializeText;
  }
  if (held == unit.builtins.event) {
    return support::BuiltinFn::kSerializeEvent;
  }
  throw InternalError(
      "hir_to_mir: a data object is read as a value it holds none of -- "
      "please report this as a bug");
}

// Appends to `block` the hand of `value`, a part holding a value a format
// reads, to the serializer `serializer`.
void AppendSerialized(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::ExprId serializer, mir::ExprId value) {
  Perform(
      block, CallOn(
                 block, SerializeEntryOf(unit, block.Get(value).type),
                 serializer, {value}, unit.builtins.void_type));
}

// What a write from a reader lands in a part of type `held`, taken from the
// deserializer `deserializer`: the bits of a packed value read into its type as
// a bit-stream cast reads them (LRM 6.24.3), a real at the width the part
// holds, or a string; none for a part of any other type, which no format of
// LRM Table 38-3 writes.
auto DeserializedValue(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::ExprId deserializer, mir::TypeId held) -> std::optional<mir::ExprId> {
  mir::Builder block = frame.Builder();
  const mir::Type& type = unit.types.Get(held);
  if (type.IsIntegral()) {
    return BuildFromBitstreamCast(
        unit, frame,
        CallOn(
            block, support::BuiltinFn::kDeserializeBits, deserializer, {},
            unit.builtins.bit_vec),
        held);
  }
  if (type.Is<mir::RealType>()) {
    return ConvertToType(
        unit, block,
        CallOn(
            block, support::BuiltinFn::kDeserializeReal, deserializer, {},
            unit.builtins.real),
        held);
  }
  if (type.IsDeclaredBy(support::LibraryDecl::kString)) {
    return CallOn(
        block, support::BuiltinFn::kDeserializeText, deserializer, {}, held);
  }
  return std::nullopt;
}

// Builds the three bodies of one type.
class PartBodyBuilder {
 public:
  PartBodyBuilder(UnitLowerer& owner, hir::TypeId type)
      : owner_(&owner), type_(type) {
  }

  auto Settle() -> mir::CallableCode;
  auto Read() -> mir::CallableCode;
  auto Write() -> mir::CallableCode;

 private:
  // A body taking the part as `part_type`, and what it is handed besides,
  // whose statements `fill` appends; `reports` says it is handed where the
  // reader's call was made, at which what fails in it is reported.
  using Fill = std::function<void(
      const WalkFrame&, const std::optional<Handed>&, const BodyParams&)>;
  auto Body(
      std::optional<mir::TypeId> part_type, support::LibraryDecl sink,
      bool reports, const Fill& fill) -> mir::CallableCode;

  // Appends to `frame`'s block `ends` where the selection ends at this depth,
  // and `goes_on` where it takes a further step.
  void AtSelectionEnd(
      const WalkFrame& frame, const BodyParams& params,
      const std::function<void(const WalkFrame&)>& ends,
      const std::function<void(const WalkFrame&)>& goes_on);

  // Appends to `frame`'s block the step the selection takes at this depth into
  // the value of this type, for each part it may reach: `take` is handed the
  // step and the part's type, and appends what follows.
  void EachStep(
      const WalkFrame& frame, const BodyParams& params,
      const std::function<
          void(const WalkFrame&, const DescentStep&, hir::TypeId)>& take);

  // The index the selection states at this depth into an array of type
  // `array`: an element's, read as an `int` as LRM 38.19 hands one; an entry's
  // of an associative array, as the key the selection names it by at the
  // index type; and one with a wildcard index, as an unsigned 64-bit integral,
  // as the design's own select hands the integral it is indexed by (LRM
  // 7.8.1).
  auto IndexOf(
      mir::Builder block, const BodyParams& params, const hir::Type& array)
      -> mir::ExprId;

  // The call of the body `body` of a part's type, handed `part`, the selection,
  // the next depth and the sink.
  void CallPartBody(
      mir::Builder block, mir::CallableId body, std::optional<mir::ExprId> part,
      const BodyParams& params);

  [[nodiscard]] auto Unit() const -> mir::CompilationUnit& {
    return owner_->Unit();
  }
  [[nodiscard]] auto Declared() const -> const hir::Type& {
    return owner_->Hir().types.Get(type_);
  }

  UnitLowerer* owner_;
  hir::TypeId type_;
};

auto PartBodyBuilder::Body(
    std::optional<mir::TypeId> part_type, support::LibraryDecl sink,
    bool reports, const Fill& fill) -> mir::CallableCode {
  mir::CompilationUnit& unit = Unit();
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  CallableBindings bindings(unit, code);
  std::optional<Handed> part;
  if (part_type.has_value()) {
    part = Handed{
        .local = bindings.DeclareAnonymous(*part_type), .type = *part_type};
  }
  const auto handed = [&](mir::TypeId type) {
    return Handed{.local = bindings.DeclareAnonymous(type), .type = type};
  };
  const BodyParams params{
      .selection = handed(
          mir::LibraryTypeOf(unit.types, support::LibraryDecl::kSelection)),
      .depth = handed(unit.builtins.machine_int64),
      .sink = handed(mir::LibraryTypeOf(unit.types, sink))};
  if (part.has_value()) code.params.push_back(part->local);
  code.params.push_back(params.selection.local);
  code.params.push_back(params.depth.local);
  code.params.push_back(params.sink.local);
  // Nothing in the source is written for a reader's reach, so what fails in
  // it is reported where the reader's own call was made.
  DeclareCallerLocation(unit, reports, bindings, code, code.params);
  code.result_type = unit.builtins.void_type;
  WalkFrame frame;
  frame.bindings = &bindings;
  frame.current_block = &code.Body();
  fill(frame, part, params);
  return code;
}

void PartBodyBuilder::AtSelectionEnd(
    const WalkFrame& frame, const BodyParams& params,
    const std::function<void(const WalkFrame&)>& ends,
    const std::function<void(const WalkFrame&)>& goes_on) {
  const mir::CompilationUnit& unit = Unit();
  mir::Builder block = frame.Builder();
  mir::Block ended;
  ends(frame.WithBlock(&ended));
  mir::Block further;
  goes_on(frame.WithBlock(&further));
  block.AppendStmt(
      mir::IfStmt{
          .condition = mir::BuildMachineIntComparison(
              unit, block, mir::BinaryOp::kEquality, params.depth.ReadIn(block),
              CallOn(
                  block, support::BuiltinFn::kSelectionDepth,
                  params.selection.PlaceIn(block), {},
                  unit.builtins.machine_int64)),
          .then_scope = block.AddScope(std::move(ended)),
          .else_scope = block.AddScope(std::move(further))});
}

auto PartBodyBuilder::IndexOf(
    mir::Builder block, const BodyParams& params, const hir::Type& array)
    -> mir::ExprId {
  mir::CompilationUnit& unit = Unit();
  const auto number = [&](mir::TypeId as) {
    return block.Add(
        mir::MakeBuiltinCall(
            unit, block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt,
            {CallOn(
                block, support::BuiltinFn::kSelectionNumber,
                params.selection.PlaceIn(block), {params.depth.ReadIn(block)},
                unit.builtins.machine_int64)},
            as));
  };
  const auto* associative = array.As<hir::AssociativeArrayType>();
  if (associative == nullptr) {
    return number(owner_->TranslateType(owner_->Hir().builtins.int_type));
  }
  if (owner_->Hir()
          .types.Get(associative->key_type)
          .Is<hir::WildcardIndexType>()) {
    return number(
        mir::PackedVectorOf(unit.types, 64, mir::IntegralStateKind::kTwoState));
  }
  const mir::TypeId key_type = owner_->TranslateType(associative->key_type);
  return block.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kSelectionKey,
              .receiver = params.selection.PlaceIn(block),
              .type_arguments = {key_type}},
          {params.depth.ReadIn(block)}, key_type));
}

void PartBodyBuilder::EachStep(
    const WalkFrame& frame, const BodyParams& params,
    const std::function<
        void(const WalkFrame&, const DescentStep&, hir::TypeId)>& take) {
  mir::CompilationUnit& unit = Unit();
  mir::Builder block = frame.Builder();
  const hir::Type& declared = Declared();
  if (declared.Is<hir::UnpackedStructType>() ||
      declared.Is<hir::UnpackedUnionType>()) {
    const std::vector<hir::TypeId> members = MemberTypesOf(*owner_, declared);
    mir::SwitchStmt chosen{
        .discriminant = CallOn(
            block, support::BuiltinFn::kSelectionNumber,
            params.selection.PlaceIn(block), {params.depth.ReadIn(block)},
            unit.builtins.machine_int64),
        .cases = {},
        .default_body = std::nullopt};
    chosen.cases.reserve(members.size());
    for (std::size_t member = 0; member < members.size(); ++member) {
      mir::Block taken;
      const WalkFrame at = frame.WithBlock(&taken);
      take(
          at,
          MemberStep(
              *owner_, at.Builder(), declared,
              base::ComponentIndex{static_cast<std::uint32_t>(member)},
              owner_->TranslateType(members[member])),
          members[member]);
      chosen.cases.push_back(
          mir::SwitchCase{
              .values = {static_cast<std::int64_t>(member)},
              .body = block.AddScope(std::move(taken))});
    }
    block.AppendStmt(std::move(chosen));
    return;
  }
  const std::optional<hir::TypeId> element = declared.ContainerElementType();
  if (!element.has_value()) {
    return;
  }
  // Each body reads the step's position more than once, so it is settled
  // here, once.
  DescentStep step = ElementStep(
      *owner_, block, type_, IndexOf(block, params, declared),
      owner_->TranslateType(*element));
  ForEachOperand(step, [&](mir::ExprId& operand) {
    operand = EvaluatedOnce(frame, operand);
  });
  take(frame, step, *element);
}

void PartBodyBuilder::CallPartBody(
    mir::Builder block, mir::CallableId body, std::optional<mir::ExprId> part,
    const BodyParams& params) {
  mir::CompilationUnit& unit = Unit();
  std::vector<mir::ExprId> arguments;
  if (part.has_value()) arguments.push_back(*part);
  arguments.push_back(params.selection.ReadIn(block));
  arguments.push_back(
      mir::BuildMachineIntBinary(
          unit, block, mir::BinaryOp::kAdd, params.depth.ReadIn(block),
          mir::BuildMachineIntLiteral(unit, block, 1)));
  arguments.push_back(params.sink.ReadIn(block));
  Perform(
      block,
      block.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = mir::CallableTarget{.callable = body}},
              std::move(arguments), unit.builtins.void_type)));
}

// Settling takes the selection's steps by the types alone: a member of a
// structure and an element of a fixed-size array lie at fixed positions,
// counted as a takeover of the variable counts them (LRM 10.6.2), and so does
// a member of an untagged union, which is a view of the union's storage from
// its first position (LRM 7.3); a member of a tagged union and an element of
// an array whose size the run sets lie at none; and
// an entry of an associative array is named by the key its array keeps it by,
// converted from what foreign code wrote as the design converts an index into
// the index type (LRM 7.8.4).
auto PartBodyBuilder::Settle() -> mir::CallableCode {
  return Body(
      std::nullopt, support::LibraryDecl::kSelectionSettler, false,
      [&](const WalkFrame& frame, const std::optional<Handed>&,
          const BodyParams& params) {
        mir::CompilationUnit& unit = Unit();
        const hir::Type& declared = Declared();
        // A part a write lands no value on is no part a takeover holds.
        if (!TakesAWrittenValue(declared) &&
            !declared.ContainerElementType().has_value() &&
            !declared.Is<hir::UnpackedStructType>() &&
            !declared.Is<hir::UnpackedUnionType>()) {
          mir::Builder block = frame.Builder();
          Perform(
              block,
              CallOn(
                  block, support::BuiltinFn::kSettleUnfixed,
                  params.sink.PlaceIn(block), {}, unit.builtins.void_type));
          return;
        }
        if (HoldsReadValue(declared)) return;
        AtSelectionEnd(
            frame, params, [](const WalkFrame&) {},
            [&](const WalkFrame& further) {
              EachStep(
                  further, params,
                  [&](const WalkFrame& at, const DescentStep& step,
                      hir::TypeId part) {
                    mir::Builder block = at.Builder();
                    const mir::ExprId settler = params.sink.PlaceIn(block);
                    // An entry of an associative array is named by its key,
                    // which a wildcard index keeps none of (LRM 7.8.1).
                    if (const auto* entry =
                            std::get_if<StepToAssocElement>(&step.to);
                        entry != nullptr &&
                        !owner_->Hir()
                             .types
                             .Get(declared.As<hir::AssociativeArrayType>()
                                      ->key_type)
                             .Is<hir::WildcardIndexType>()) {
                      Perform(
                          block,
                          CallOn(
                              block, support::BuiltinFn::kSettleKey, settler,
                              {params.depth.ReadIn(block), entry->key},
                              unit.builtins.void_type));
                    }
                    const std::expected<StepPosition, UnnumberedPart> moved =
                        PositionOfStep(
                            unit, block, owner_->TranslateType(type_), step);
                    Perform(
                        block,
                        moved.has_value()
                            ? CallOn(
                                  block, support::BuiltinFn::kSettlePositions,
                                  settler, {moved->offset},
                                  unit.builtins.void_type)
                            : CallOn(
                                  block, support::BuiltinFn::kSettleUnfixed,
                                  settler, {}, unit.builtins.void_type));
                    CallPartBody(
                        block, PartBodiesOf(*owner_, part).settle, std::nullopt,
                        params);
                  });
            });
      });
}

// A read hands the value of a part holding one, and, where the selection ends
// at an array whose size the run sets, how many elements it holds now and,
// where the reader asks for them, the keys of an associative array's entries
// in the order it keeps them (LRM 37.17 detail 9, 7.9.4); a part holding no
// value a format reads hands nothing. A step is taken as the design's own read
// of the part takes it -- an element it does not hold reads as its type's
// default and creates nothing (LRM 7.4.6, 7.8.6), and a member of a tagged
// union is checked against the tag (LRM 7.3.2) -- and the part is handed on as
// a reference to where it lies.
auto PartBodyBuilder::Read() -> mir::CallableCode {
  mir::CompilationUnit& unit = Unit();
  const mir::TypeId value_type = owner_->TranslateType(type_);
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = value_type, .mutability = mir::Mutability::kReadOnly}});
  return Body(
      reference, support::LibraryDecl::kValueSerializer, true,
      [&](const WalkFrame& frame, const std::optional<Handed>& part,
          const BodyParams& params) {
        const hir::Type& declared = Declared();
        const auto value_at = [&](mir::Builder block) {
          return block.Add(mir::MakeDerefExpr(part->ReadIn(block), value_type));
        };
        if (HoldsReadValue(declared)) {
          mir::Builder block = frame.Builder();
          const mir::ExprId value = PathValueAsDeclared(
              unit, block, AccessPath{.owner = value_at(block), .descent = {}});
          AppendSerialized(unit, block, params.sink.PlaceIn(block), value);
          return;
        }
        const auto ends = [&](const WalkFrame& at) {
          if (!declared.Is<hir::DynamicArrayType>() &&
              !declared.Is<hir::QueueType>() &&
              !declared.Is<hir::AssociativeArrayType>()) {
            return;
          }
          mir::Builder reading = at.Builder();
          const WalkedArray array = WalkedArrayAt(
              unit, at, AccessPath{.owner = value_at(reading), .descent = {}},
              std::nullopt);
          const mir::ExprId wants_keys = CallOn(
              reading, support::BuiltinFn::kSerializeSize,
              params.sink.PlaceIn(reading),
              {BuildToInt64Call(
                  unit, reading, EntryCount(unit, reading, array))},
              unit.builtins.machine_bool);
          // A wildcard index's keys are listed by no operation of the
          // language (LRM 7.9), so its array hands its size alone.
          const auto* associative = declared.As<hir::AssociativeArrayType>();
          if (associative == nullptr || owner_->Hir()
                                            .types.Get(associative->key_type)
                                            .Is<hir::WildcardIndexType>()) {
            Perform(reading, wants_keys);
            return;
          }
          mir::Block listed;
          const WalkFrame listing = at.WithBlock(&listed);
          auto keys = BuildEntryLoopStmt(
              unit, listing, array, WalkOrder::kFromFirst, std::nullopt,
              [&](const WalkFrame& entry_at,
                  const VisitedEntry& entry) -> diag::Result<void> {
                mir::Builder visiting = entry_at.Builder();
                Perform(
                    visiting,
                    CallOn(
                        visiting, support::BuiltinFn::kSerializeKey,
                        params.sink.PlaceIn(visiting),
                        {visiting.ReadLocal(
                            entry.place.index, entry.place.index_type)},
                        unit.builtins.void_type));
                return {};
              });
          if (!keys) {
            throw InternalError(
                "hir_to_mir: listing the keys of an associative array failed "
                "-- please report this as a bug");
          }
          listing.Builder().AppendStmt(*std::move(keys));
          reading.AppendStmt(
              mir::IfStmt{
                  .condition = wants_keys,
                  .then_scope = reading.AddScope(std::move(listed)),
                  .else_scope = std::nullopt});
        };
        AtSelectionEnd(frame, params, ends, [&](const WalkFrame& further) {
          EachStep(
              further, params,
              [&](const WalkFrame& at, const DescentStep& step,
                  hir::TypeId part_type) {
                mir::Builder block = at.Builder();
                const AccessPath path{
                    .owner = value_at(block), .descent = {step}};
                const mir::TypeId part_value = owner_->TranslateType(part_type);
                // A member of a union is not among what may be passed by
                // reference (LRM 13.5.2), so no reference is formed over it;
                // its value is read, checked against the tag where there is
                // one, and the reference formed over that.
                const mir::ExprId lies_at =
                    IsUnionMemberStep(step)
                        ? block.LocalPlace(
                              DeclareLocal(at, PathValue(unit, block, path)),
                              part_value)
                        : PathAsPlace(unit, block, path);
                const mir::ExprId borrowed = block.Add(
                    mir::Expr{
                        .data = mir::BorrowExpr{.operand = lies_at},
                        .type = unit.types.Intern(
                            mir::Type{mir::ReferenceType{
                                .referent = part_value,
                                .mutability = mir::Mutability::kReadOnly}})});
                CallPartBody(
                    block, PartBodiesOf(*owner_, part_type).read, borrowed,
                    params);
              });
        });
      });
}

// A write lands the value handed on a part holding one, as the design's own
// write through a reference to it does, so what waits on the part sees the
// write as it would see the design's (LRM 38.34, 9.4.2); a step is taken as a
// reference to the part is taken -- an element it does not hold is made (LRM
// 7.8.7), a member of a tagged union is checked against the tag (LRM 7.3.2) --
// and the reference, belonging to the same variable, is handed on.
auto PartBodyBuilder::Write() -> mir::CallableCode {
  mir::CompilationUnit& unit = Unit();
  const mir::TypeId value_type = owner_->TranslateType(type_);
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{mir::VariableRefType{
          .pointee = value_type, .mutability = mir::Mutability::kMutable}});
  return Body(
      reference, support::LibraryDecl::kValueDeserializer, true,
      [&](const WalkFrame& frame, const std::optional<Handed>& part,
          const BodyParams& params) {
        const hir::Type& declared = Declared();
        if (HoldsReadValue(declared)) {
          mir::Builder block = frame.Builder();
          const std::optional<mir::ExprId> value = DeserializedValue(
              unit, frame, params.sink.PlaceIn(block), value_type);
          if (!value.has_value()) return;
          Perform(
              block,
              block.Add(BuildStoreExpr(
                  unit, block,
                  AccessPath{.owner = part->PlaceIn(block), .descent = {}},
                  *value)));
          return;
        }
        AtSelectionEnd(
            frame, params, [](const WalkFrame&) {},
            [&](const WalkFrame& further) {
              EachStep(
                  further, params,
                  [&](const WalkFrame& at, const DescentStep& step,
                      hir::TypeId part_type) {
                    mir::Builder block = at.Builder();
                    const AccessPath path{
                        .owner = part->PlaceIn(block), .descent = {step}};
                    const mir::CallableId deeper =
                        PartBodiesOf(*owner_, part_type).write;
                    if (!IsUnionMemberStep(step)) {
                      CallPartBody(
                          block, deeper, PathReference(unit, block, path),
                          params);
                      return;
                    }
                    // A member of a union is not among what may be passed
                    // by reference (LRM 13.5.2), so no reference is lent to
                    // it: its value, checked against the tag where there is
                    // one, is written in a value of its own and that value
                    // written to the member, as the design's own write of the
                    // member lands.
                    AppendWriteChecks(unit, block, path);
                    const mir::TypeId part_value =
                        owner_->TranslateType(part_type);
                    const mir::LocalId held =
                        DeclareLocal(at, PathValue(unit, block, path));
                    CallPartBody(
                        block, deeper,
                        PathReference(
                            unit, block,
                            AccessPath{
                                .owner = block.LocalPlace(held, part_value),
                                .descent = {}}),
                        params);
                    Perform(
                        block, block.Add(BuildStoreExpr(
                                   unit, block, path,
                                   block.ReadLocal(held, part_value))));
                  });
            });
      });
}

}  // namespace

auto PartBodiesOf(UnitLowerer& owner, hir::TypeId type) -> PartBodies {
  auto& known = owner.PartBodiesByType();
  if (const auto found = known.find(type); found != known.end()) {
    return found->second;
  }
  mir::CompilationUnit& unit = owner.Unit();
  const PartBodies ids{
      .settle = unit.TakeNamespaceCallable(),
      .read = unit.TakeNamespaceCallable(),
      .write = unit.TakeNamespaceCallable()};
  known.emplace(type, ids);
  PartBodyBuilder builder(owner, type);
  unit.StateCallable(ids.settle, builder.Settle());
  unit.StateCallable(ids.read, builder.Read());
  unit.StateCallable(ids.write, builder.Write());
  return ids;
}

auto TakesAWrittenValue(const UnitLowerer& owner, hir::TypeId type) -> bool {
  return TakesAWrittenValue(owner.Hir().types.Get(type));
}

namespace {

// A type reached again -- members of one type -- answers as `answered` holds
// it did the first time, so the question costs the type's edges, never its
// parts' number.
auto HasWrittenPart(
    const UnitLowerer& owner, hir::TypeId type,
    std::unordered_map<hir::TypeId, bool>& answered) -> bool {
  if (const auto found = answered.find(type); found != answered.end()) {
    return found->second;
  }
  const hir::Type& declared = owner.Hir().types.Get(type);
  bool answer = false;
  if (HoldsReadValue(declared)) {
    answer = TakesAWrittenValue(declared);
  } else if (
      declared.Is<hir::UnpackedStructType>() ||
      declared.Is<hir::UnpackedUnionType>()) {
    for (const hir::TypeId member : MemberTypesOf(owner, declared)) {
      if (HasWrittenPart(owner, member, answered)) {
        answer = true;
        break;
      }
    }
  } else {
    const std::optional<hir::TypeId> element = declared.ContainerElementType();
    answer = element.has_value() && HasWrittenPart(owner, *element, answered);
  }
  answered.emplace(type, answer);
  return answer;
}

}  // namespace

auto HasWrittenPart(const UnitLowerer& owner, hir::TypeId type) -> bool {
  std::unordered_map<hir::TypeId, bool> answered;
  return HasWrittenPart(owner, type, answered);
}

}  // namespace lyra::lowering::hir_to_mir
