#include "lyra/lowering/hir_to_mir/value_operations.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/concatenation.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/value_parts.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The body answering `answers` for a value of `type`, where its interface
// declares the operation as the type's own: the method of the structure or
// the union it is, or the operation of the fixed-size array, the tuple or the
// run-time-sized container it is. A type that has no such operation answers
// through the library.
auto OwnOperationTarget(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::DirectTarget> {
  const std::optional<mir::TypeInterface> interface =
      mir::TypeInterfaceOf(unit, type);
  if (!interface.has_value() || !mir::Declares(*interface, answers)) {
    return std::nullopt;
  }
  if (const mir::RecordId* record = mir::RecordOf(unit.types.Get(type))) {
    return mir::CallableTarget{
        .callable = mir::RecordMethodOf(unit, *record, answers)};
  }
  return mir::TypeOperationTarget{.type = type, .answers = answers};
}

// Whether a value holds an unknown bit, as a machine boolean.
auto BuildHasUnknown(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::UnknownItem::kHasUnknown, value, {},
      unit.builtins.machine_bool);
}

// How a body of the record `record` declares names another of its methods: by
// the method the record declares for the operation, every one of which is
// declared before any of their bodies is built.
auto MethodsOf(const mir::CompilationUnit& unit, mir::RecordId record)
    -> OperationBodies::OwnTarget {
  return [&unit, record](support::ValueOperation answers) -> mir::DirectTarget {
    return mir::CallableTarget{
        .callable = mir::RecordMethodOf(unit, record, answers)};
  };
}

// Builds the operations of one product, a structure (LRM 7.2) or a tuple: its
// components side by side, so every operation on the whole value is that
// operation on each component in turn, the first most significant in a
// stream. Another of its operations is named as `own` says.
class ProductBodies : public OperationBodies {
 public:
  ProductBodies(
      const mir::CompilationUnit& unit, mir::TypeId product,
      std::vector<mir::TypeId> components, OwnTarget own)
      : OperationBodies(unit, product, std::move(components), std::move(own)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto ToBitVec() -> mir::CallableCode;
  auto FromBitVec() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto WritePositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::DeclaredDefault;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

  // Every operation a product's interface declares, its default built as
  // `declared_default` builds it.
  template <typename Default>
  auto BuildWith(support::ValueOperation answers, Default declared_default)
      -> mir::CallableCode {
    return Dispatched(
        answers,
        Overloaded{
            [&](support::EqualityItem item) {
              return AnswerEquality(*this, item);
            },
            [&](support::ChangeTestItem item) {
              return AnswerChangeTest(*this, item);
            },
            // A product's components are each held where they lie, so the
            // value an unknown condition answers is built component by
            // component where it is asked.
            [&](support::UnknownItem item) -> mir::CallableCode {
              switch (item) {
                case support::UnknownItem::kHasUnknown:
                  return HasUnknown();
                case support::UnknownItem::kMergeConditional:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown unknown-bit item");
            },
            [&](support::StreamItem item) -> mir::CallableCode {
              switch (item) {
                case support::StreamItem::kBitstreamWidth:
                  return BitWidth();
                case support::StreamItem::kCountBits:
                  return CountBits();
                case support::StreamItem::kToBitstream:
                  return ToBitstream();
                case support::StreamItem::kFromBitstream:
                  return FromBitstream();
                case support::StreamItem::kToBitVec:
                  return ToBitVec();
                case support::StreamItem::kFromBitVec:
                  return FromBitVec();
              }
              throw InternalError("hir_to_mir: unknown stream item");
            },
            [&](support::PositionsItem item) {
              return AnswerPositions(*this, item);
            },
            [&](support::DefaultItem item) -> mir::CallableCode {
              switch (item) {
                case support::DefaultItem::kDeclaredDefault:
                  return declared_default();
              }
              throw InternalError("hir_to_mir: unknown default item");
            },
            [&](support::ElementAccessItem item) -> mir::CallableCode {
              NoElementAccess(item);
            },
            [&](support::ResizeItem item) -> mir::CallableCode {
              NoResize(item);
            },
            [&](support::BuiltinFn) -> mir::CallableCode { NoBodyBuilt(); }});
  }
  auto Build(support::ValueOperation answers) -> mir::CallableCode {
    return BuildWith(answers, [&] { return DeclaredDefault(); });
  }

 private:
  // The stream this product makes, which every body over one has.
  auto Stream() -> mir::StreamShape {
    return *mir::FixedStreamShapeOfParts(Unit(), Members());
  }

  // The `int` sum of what `term(i)` answers for each component `i`, added into
  // a local of `code` one statement per component.
  auto SumOverMembers(
      mir::CallableCode& code, mir::Builder block, const auto& term)
      -> mir::ExprId {
    const mir::TypeId count = Unit().builtins.int_type;
    std::vector<mir::ExprId> terms;
    terms.reserve(Members().size());
    for (std::size_t i = 0; i < Members().size(); ++i) {
      terms.push_back(term(i));
    }
    const mir::ExprId zero = mir::BuildIntLiteral(Unit(), block, 0);
    const mir::LocalId sum = code.AddLocal(count);
    return AccumulateMirBinary(
        Unit(), block, mir::BinaryOp::kAdd, zero, sum, count, terms);
  }

  // The product built from one value per component.
  auto Built(mir::Builder block, std::vector<mir::ExprId> members)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = std::move(members)},
            .type = WholeType()});
  }
};

// The methods of one struct, named by the declaration being made, and what a
// variable of it starts as, which its declaration gives.
class StructBodies : public ProductBodies {
 public:
  StructBodies(
      UnitLowerer& lowerer, const hir::UnpackedStructDefinition& source,
      mir::RecordId record, mir::TypeId structure,
      std::vector<mir::TypeId> members)
      : ProductBodies(
            lowerer.Unit(), structure, std::move(members),
            MethodsOf(lowerer.Unit(), record)),
        lowerer_(&lowerer),
        source_(&source) {
  }

  auto DeclaredDefault() -> mir::CallableCode;
  auto Build(support::ValueOperation answers) -> mir::CallableCode {
    return BuildWith(answers, [&] { return DeclaredDefault(); });
  }

 private:
  const UnitLowerer* lowerer_;
  const hir::UnpackedStructDefinition* source_;
};

// LRM 11.4.5: equal when every pair of members is, and unknown where no pair is
// unequal and some pair is unknown -- which is the logical AND of the members'
// own answers, each taken to the product's own answer type first.
auto ProductBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::EqualityItem::kEqual,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          equal.push_back(PartEqual(block, lhs, rhs, Members()[i]));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(answer_type), answer_type, equal);
      });
}

auto ProductBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::EqualityItem::kCaseEqual,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          equal.push_back(BuildCaseEquality(Unit(), block, lhs, rhs));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(bit), bit, equal);
      });
}

// Identical where every pair of members is, asked in order and answered at the
// first pair that is not: one statement per member, one after another.
auto ProductBodies::BitIdentical() -> mir::CallableCode {
  return Body(
      support::ChangeTestItem::kBitIdentical,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          ReturnWhere(
              block,
              BuildConditionNot(
                  Unit(), block, BuildBitIdentity(Unit(), block, lhs, rhs)),
              [&](mir::Builder taken) {
                return BuildMachineBool(Unit(), taken, false);
              });
        }
        return BuildMachineBool(Unit(), block, true);
      });
}

// Holding an x or z where any member does, asked in order and answered at the
// first member that holds one.
auto ProductBodies::HasUnknown() -> mir::CallableCode {
  return Body(
      support::UnknownItem::kHasUnknown,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block, BuildHasUnknown(Unit(), block, Member(block, p[0], i)),
              [&](mir::Builder taken) {
                return BuildMachineBool(Unit(), taken, true);
              });
        }
        return BuildMachineBool(Unit(), block, false);
      });
}

// LRM 20.6.2: a structure, as any product, holds the bits its members hold.
auto ProductBodies::BitWidth() -> mir::CallableCode {
  return BodyOf(
      support::StreamItem::kBitstreamWidth,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          return BuildBitWidth(Unit(), block, Member(block, p[0], i));
        });
      });
}

// LRM 20.9: a product's stream is its members' laid end to end, so the count
// over it is the sum of theirs under the same control set.
auto ProductBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = mir::BitCountControlType(Unit().types);
  return BodyOf(
      support::StreamItem::kCountBits,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          const mir::ExprId member = Member(block, p[0], i);
          const mir::ExprId control_set = Referent(block, p[1], control);
          return BuildBitCount(Unit(), block, member, control_set);
        });
      });
}

// LRM 6.24.3: the first member's stream most significant, each after it
// concatenated below. A stream is only built where the type fixes its width, so
// no member can refuse.
auto ProductBodies::ToBitstream() -> mir::CallableCode {
  return BodyOf(
      support::StreamItem::kToBitstream,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "ProductBodies::ToBitstream: a component of a product whose "
                "stream is fixed has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildBitsJoin(
            Unit(), block,
            code.AddLocal(JoinedBitsType(Unit(), block.Body(), streams)),
            streams);
      });
}

// The inverse: each member reads its own width off the stream, the first from
// the most significant end (LRM 11.4.14.3). The type fixes every member's
// width, so where each one starts is known here.
auto ProductBodies::FromBitstream() -> mir::CallableCode {
  const mir::StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Body(
      support::StreamItem::kFromBitstream,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t taken = 0;
        for (const mir::TypeId member : Members()) {
          const std::uint64_t width =
              mir::FixedStreamShapeOf(Unit(), member)->width;
          taken += width;
          const mir::ExprId bits = block.Add(BuildPackedBitsRead(
              Unit(), block, Referent(block, p[0], stream), shape.width - taken,
              mir::PackedVectorOf(Unit().types, width, shape.state_kind)));
          members.push_back(BuildFromBitstream(Unit(), block, bits, member));
        }
        return Built(block, std::move(members));
      });
}

// LRM 6.24.3: where a member's width is the running program's, the members'
// streams laid end to end into a run of bits, the first most significant.
auto ProductBodies::ToBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::StreamItem::kToBitVec,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "ProductBodies::ToBitVec: a component of a product holding a "
                "stream has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildRunJoin(Unit(), block, code.AddLocal(run), streams);
      });
}

// The inverse, out of a run holding exactly the product's bits: a member
// whose type fixes its width takes that many, and one whose width is the
// program's takes what the members after it leave where they are empty, so
// the first such member takes the rest (LRM 11.4.14.4).
auto ProductBodies::FromBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::StreamItem::kFromBitVec,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId length = DeclaredCount(
            code, block,
            BuildRunLength(Unit(), block, Referent(block, p[0], run)));
        const mir::LocalId at =
            DeclaredCount(code, block, CountLiteral(block, 0));
        std::uint64_t after = 0;
        for (const mir::TypeId member : Members()) {
          after += StreamFillOf(Unit(), member).empty_width;
        }
        std::vector<mir::ExprId> members;
        for (const mir::TypeId member : Members()) {
          after -= StreamFillOf(Unit(), member).empty_width;
          if (const std::optional<mir::StreamShape> fixed =
                  mir::FixedStreamShapeOf(Unit(), member)) {
            const mir::ExprId source = Referent(block, p[0], run);
            const mir::ExprId start = CountNow(block, at);
            members.push_back(Held(
                code, block, member,
                BuildFromBitstream(
                    Unit(), block,
                    BuildRunLoad(Unit(), block, source, start, *fixed),
                    member)));
            const mir::ExprId lhs = CountNow(block, at);
            const mir::ExprId rhs = CountLiteral(block, fixed->width);
            SetCount(
                block, at,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd, lhs, rhs));
            continue;
          }
          const mir::LocalId allotted = PartAllotment(
              code, block, length, at, CountLiteral(block, after));
          const mir::ExprId source = Referent(block, p[0], run);
          const mir::ExprId start = CountNow(block, at);
          const mir::ExprId width = CountNow(block, allotted);
          members.push_back(Held(
              code, block, member,
              BuildFromBitVec(
                  Unit(), block,
                  BuildRunExtract(Unit(), block, source, start, width),
                  member)));
          const mir::ExprId lhs = CountNow(block, at);
          const mir::ExprId rhs = CountNow(block, allotted);
          SetCount(
              block, at,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kAdd, lhs, rhs));
        }
        return Built(block, std::move(members));
      });
}

// What a variable of the struct starts as: each member at what the struct's
// declaration gives it (LRM 7.2.2), the rest at their type's default.
auto StructBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::DefaultItem::kDeclaredDefault,
      [&](mir::Builder block, const std::vector<mir::LocalId>&) {
        return block.Add(
            BuildDeclaredStructValue(*lowerer_, block, *source_, WholeType()));
      });
}

// A product's positions are its members', the first member's first.
auto ProductBodies::TakePositions() -> mir::CallableCode {
  return BodyOf(
      support::PositionsItem::kTakePositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        std::vector<mir::ExprId> members;
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId into = Member(block, p[0], i);
          const mir::ExprId from = Member(block, p[1], i);
          const mir::ExprId start = CountLiteral(block, at);
          members.push_back(
              PartTaken(code, block, Members()[i], into, from, start, run));
          at += mir::PositionCountOf(Unit(), Members()[i]);
        }
        return Built(block, std::move(members));
      });
}

// The product whose components' positions an integral value of its positions
// holds, each read from where the components before it end.
auto ProductBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::PositionsItem::kFromPositions,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t at = 0;
        for (const mir::TypeId member : Members()) {
          const mir::ExprId bits = block.Add(BuildPackedBitsRead(
              Unit(), block, Referent(block, p[0], positions_type), at,
              mir::PositionsTypeOf(Unit(), member)));
          members.push_back(
              block.Add(BuildFromPositions(Unit(), block, bits, member)));
          at += mir::PositionCountOf(Unit(), member);
        }
        return Built(block, std::move(members));
      });
}

// A product's positions are its components', the first at position 0, each
// laid in where the components before it end.
auto ProductBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  return BodyOf(
      support::PositionsItem::kToPositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId positions = Declared(
            code, block, positions_type,
            block.Add(BuildDefaultValueExpr(Unit(), block, positions_type)));
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          LayPositions(
              block, positions, positions_type,
              BuildConstantPosition(
                  Unit(), block, static_cast<std::int64_t>(at)),
              PartPositions(block, Members()[i], Member(block, p[0], i)),
              mir::PositionsTypeOf(Unit(), Members()[i]));
          at += mir::PositionCountOf(Unit(), Members()[i]);
        }
        return block.ReadLocal(positions, positions_type);
      });
}

// Each component the view reaches is written, and no other.
auto ProductBodies::WritePositions() -> mir::CallableCode {
  return BodyOf(
      support::PositionsItem::kWritePositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const PositionsWrite write = HoldPositionsWrite(code, block, p);
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const std::uint64_t count =
              mir::PositionCountOf(Unit(), Members()[i]);
          WhereReached(
              block, write, CountLiteral(block, at), count,
              [&](mir::Builder reached) {
                PartWritten(
                    code, reached, Members()[i],
                    AccessPath{
                        .owner = Written(reached, write),
                        .descent = {DescentStep{
                            .to =
                                StepToComponent{
                                    .position =
                                        base::ComponentIndex{
                                            static_cast<std::uint32_t>(i)}},
                            .part_type = Members()[i]}}},
                    CountLiteral(reached, at), write);
              });
          at += count;
        }
        return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
      });
}

// Builds the operations of one untagged union (LRM 7.3). Its one component is
// its storage, an integral value every member is a view of from its first
// position, so what holds of the whole value holds of the storage: whether it
// changed, the positions a net resolves or a procedural continuous assignment
// takes. Every operation the language defines on the whole value is its first
// member's, read through the storage, as LRM 21.2.1.6 and 11.4.14.1 have
// printing and streaming read that member; one answering a union answers one
// whose storage holds that member's positions and nothing above them.
class UnionBodies : public OperationBodies {
 public:
  UnionBodies(
      const mir::CompilationUnit& unit, mir::RecordId record,
      mir::TypeId untagged, std::vector<mir::TypeId> members)
      : OperationBodies(
            unit, untagged, std::move(members), MethodsOf(unit, record)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto WritePositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

  auto Build(support::ValueOperation answers) -> mir::CallableCode {
    return Dispatched(
        answers,
        Overloaded{
            [&](support::EqualityItem item) {
              return AnswerEquality(*this, item);
            },
            [&](support::ChangeTestItem item) {
              return AnswerChangeTest(*this, item);
            },
            // A union is held whole, and the value an unknown condition
            // answers is built where it is asked.
            [&](support::UnknownItem item) -> mir::CallableCode {
              switch (item) {
                case support::UnknownItem::kHasUnknown:
                  return HasUnknown();
                case support::UnknownItem::kMergeConditional:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown unknown-bit item");
            },
            // A union streams as its first member does (LRM 11.4.14.1), whose
            // width is fixed, so its stream is never a run the program sizes.
            [&](support::StreamItem item) -> mir::CallableCode {
              switch (item) {
                case support::StreamItem::kBitstreamWidth:
                  return BitWidth();
                case support::StreamItem::kCountBits:
                  return CountBits();
                case support::StreamItem::kToBitstream:
                  return ToBitstream();
                case support::StreamItem::kFromBitstream:
                  return FromBitstream();
                case support::StreamItem::kToBitVec:
                case support::StreamItem::kFromBitVec:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown stream item");
            },
            [&](support::PositionsItem item) {
              return AnswerPositions(*this, item);
            },
            [&](support::DefaultItem item) {
              return AnswerDeclaredDefault(*this, item);
            },
            [&](support::ElementAccessItem item) -> mir::CallableCode {
              NoElementAccess(item);
            },
            [&](support::ResizeItem item) -> mir::CallableCode {
              NoResize(item);
            },
            [&](support::BuiltinFn) -> mir::CallableCode { NoBodyBuilt(); }});
  }

 private:
  [[nodiscard]] auto StorageType() const -> mir::TypeId {
    return *mir::UnionStorageOf(Unit(), WholeType());
  }
  // The storage of the union the parameter `local` refers to, as a value.
  auto Storage(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.AsValue(
        Unit().types, UnionStorage(Unit(), block, Whole(block, local)));
  }
  // The first member of the union the parameter `local` refers to, read
  // through the union's storage.
  auto First(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.Add(StepRead(
        Unit(), block,
        DescentStep{
            .to = StepToUnionMember{.position = base::ComponentIndex{0}},
            .part_type = FirstType()},
        Whole(block, local)));
  }
  [[nodiscard]] auto FirstType() const -> mir::TypeId {
    return Members().front();
  }
  // The union whose storage is `storage`.
  auto Of(mir::Builder block, mir::ExprId storage) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = {storage}},
            .type = WholeType()});
  }
  // The union holding `value` as its first member: the storage holding the
  // member's positions and zero above them, as a literal naming a member
  // leaves everything else zero (LRM 7.3).
  auto Holding(mir::Builder block, mir::ExprId value) -> mir::ExprId {
    const mir::TypeId storage = StorageType();
    const mir::TypeId bits = StorageBitsFor(Unit(), storage, FirstType());
    return Of(
        block, ConvertToType(
                   Unit(), block, BuildToPositions(Unit(), block, value, bits),
                   storage));
  }
  // The stream the first member makes, which every body over one has.
  auto Stream() -> mir::TypeId {
    const mir::StreamShape shape =
        *mir::FixedStreamShapeOf(Unit(), FirstType());
    return mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  }
};

auto UnionBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return Body(
      support::EqualityItem::kEqual,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId lhs = First(block, p[0]);
        const mir::ExprId rhs = First(block, p[1]);
        return block.Add(BuildMirBinaryExpr(
            Unit(), block, hir::BinaryOp::kEquality, lhs, rhs, answer_type));
      });
}

auto UnionBodies::CaseEqual() -> mir::CallableCode {
  return Body(
      support::EqualityItem::kCaseEqual,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId lhs = First(block, p[0]);
        const mir::ExprId rhs = First(block, p[1]);
        return BuildCaseEquality(Unit(), block, lhs, rhs);
      });
}

// Every member is a view of the storage, so a read through any of them sees a
// change exactly where the storage's bits changed.
auto UnionBodies::BitIdentical() -> mir::CallableCode {
  return Body(
      support::ChangeTestItem::kBitIdentical,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildBitIdentity(
            Unit(), block, Storage(block, p[0]), Storage(block, p[1]));
      });
}

auto UnionBodies::HasUnknown() -> mir::CallableCode {
  return Body(
      support::UnknownItem::kHasUnknown,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildHasUnknown(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::BitWidth() -> mir::CallableCode {
  return Body(
      support::StreamItem::kBitstreamWidth,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildBitWidth(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = mir::BitCountControlType(Unit().types);
  return Body(
      support::StreamItem::kCountBits,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId member = First(block, p[0]);
        const mir::ExprId control_set = Referent(block, p[1], control);
        return BuildBitCount(Unit(), block, member, control_set);
      });
}

auto UnionBodies::ToBitstream() -> mir::CallableCode {
  return Body(
      support::StreamItem::kToBitstream,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        auto stream = BuildToBitstream(Unit(), block, First(block, p[0]), {});
        if (!stream) {
          throw InternalError(
              "UnionBodies::ToBitstream: the first member of a union whose "
              "stream is fixed has none");
        }
        return *stream;
      });
}

auto UnionBodies::FromBitstream() -> mir::CallableCode {
  const mir::TypeId stream = Stream();
  return Body(
      support::StreamItem::kFromBitstream,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId bits = block.Add(BuildPackedBitsRead(
            Unit(), block, Referent(block, p[0], stream), 0, stream));
        return Holding(
            block, BuildFromBitstream(Unit(), block, bits, FirstType()));
      });
}

// A union nothing wrote holds its first member at that member's default (LRM
// Table 7-1).
auto UnionBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::DefaultItem::kDeclaredDefault,
      [&](mir::Builder block, const std::vector<mir::LocalId>&) {
        return Holding(
            block,
            block.Add(BuildDefaultValueExpr(Unit(), block, FirstType())));
      });
}

// A union's positions are its storage's, so a run of them is taken there.
auto UnionBodies::TakePositions() -> mir::CallableCode {
  return BodyOf(
      support::PositionsItem::kTakePositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        return Of(
            block, PartTaken(
                       code, block, StorageType(), Storage(block, p[0]),
                       Storage(block, p[1]), CountLiteral(block, 0), run));
      });
}

auto UnionBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::PositionsItem::kToPositions,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return ConvertToType(
            Unit(), block, Storage(block, p[0]), positions_type);
      });
}

auto UnionBodies::WritePositions() -> mir::CallableCode {
  return BodyOf(
      support::PositionsItem::kWritePositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const PositionsWrite write = HoldPositionsWrite(code, block, p);
        WhereReached(
            block, write, CountLiteral(block, 0),
            mir::PositionCountOf(Unit(), StorageType()),
            [&](mir::Builder reached) {
              PartWritten(
                  code, reached, StorageType(),
                  AccessPath{
                      .owner = Written(reached, write),
                      .descent = {DescentStep{
                          .to =
                              StepToComponent{
                                  .position = base::ComponentIndex{0}},
                          .part_type = StorageType()}}},
                  CountLiteral(reached, 0), write);
            });
        return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
      });
}

auto UnionBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::PositionsItem::kFromPositions,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return Of(
            block, ConvertToType(
                       Unit(), block,
                       block.AsValue(
                           Unit().types, Referent(block, p[0], positions_type)),
                       StorageType()));
      });
}

// Builds the operations of one tagged union (LRM 7.3.2, 11.9): each is the
// operation of the member the tag names, which two values compared have to
// name alike, and two values holding different members are unequal.
class TaggedUnionBodies : public OperationBodies {
 public:
  TaggedUnionBodies(
      const mir::CompilationUnit& unit, mir::RecordId record,
      mir::TypeId tagged, std::vector<mir::TypeId> members)
      : OperationBodies(
            unit, tagged, std::move(members), MethodsOf(unit, record)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::Inequality;

  auto Build(support::ValueOperation answers) -> mir::CallableCode {
    return Dispatched(
        answers,
        Overloaded{
            [&](support::EqualityItem item) {
              return AnswerEquality(*this, item);
            },
            [&](support::ChangeTestItem item) {
              return AnswerChangeTest(*this, item);
            },
            [&](support::UnknownItem item) -> mir::CallableCode {
              switch (item) {
                case support::UnknownItem::kHasUnknown:
                  return HasUnknown();
                case support::UnknownItem::kMergeConditional:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown unknown-bit item");
            },
            // A tagged union holds a tag beside its member, which no stream
            // carries (LRM 11.4.14.1).
            [&](support::StreamItem item) -> mir::CallableCode {
              switch (item) {
                case support::StreamItem::kBitstreamWidth:
                case support::StreamItem::kCountBits:
                case support::StreamItem::kToBitstream:
                case support::StreamItem::kFromBitstream:
                case support::StreamItem::kToBitVec:
                case support::StreamItem::kFromBitVec:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown stream item");
            },
            // No net type's tables read a tag, so no net writes a tagged
            // union's positions (LRM 6.7.1).
            [&](support::PositionsItem item) -> mir::CallableCode {
              switch (item) {
                case support::PositionsItem::kTakePositions:
                  return TakePositions();
                case support::PositionsItem::kPositionCount:
                  return PositionCount();
                case support::PositionsItem::kToPositions:
                  return ToPositions();
                case support::PositionsItem::kFromPositions:
                  return FromPositions();
                case support::PositionsItem::kWritePositions:
                  NoBodyBuilt();
              }
              throw InternalError("hir_to_mir: unknown positions item");
            },
            [&](support::DefaultItem item) {
              return AnswerDeclaredDefault(*this, item);
            },
            [&](support::ElementAccessItem item) -> mir::CallableCode {
              NoElementAccess(item);
            },
            [&](support::ResizeItem item) -> mir::CallableCode {
              NoResize(item);
            },
            [&](support::BuiltinFn) -> mir::CallableCode { NoBodyBuilt(); }});
  }

 private:
  // Where its tag lies among its positions: above its widest member's.
  [[nodiscard]] auto TagAt() const -> std::uint64_t {
    return mir::WidestPositionCountOf(Unit(), Members());
  }

  // What `held(taken, i)` answers where every one of the values `locals`
  // refers to holds member `i`, returned by the member they hold, and what
  // `neither(taken)` builds where they hold no one member alike. Each value's
  // tag is read once, and two tags are compared before any member is, as
  // rustc's derived comparison of an enum compares the discriminants first.
  // Each answer is built in the scope that returns it, so a member is read only
  // where it is held.
  template <typename Held, typename Neither>
  auto ByHeldMember(
      mir::CallableCode& code, mir::Builder block,
      std::span<const mir::LocalId> locals, Held held, Neither neither)
      -> mir::ExprId {
    const mir::TypeId tag_type = Unit().builtins.machine_int64;
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    std::vector<mir::LocalId> tags;
    tags.reserve(locals.size());
    for (const mir::LocalId local : locals) {
      const mir::LocalId tag = code.AddLocal(tag_type);
      block.AppendStmt(
          mir::LocalDeclStmt{
              .target = tag,
              .init = block.Add(
                  mir::MakeDiscriminantExpr(Whole(block, local), tag_type))});
      tags.push_back(tag);
    }
    for (const mir::LocalId other : std::span{tags}.subspan(1)) {
      const mir::ExprId lhs = block.ReadLocal(tags.front(), tag_type);
      const mir::ExprId rhs = block.ReadLocal(other, tag_type);
      ReturnWhere(
          block,
          block.Add(
              mir::MakeBinary(
                  Unit(), block.Body(), mir::BinaryOp::kInequality, lhs, rhs,
                  boolean)),
          neither);
    }
    for (std::size_t i = 0; i < Members().size(); ++i) {
      const mir::ExprId lhs = block.ReadLocal(tags.front(), tag_type);
      const mir::ExprId rhs = mir::BuildMachineIntLiteral(
          Unit(), block, static_cast<std::int64_t>(i));
      ReturnWhere(
          block,
          block.Add(
              mir::MakeBinary(
                  Unit(), block.Body(), mir::BinaryOp::kEquality, lhs, rhs,
                  boolean)),
          [&](mir::Builder taken) { return held(taken, i); });
    }
    return neither(block);
  }
};

// LRM 11.4.5: equal where both hold one member and that member's values are,
// unequal where they hold different members.
auto TaggedUnionBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::EqualityItem::kEqual,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return PartEqual(taken, lhs, rhs, Members()[i]);
            },
            [&](mir::Builder taken) {
              return KnownBit(taken, answer_type, false);
            });
      });
}

auto TaggedUnionBodies::CaseEqual() -> mir::CallableCode {
  return BodyOf(
      support::EqualityItem::kCaseEqual,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return BuildCaseEquality(Unit(), taken, lhs, rhs);
            },
            [&](mir::Builder taken) {
              return mir::BuildBit1Literal(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::BitIdentical() -> mir::CallableCode {
  return BodyOf(
      support::ChangeTestItem::kBitIdentical,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return BuildBitIdentity(Unit(), taken, lhs, rhs);
            },
            [&](mir::Builder taken) {
              return BuildMachineBool(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::HasUnknown() -> mir::CallableCode {
  return BodyOf(
      support::UnknownItem::kHasUnknown,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, std::span{p}.first(1),
            [&](mir::Builder taken, std::size_t i) {
              return BuildHasUnknown(Unit(), taken, Member(taken, p[0], i));
            },
            [&](mir::Builder taken) {
              return BuildMachineBool(Unit(), taken, false);
            });
      });
}

// A tagged union holding its first member at that member's default: LRM 11.9
// leaves an uninitialized tagged union undefined, and its first member is
// Lyra's answer, as Table 7-1's is for an untagged one.
auto TaggedUnionBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::DefaultItem::kDeclaredDefault,
      [&](mir::Builder block, const std::vector<mir::LocalId>&) {
        constexpr base::ComponentIndex kFirstMember{0};
        return block.Add(
            mir::MakeTaggedUnionExpr(
                block.Add(
                    BuildDefaultValueExpr(Unit(), block, Members().front())),
                kFirstMember, WholeType()));
      });
}

// Taken whole, since its tag decides which member lies there (LRM 7.3.2): a
// run naming any of its positions takes all of it.
auto TaggedUnionBodies::TakePositions() -> mir::CallableCode {
  return Body(
      support::PositionsItem::kTakePositions,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return block.AsValue(Unit().types, Whole(block, p[1]));
      });
}

// A tagged union's positions are the member it holds from the first and its
// tag above its widest member's, as a packed tagged union lays them out (LRM
// 7.3.2), which an untagged union holding one is a view of (LRM 7.3). A tag
// naming no member is bits another member of such a union wrote, and holds no
// positions of a member.
auto TaggedUnionBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  const std::uint64_t tag_positions = mir::TagPositionCountOf(Members().size());
  return BodyOf(
      support::PositionsItem::kToPositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const auto none = [&](mir::Builder at) {
          return at.Add(BuildDefaultValueExpr(Unit(), at, positions_type));
        };
        return ByHeldMember(
            code, block, std::span{p}.first(1),
            [&](mir::Builder taken, std::size_t i) {
              const mir::LocalId positions =
                  Declared(code, taken, positions_type, none(taken));
              const mir::TypeId member = Members()[i];
              if (mir::PositionCountOf(Unit(), member) != 0) {
                LayPositions(
                    taken, positions, positions_type,
                    BuildConstantPosition(Unit(), taken, 0),
                    PartPositions(taken, member, Member(taken, p[0], i)),
                    mir::PositionsTypeOf(Unit(), member));
              }
              if (tag_positions != 0) {
                const mir::TypeId tag = mir::PackedVectorOf(
                    Unit().types, tag_positions,
                    mir::IntegralStateKind::kFourState);
                LayPositions(
                    taken, positions, positions_type,
                    BuildConstantPosition(
                        Unit(), taken, static_cast<std::int64_t>(TagAt())),
                    taken.Add(
                        mir::MakeBuiltinCall(
                            Unit(), taken.Body(),
                            support::BuiltinFn::kIntegralFromInt, std::nullopt,
                            {mir::BuildMachineIntLiteral(
                                Unit(), taken, static_cast<std::int64_t>(i))},
                            tag)),
                    tag);
              }
              return taken.ReadLocal(positions, positions_type);
            },
            none);
      });
}

// The tagged union an integral value of its positions holds: the member its
// tag names, made from the positions below the tag; one naming no member, by
// its value or by an unknown bit, holds the first member at its default, which
// is Lyra's answer for bits another member of an untagged union holding it
// wrote (LRM 7.3.2).
auto TaggedUnionBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = mir::PositionsTypeOf(Unit(), WholeType());
  const std::uint64_t tag_positions = mir::TagPositionCountOf(Members().size());
  return BodyOf(
      support::PositionsItem::kFromPositions,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::TypeId number = Unit().builtins.machine_int64;
        // One member has no tag bits, and names that member.
        mir::ExprId tag_read = mir::BuildMachineIntLiteral(Unit(), block, 0);
        if (tag_positions != 0) {
          const mir::TypeId tag_bits = mir::PackedVectorOf(
              Unit().types, tag_positions, mir::IntegralStateKind::kFourState);
          const auto tag_bits_read = [&] {
            return block.Add(BuildPackedBitsRead(
                Unit(), block, Referent(block, p[0], positions_type), TagAt(),
                tag_bits));
          };
          // A tag with an unknown bit names no member either.
          ReturnWhere(
              block, BuildHasUnknown(Unit(), block, tag_bits_read()),
              [&](mir::Builder taken) {
                return taken.Add(
                    BuildDefaultValueExpr(Unit(), taken, WholeType()));
              });
          tag_read = BuildToInt64Call(Unit(), block, tag_bits_read());
        }
        const mir::LocalId tag = Declared(code, block, number, tag_read);
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block,
              block.Add(
                  mir::MakeBinary(
                      Unit(), block.Body(), mir::BinaryOp::kEquality,
                      block.ReadLocal(tag, number),
                      mir::BuildMachineIntLiteral(
                          Unit(), block, static_cast<std::int64_t>(i)),
                      Unit().builtins.machine_bool)),
              [&](mir::Builder taken) {
                const mir::TypeId member = Members()[i];
                const mir::ExprId value =
                    mir::PositionCountOf(Unit(), member) == 0
                        ? taken.Add(
                              BuildDefaultValueExpr(Unit(), taken, member))
                        : taken.Add(BuildFromPositions(
                              Unit(), taken,
                              taken.Add(BuildPackedBitsRead(
                                  Unit(), taken,
                                  Referent(taken, p[0], positions_type), 0,
                                  mir::PositionsTypeOf(Unit(), member))),
                              member));
                return taken.Add(
                    mir::MakeTaggedUnionExpr(
                        value,
                        base::ComponentIndex{static_cast<std::uint32_t>(i)},
                        WholeType()));
              });
        }
        return block.Add(BuildDefaultValueExpr(Unit(), block, WholeType()));
      });
}

// The methods a record's interface declares, each built by `bodies`.
template <typename Bodies>
auto MethodsBuilt(
    const mir::CompilationUnit& unit, mir::TypeId type, Bodies& bodies)
    -> std::vector<RecordMethodBody> {
  const mir::TypeInterface interface = mir::TypeInterfaceOf(unit, type).value();
  std::vector<RecordMethodBody> methods;
  for (const support::ValueOperation answers : interface.operations) {
    methods.push_back(
        RecordMethodBody{.answers = answers, .code = bodies.Build(answers)});
  }
  return methods;
}

}  // namespace

auto ReferenceTo(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = type, .mutability = mir::Mutability::kReadOnly}});
}

auto BuildBitIdentity(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::ChangeTestItem::kBitIdentical, lhs, {rhs},
      unit.builtins.machine_bool);
}

auto RecordMethodBodies(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type) -> std::vector<RecordMethodBody> {
  const mir::CompilationUnit& unit = lowerer.Unit();
  std::vector<mir::TypeId> members = mir::PartTypes(unit, type);
  switch (mir::RecordKindOf(unit, record)) {
    case mir::RecordKind::kStruct: {
      if (structure == nullptr) {
        throw InternalError(
            "hir_to_mir: a struct's methods are asked for without its "
            "declaration -- please report this as a bug");
      }
      StructBodies bodies(
          lowerer, *structure, record, type, std::move(members));
      return MethodsBuilt(unit, type, bodies);
    }
    case mir::RecordKind::kUnion: {
      UnionBodies bodies(unit, record, type, std::move(members));
      return MethodsBuilt(unit, type, bodies);
    }
    case mir::RecordKind::kTaggedUnion: {
      TaggedUnionBodies bodies(unit, record, type, std::move(members));
      return MethodsBuilt(unit, type, bodies);
    }
  }
  throw InternalError("hir_to_mir: unknown record kind");
}

auto BuildTupleOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode {
  const mir::TypeId type = source.type;
  ProductBodies bodies(
      unit, type, unit.types.Get(type).Get<mir::TupleType>().elements,
      [type](support::ValueOperation answers) -> mir::DirectTarget {
        return mir::TypeOperationTarget{.type = type, .answers = answers};
      });
  return bodies.Build(source.answers);
}

auto BuildToPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::TypeId positions_type) -> mir::ExprId {
  const mir::TypeId type = block.Get(value).type;
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return ConvertToType(
        unit, block, block.AsValue(unit.types, value), positions_type);
  }
  if (t.Is<mir::RealType>()) {
    const mir::ExprId pattern = block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kToBits,
                            .receiver = block.AsValue(unit.types, value)},
                    .arguments = {}},
            .type = unit.builtins.machine_int64});
    return block.Add(
        mir::MakeBuiltinCall(
            unit, block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt, {pattern}, positions_type));
  }
  return ConvertToType(
      unit, block,
      BuildValueOperation(
          unit, block, support::PositionsItem::kToPositions, value, {},
          mir::PositionsTypeOf(unit, type)),
      positions_type);
}

auto BuildFromPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId positions,
    mir::TypeId type) -> mir::Expr {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return BuildValueConversion(unit, block, positions, type);
  }
  if (t.Is<mir::RealType>()) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromBits,
                        .type_arguments = {type}},
                .arguments = {BuildToInt64Call(unit, block, positions)}},
        .type = type};
  }
  // A type's own operation, asked of the type rather than of a value, which
  // takes the positions by reference.
  return mir::MakeCallExpr(
      mir::Direct{
          .target = *OwnOperationTarget(
              unit, type, support::PositionsItem::kFromPositions)},
      {mir::Borrow(
          unit, block,
          ConvertToType(
              unit, block, positions, mir::PositionsTypeOf(unit, type)))},
      type);
}

auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::LibraryAnsweredItem item, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  const mir::TypeId deciding =
      receiver.has_value() ? block.Get(*receiver).type : result;
  if (std::optional<mir::DirectTarget> own =
          OwnOperationTarget(unit, deciding, item)) {
    return OwnOperationCall(
        unit, block, deciding, *std::move(own), item, receiver,
        std::move(operands), result);
  }
  return BuildLibraryCall(
      unit, block, support::LibraryEntryOf(item), receiver, std::move(operands),
      result);
}

auto BuildLibraryCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  for (mir::ExprId& operand : operands) {
    operand = block.AsValue(unit.types, operand);
  }
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), entry, receiver, std::move(operands), result));
}

auto BuildEquality(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::EqualityItem item, mir::ExprId lhs, mir::ExprId rhs,
    mir::TypeId result) -> mir::ExprId {
  const mir::TypeId type = block.Get(lhs).type;
  if (std::optional<mir::DirectTarget> own =
          OwnOperationTarget(unit, type, item)) {
    const mir::TypeId answer = [&] {
      switch (item) {
        case support::EqualityItem::kEqual:
        case support::EqualityItem::kNotEqual:
          return mir::EqualityAnswerType(unit, type);
        case support::EqualityItem::kCaseEqual:
          return unit.builtins.bit1;
      }
      throw InternalError("BuildEquality: unknown equality item");
    }();
    return ConvertToType(
        unit, block,
        OwnOperationCall(
            unit, block, type, *std::move(own), item, lhs, {rhs}, answer),
        result);
  }
  // A type whose operations the compiler builds none of answers through what
  // the language gives every value: the operators, and the library's case
  // equality.
  const mir::ExprId rhs_value = block.AsValue(unit.types, rhs);
  const auto operator_call = [&](mir::BinaryOp op) {
    return block.Add(
        mir::MakeBinary(
            unit, block.Body(), op, block.AsValue(unit.types, lhs), rhs_value,
            result));
  };
  switch (item) {
    case support::EqualityItem::kEqual:
      return operator_call(mir::BinaryOp::kEquality);
    case support::EqualityItem::kNotEqual:
      return operator_call(mir::BinaryOp::kInequality);
    case support::EqualityItem::kCaseEqual:
      return block.Add(
          mir::MakeBuiltinCall(
              unit, block.Body(), support::BuiltinFn::kCaseEqual, lhs,
              {rhs_value}, result));
  }
  throw InternalError("BuildEquality: unknown equality item");
}

auto BuildCaseEquality(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildEquality(
      unit, block, support::EqualityItem::kCaseEqual, lhs, rhs,
      unit.builtins.bit1);
}

auto BuildCaseComparison(
    const mir::CompilationUnit& unit, mir::Builder block,
    hir::CaseKeyword keyword, mir::ExprId value, mir::ExprId label)
    -> mir::ExprId {
  const auto wildcard = [&](support::BuiltinFn entry) {
    return BuildLibraryCall(
        unit, block, entry, value, {label}, unit.builtins.bit1);
  };
  switch (keyword) {
    case hir::CaseKeyword::kCase:
      return BuildCaseEquality(unit, block, value, label);
    case hir::CaseKeyword::kCasez:
      return wildcard(support::BuiltinFn::kCasezEquals);
    case hir::CaseKeyword::kCasex:
      return wildcard(support::BuiltinFn::kCasexEquals);
  }
  throw InternalError("BuildCaseComparison: unknown case keyword");
}

auto BuildUnknownTest(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kFromBool, std::nullopt,
          {BuildHasUnknown(unit, block, value)}, unit.builtins.bit1));
}

auto BuildBitCount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::ExprId control) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::StreamItem::kCountBits, value, {control},
      unit.builtins.int_type);
}

auto BuildBitWidth(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  // An integral value is as many bits as its type is wide.
  const mir::Type& type = unit.types.Get(block.Get(value).type);
  if (type.IsIntegral()) {
    return mir::BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(type.Integral().bit_width));
  }
  return BuildValueOperation(
      unit, block, support::StreamItem::kBitstreamWidth, value, {},
      unit.builtins.int_type);
}

}  // namespace lyra::lowering::hir_to_mir
