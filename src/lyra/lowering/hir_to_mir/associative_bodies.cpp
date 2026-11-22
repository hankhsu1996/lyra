#include "lyra/lowering/hir_to_mir/associative_bodies.hpp"

#include <array>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/container_bodies.hpp"
#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/array_impls.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/value_parts.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/language_trait.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"
#include "lyra/value/container_core.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The kind of index the core of an associative array indexed by `index`
// orders its entries by (LRM 7.8.1 -- 7.8.5): an integral value, an enumeration
// included, by its number; a string; a class handle and a `chandle` by what
// they name; a wildcard index by the number it names; and any other type by
// nothing the core knows.
auto IndexKindOf(const mir::CompilationUnit& unit, mir::TypeId index)
    -> value::IndexKind {
  const mir::Type& type = unit.types.Get(index);
  if (type.IsIntegral()) {
    return value::IndexKind::kIntegral;
  }
  if (type.IsDeclaredBy(support::LibraryDecl::kString)) {
    return value::IndexKind::kString;
  }
  if (type.IsDeclaredBy(support::LibraryDecl::kWildcardIndex)) {
    return value::IndexKind::kWildcard;
  }
  if (type.Is<mir::ManagedRefType>()) {
    return value::IndexKind::kClassHandle;
  }
  if (type.Is<mir::UnmanagedRefType>()) {
    return value::IndexKind::kChandle;
  }
  return value::IndexKind::kOther;
}

// The bodies built over the entries of one associative array type (LRM 7.8):
// each a program over the element type's and the index type's own operations
// and the library's core, which keeps the entries' storage and, for an index
// whose order the language fixes, their order. Where an index names an entry
// is found by the core for such an index, and by these bodies, comparing each
// entry's index by the index type's equality, for any other (LRM 7.8.5).
class AssociativeBodies : public OperationBodies {
  friend class OperationBodies;

 public:
  AssociativeBodies(
      const mir::CompilationUnit& unit, mir::TypeId array, mir::TypeId element,
      mir::TypeId index)
      : OperationBodies(
            unit, array, {element},
            [array](support::ValueOperation answers) -> mir::DirectTarget {
              return mir::TypeOperationTarget{
                  .type = array, .answers = answers};
            }),
        element_(element),
        index_(index),
        kind_(IndexKindOf(unit, index)) {
  }

  // Every operation an associative array of this type answers with a body,
  // each its type's interface declares.
  auto Build(const mir::TypeOperationSource& source) -> mir::CallableCode {
    return std::visit(
        Overloaded{
            [&](support::ValueStep step) -> mir::CallableCode {
              return Step(step);
            },
            [](support::IntegralOp) -> mir::CallableCode { NoBodyBuilt(); },
            [&](support::EqualityItem item) {
              return AnswerEquality(*this, item);
            },
            [&](support::LibraryAnsweredItem trait_item) {
              return std::visit(
                  Overloaded{
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
                        throw InternalError(
                            "hir_to_mir: unknown unknown-bit item");
                      },
                      [&](support::StreamItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::StreamItem::kBitstreamWidth:
                            return BitWidth();
                          case support::StreamItem::kCountBits:
                            return CountBits();
                          case support::StreamItem::kToBitVec:
                            return ToBitVec();
                          case support::StreamItem::kToBitstream:
                          case support::StreamItem::kFromBitstream:
                          case support::StreamItem::kFromBitVec:
                            NoBodyBuilt();
                        }
                        throw InternalError("hir_to_mir: unknown stream item");
                      },
                      [&](support::PositionsItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::PositionsItem::kTakePositions:
                            return TakePositions();
                          case support::PositionsItem::kPositionCount:
                          case support::PositionsItem::kToPositions:
                          case support::PositionsItem::kFromPositions:
                          case support::PositionsItem::kWritePositions:
                            NoBodyBuilt();
                        }
                        throw InternalError(
                            "hir_to_mir: unknown positions item");
                      },
                      [&](support::DefaultItem item) -> mir::CallableCode {
                        switch (item) {
                          case support::DefaultItem::kDeclaredDefault:
                            return Default();
                        }
                        throw InternalError("hir_to_mir: unknown default item");
                      },
                      [&](support::ElementAccessItem item)
                          -> mir::CallableCode {
                        switch (item) {
                          case support::ElementAccessItem::kElementPlace:
                            return ElementPlace();
                          case support::ElementAccessItem::kGetUnchecked:
                          case support::ElementAccessItem::kGetUncheckedMut:
                            NoBodyBuilt();
                        }
                        throw InternalError(
                            "hir_to_mir: unknown element-access item");
                      },
                      [](support::ResizeItem item) -> mir::CallableCode {
                        NoResize(item);
                      }},
                  trait_item);
            },
            [](support::BoundFreeItem) -> mir::CallableCode {
              NoBoundFreeBody();
            },
            [&](support::BuiltinFn fn) -> mir::CallableCode {
              return OperationNamed(fn);
            }},
        source.answers);
  }

  // One of the array's own methods or an item of a language trait it
  // implements, and how its body is built.
  struct OperationBody {
    support::BuiltinFn name;
    mir::CallableCode (*build)(AssociativeBodies& bodies);
  };
  // The array's own methods' rows, its language traits' rows, and the two
  // together, which name exactly the operations MIR declares the array is
  // called by.
  static const std::array<OperationBody, mir::kAssociativeArrayMethods.size()>
      kMethodBodies;
  static const std::array<
      OperationBody, support::kMemoryWordsRequirements.size()>
      kLanguageTraitBodies;
  static const std::array<
      OperationBody, mir::kAssociativeArrayOperations.size()>
      kOperationBodies;

 private:
  // The body of the operation `fn` the array is called by (LRM 7.9).
  auto OperationNamed(support::BuiltinFn fn) -> mir::CallableCode {
    for (const OperationBody& row : kOperationBodies) {
      if (row.name == fn) {
        return row.build(*this);
      }
    }
    NoBodyBuilt();
  }

  auto Step(support::ValueStep step) -> mir::CallableCode {
    switch (step) {
      case support::ValueStep::kCopy:
        return Copy();
      // A relocation is no operation of a type's: it moves a value's bytes
      // whatever the type.
      case support::ValueStep::kRelocate:
        throw InternalError(
            "hir_to_mir: an associative array is asked a relocation as an "
            "operation of its type -- please report this as a bug");
      case support::ValueStep::kAssign:
        return Assign();
      case support::ValueStep::kEnd:
        return End();
    }
    throw InternalError("hir_to_mir: unknown value step");
  }

  // The index the parameter `local` refers to, as the place it is.
  auto IndexAt(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Referent(block, local, index_);
  }

  // An address owning nothing, to read or to write, of storage holding no
  // known type, and where an element and an index lie.
  [[nodiscard]] auto Address(mir::Mutability mutability) const -> mir::TypeId {
    return Unit().types.Intern(
        mir::Type{mir::PointerType{
            .pointee = Unit().types.Intern(mir::Type{mir::VoidType{}}),
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mutability}});
  }
  [[nodiscard]] auto ElementPointer(mir::Mutability mutability) const
      -> mir::TypeId {
    return mir::BorrowedPointerTo(Unit().types, element_, mutability);
  }
  [[nodiscard]] auto IndexPointer(mir::Mutability mutability) const
      -> mir::TypeId {
    return mir::BorrowedPointerTo(Unit().types, index_, mutability);
  }

  // Whether the address `address` is one.
  auto IsSome(mir::Builder block, mir::ExprId address) -> mir::ExprId {
    return BuildConditionNot(Unit(), block, IsNull(block, address));
  }
  static auto NoAddress(mir::Builder block, mir::TypeId pointer)
      -> mir::ExprId {
    return block.Add(mir::Expr{.data = mir::NullLiteral{}, .type = pointer});
  }

  auto CoreAddress(
      mir::Builder block, support::BuiltinFn fn, mir::ExprId array,
      std::vector<mir::ExprId> arguments) -> mir::ExprId {
    return Core(
        block, fn, array, std::move(arguments),
        Address(mir::Mutability::kMutable));
  }
  void CoreStatement(
      mir::Builder block, support::BuiltinFn fn, mir::ExprId array,
      std::vector<mir::ExprId> arguments) {
    Statement(
        block,
        Core(
            block, fn, array, std::move(arguments), Unit().builtins.void_type));
  }

  // The element and the index of the entry whose element lies at the address
  // `entry`, as places.
  auto ElementOf(
      mir::Builder block, mir::ExprId entry, mir::Mutability mutability)
      -> mir::ExprId {
    return block.Deref(
        Unit().types, CastTo(block, entry, ElementPointer(mutability)),
        element_);
  }
  auto IndexOf(mir::Builder block, mir::ExprId array, mir::ExprId entry)
      -> mir::ExprId {
    return block.Deref(
        Unit().types,
        CastTo(
            block,
            CoreAddress(
                block, support::BuiltinFn::kAssociativeArrayIndexOf, array,
                {entry}),
            IndexPointer(mir::Mutability::kReadOnly)),
        index_);
  }
  // Where the index the place `index` names lies, as the core reads it.
  auto AddressOfIndex(mir::Builder block, mir::ExprId index) -> mir::ExprId {
    return block.Add(
        mir::MakeAddressOfExpr(
            index, IndexPointer(mir::Mutability::kReadOnly)));
  }

  // Whether the index the place `index` names names an entry at all: one
  // holding x or z names none (LRM 7.8.6), which only an index type whose
  // values can hold one asks.
  auto Names(mir::Builder block, mir::ExprId index)
      -> std::optional<mir::ExprId> {
    if (kind_ == value::IndexKind::kWildcard) {
      return BuildConditionNot(
          Unit(), block,
          block.Add(
              mir::MakeBuiltinCall(
                  Unit(), block.Body(),
                  support::BuiltinFn::kWildcardIndexHasUnknown, std::nullopt,
                  {block.AsValue(Unit().types, index)},
                  Unit().builtins.machine_bool)));
    }
    if (!mir::CarriesUnknowns(Unit(), index_)) {
      return std::nullopt;
    }
    return BuildConditionNot(
        Unit(), block,
        BuildValueOperation(
            Unit(), block, support::UnknownItem::kHasUnknown, index, {},
            Unit().builtins.machine_bool));
  }

  // A local holding where the element of the entry the index the place
  // `index(block)` names lies in the array the place `array(block)` names, or
  // no address, the index holding no x or z: found by the core where it orders
  // the index, and otherwise by comparing each entry's index by the index
  // type's equality, which answers known where neither holds x or z (LRM
  // 7.8.5).
  template <typename ArrayAt, typename IndexAt>
  auto Found(
      mir::CallableCode& code, mir::Builder block, ArrayAt array, IndexAt index)
      -> mir::LocalId {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    if (kind_ != value::IndexKind::kOther) {
      return Declared(
          code, block, address,
          CoreAddress(
              block, support::BuiltinFn::kAssociativeArrayFind, array(block),
              {AddressOfIndex(block, index(block))}));
    }
    // A cursor from the first entry, advanced while the entry it is at holds
    // an index unequal to this one; no two entries hold equal indices, so it
    // stops at the one found, or past the last.
    const mir::LocalId found = Declared(
        code, block, address,
        CoreAddress(
            block, support::BuiltinFn::kAssociativeArrayFirst, array(block),
            {}));
    const mir::ExprId unequal = BuildConditionNot(
        Unit(), block,
        ReduceToCondition(
            Unit(), block,
            PartEqual(
                block,
                IndexOf(block, array(block), block.ReadLocal(found, address)),
                index(block), index_)));
    block.AppendStmt(
        mir::ForStmt{
            .init = {},
            .condition = Chosen(
                block, IsSome(block, block.ReadLocal(found, address)), unequal,
                BuildMachineBool(Unit(), block, false),
                Unit().builtins.machine_bool),
            .step = {block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(found, address),
                    CoreAddress(
                        block, support::BuiltinFn::kAssociativeArrayNext,
                        array(block), {block.ReadLocal(found, address)})))},
            .scope = block.AddScope(mir::Block{})});
    return found;
  }

  // A loop over the entries of the array the place `array(block)` names, in
  // its order, after what `block` holds, whose body `each(body, entry)`
  // states, `entry` the local holding where each one's element lies.
  template <typename ArrayAt, typename Each>
  void ForEachEntry(
      mir::CallableCode& code, mir::Builder block, ArrayAt array, Each each) {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    const mir::LocalId entry = code.AddLocal(address);
    mir::Block body;
    mir::Builder body_builder = block.Into(body);
    each(body_builder, entry);
    const mir::BlockId scope = block.AddScope(std::move(body));
    std::vector<mir::ForInit> init;
    init.emplace_back(
        mir::ForInitDecl{
            .induction_var = entry,
            .init = CoreAddress(
                block, support::BuiltinFn::kAssociativeArrayFirst, array(block),
                {})});
    block.AppendStmt(
        mir::ForStmt{
            .init = std::move(init),
            .condition = IsSome(block, block.ReadLocal(entry, address)),
            .step = {block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(entry, address),
                    CoreAddress(
                        block, support::BuiltinFn::kAssociativeArrayNext,
                        array(block), {block.ReadLocal(entry, address)})))},
            .scope = scope});
  }

  // Where the value an index naming no entry reads lies (LRM 7.8.6, 7.9.11):
  // the one a `default:` clause gave the array, or the element type's default.
  auto Absent(mir::CallableCode& code, mir::Builder block, mir::ExprId array)
      -> mir::ExprId {
    return DefaultOf(block, StatedDefaultOf(code, block, array));
  }

  // The same, read by the design (LRM 7.8.6): where the array was given no
  // `default:` value, the read is said with `report`, where it was written.
  auto Unwritten(
      mir::CallableCode& code, mir::Builder block, mir::ExprId array,
      support::BuiltinFn report) -> mir::ExprId {
    const mir::TypeId pointer = ElementPointer(mir::Mutability::kReadOnly);
    const mir::LocalId stated = StatedDefaultOf(code, block, array);
    mir::Block said;
    mir::Builder said_builder = block.Into(said);
    Statement(
        said_builder,
        said_builder.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = report}, {}, Unit().builtins.void_type)));
    block.AppendIfThen(
        IsNull(block, block.ReadLocal(stated, pointer)), std::move(said));
    return DefaultOf(block, stated);
  }

  // A local holding where the `default:` value the array the place `array`
  // names was given lies, or no address.
  auto StatedDefaultOf(
      mir::CallableCode& code, mir::Builder block, mir::ExprId array)
      -> mir::LocalId {
    return Declared(
        code, block, ElementPointer(mir::Mutability::kReadOnly),
        StatedDefaultAt(block, array));
  }
  // Where the `default:` value the array the place `array` names was given
  // lies, or no address.
  auto StatedDefaultAt(mir::Builder block, mir::ExprId array) -> mir::ExprId {
    return CastTo(
        block,
        Core(
            block, support::BuiltinFn::kAssociativeArrayStatedDefault, array,
            {}, Address(mir::Mutability::kReadOnly)),
        ElementPointer(mir::Mutability::kReadOnly));
  }

  // Where the `default:` value the local `stated` holds lies, or the element
  // default where it holds no address.
  auto DefaultOf(mir::Builder block, mir::LocalId stated) -> mir::ExprId {
    const mir::TypeId pointer = ElementPointer(mir::Mutability::kReadOnly);
    return Chosen(
        block, IsNull(block, block.ReadLocal(stated, pointer)),
        block.Add(
            mir::MakeBuiltinCall(
                Unit(), block.Body(), support::BuiltinFn::kElementDefault,
                std::nullopt, std::nullopt, {element_}, {}, pointer)),
        block.ReadLocal(stated, pointer), pointer);
  }

  // `value(block)` built in the storage `storage` points at, of `type`.
  template <typename Value>
  auto BuiltAt(
      mir::Builder block, mir::ExprId storage, mir::TypeId type, Value value)
      -> mir::ExprId {
    const mir::TypeId pointer =
        mir::BorrowedPointerTo(Unit().types, type, mir::Mutability::kMutable);
    return block.Add(
        mir::MakePlacementNewExpr(
            value(block),
            mir::PlacementAllocation{
                .allocate = support::BuiltinFn::kDefaultPlacementNew,
                .arguments = {CastTo(block, storage, pointer)}},
            pointer));
  }

  // An entry of the array the place `array(block)` names, its index a copy of
  // what `index(block)` names and its element `element(block)` builds, made
  // and committed; where its element lies.
  template <typename ArrayAt, typename IndexAt, typename ElementValue>
  auto Committed(
      mir::CallableCode& code, mir::Builder block, ArrayAt array, IndexAt index,
      ElementValue element) -> mir::LocalId {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    const mir::LocalId entry = Declared(
        code, block, address,
        CoreAddress(
            block, support::BuiltinFn::kAssociativeArrayPrepare, array(block),
            {}));
    Statement(
        block, BuiltAt(
                   block,
                   CoreAddress(
                       block, support::BuiltinFn::kAssociativeArrayIndexOf,
                       array(block), {block.ReadLocal(entry, address)}),
                   index_, [&](mir::Builder at) {
                     return at.AsValue(Unit().types, index(at));
                   }));
    Statement(
        block,
        BuiltAt(block, block.ReadLocal(entry, address), element_, element));
    CoreStatement(
        block, support::BuiltinFn::kAssociativeArrayCommit, array(block),
        {block.ReadLocal(entry, address)});
    return entry;
  }

  // LRM Table 7-1: an array of no entries, told its index kind and the
  // records of its index and element types.
  auto Default() -> mir::CallableCode {
    return Body(
        support::DefaultItem::kDeclaredDefault,
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          return Empty(block);
        });
  }
  auto Empty(mir::Builder block) -> mir::ExprId {
    const auto record = [&](mir::TypeId of) {
      return TypeRecordOf(Unit(), block, of);
    };
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(),
            support::BuiltinFn::kAssociativeArrayConstruct, std::nullopt,
            std::nullopt, {WholeType()},
            {mir::BuildMachineIntLiteral(
                 Unit(), block, static_cast<std::int64_t>(kind_)),
             record(index_), record(element_)},
            WholeType()));
  }

  // LRM 7.9.9: a copy of the array the parameter refers to, answered as a
  // value, as Rust's `Clone::clone` answers one: an array of no entries, then
  // the value a `default:` clause gave it and each entry copied in order.
  auto Copy() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kCopy),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId made =
              Declared(code, block, WholeType(), Empty(block));
          const auto into = [&](mir::Builder at) {
            return at.LocalPlace(made, WholeType());
          };
          const auto from = [&](mir::Builder at) { return Read(at, p[0]); };
          const mir::TypeId stated_pointer =
              ElementPointer(mir::Mutability::kReadOnly);
          const mir::LocalId stated = StatedDefaultOf(code, block, from(block));
          mir::Block given;
          mir::Builder given_builder = block.Into(given);
          SetDefaultTo(given_builder, into, [&](mir::Builder at) {
            return at.Deref(
                Unit().types, at.ReadLocal(stated, stated_pointer), element_);
          });
          block.AppendIfThen(
              IsSome(block, block.ReadLocal(stated, stated_pointer)),
              std::move(given));
          ForEachEntry(
              code, block, from, [&](mir::Builder body, mir::LocalId entry) {
                const mir::TypeId address = Address(mir::Mutability::kMutable);
                Committed(
                    code, body, into,
                    [&](mir::Builder at) {
                      return IndexOf(
                          at, from(at), at.ReadLocal(entry, address));
                    },
                    [&](mir::Builder at) {
                      return at.AsValue(
                          Unit().types, ElementOf(
                                            at, at.ReadLocal(entry, address),
                                            mir::Mutability::kReadOnly));
                    });
              });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = block.Add(
                      mir::Expr{
                          .data =
                              mir::MoveExpr{
                                  .operand =
                                      block.LocalPlace(made, WholeType())},
                          .type = WholeType()})});
        });
  }

  // LRM 7.9.9: the array written over clears its entries and takes a copy of
  // each of the source's; a copy is made first, so writing an array over
  // itself copies its entries before ending them, and the array then takes
  // the copy's entries as its own.
  auto Assign() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kAssign),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId made = Declared(
              code, block, WholeType(),
              Own(block, support::ValueStep::kCopy, Read(block, p[1]), {},
                  WholeType()));
          CoreStatement(
              block, support::BuiltinFn::kAssociativeArrayAdopt,
              Written(block, p[0]),
              {block.Add(
                  mir::Expr{
                      .data =
                          mir::BorrowExpr{
                              .operand = block.LocalPlace(made, WholeType())},
                      .type = Unit().types.Intern(
                          mir::Type{mir::ReferenceType{
                              .referent = WholeType(),
                              .mutability = mir::Mutability::kMutable}})})});
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = block.ReadLocal(p[0], code.locals.Get(p[0]).type)});
        });
  }

  // The array ended: the core ends each entry through its types' records,
  // leaving one a call leases to the last lease (LRM 13.5.2).
  auto End() -> mir::CallableCode {
    return Defined(
        Declaration(support::ValueStep::kEnd),
        [&](mir::CallableCode&, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          CoreStatement(
              block, support::BuiltinFn::kAssociativeArrayEnd,
              Written(block, p[0]), {});
        });
  }

  // The value a `default:` clause gives the array the place `array(block)`
  // names set to a copy of what `value(block)` names (LRM 7.9.11).
  template <typename ArrayAt, typename ValueAt>
  void SetDefaultTo(mir::Builder block, ArrayAt array, ValueAt value) {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    const mir::ExprId storage = CoreAddress(
        block, support::BuiltinFn::kAssociativeArrayPrepareDefault,
        array(block), {});
    const mir::ExprId built = BuiltAt(
        block, storage, element_,
        [&](mir::Builder at) { return at.AsValue(Unit().types, value(at)); });
    CoreStatement(
        block, support::BuiltinFn::kAssociativeArraySetDefault, array(block),
        {CastTo(block, built, address)});
  }

  // LRM 7.9.1: how many entries the array holds, as an `int`.
  auto Size() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kSize,
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          return CountAsInt(block, Read(block, p[0]));
        });
  }
  auto CountAsInt(mir::Builder block, mir::ExprId array) -> mir::ExprId {
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt, {CountOf(block, array)}, Unit().builtins.int_type));
  }

  // LRM 7.9.3: 1 where the index names an entry, 0 otherwise.
  auto Exists() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kExists),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          ReturnWhereNamesNone(block, p[1], [&](mir::Builder at) {
            return mir::BuildIntLiteral(Unit(), at, 0);
          });
          const mir::LocalId found = Found(
              code, block, [&](mir::Builder at) { return Read(at, p[0]); },
              [&](mir::Builder at) { return IndexAt(at, p[1]); });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Chosen(
                      block,
                      IsNull(
                          block,
                          block.ReadLocal(
                              found, Address(mir::Mutability::kMutable))),
                      mir::BuildIntLiteral(Unit(), block, 0),
                      mir::BuildIntLiteral(Unit(), block, 1),
                      Unit().builtins.int_type)});
        });
  }

  // `return` of what `answer` builds where the index the parameter `index`
  // refers to names no entry (LRM 7.8.6), stated where its type can hold x or
  // z.
  template <typename Answer>
  void ReturnWhereNamesNone(
      mir::Builder block, mir::LocalId index, Answer answer) {
    const std::optional<mir::ExprId> names =
        Names(block, IndexAt(block, index));
    if (!names.has_value()) {
      return;
    }
    ReturnWhere(block, BuildConditionNot(Unit(), block, *names), answer);
  }

  // LRM 7.8.6: where the element the index names lies, and where the value an
  // index naming no entry reads lies where it names none, which a read
  // answers without the array gaining an entry, saying so where the array was
  // given no `default:` value.
  auto ElementRead() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kAssocElement),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId pointer =
              ElementPointer(mir::Mutability::kReadOnly);
          ReturnWhereNamesNone(block, p[1], [&](mir::Builder at) {
            return Unwritten(
                code, at, Read(at, p[0]),
                support::BuiltinFn::kAssociativeArrayReadAtAnInvalidIndex);
          });
          const mir::TypeId address = Address(mir::Mutability::kMutable);
          const mir::LocalId found = Found(
              code, block, [&](mir::Builder at) { return Read(at, p[0]); },
              [&](mir::Builder at) { return IndexAt(at, p[1]); });
          ReturnWhere(
              block, IsSome(block, block.ReadLocal(found, address)),
              [&](mir::Builder at) {
                return CastTo(at, at.ReadLocal(found, address), pointer);
              });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Unwritten(
                      code, block, Read(block, p[0]),
                      support::BuiltinFn::
                          kAssociativeArrayReadAtAMissingEntry)});
        });
  }

  // What a write's step finds: where the element lies, or no address, and
  // whether the write made it.
  auto FoundElement(mir::Builder block, mir::ExprId element, mir::ExprId made)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = {element, made}},
            .type = mir::FoundElementType(Unit(), element_)});
  }

  // LRM 7.8.6, 7.8.7: where a write through the index lands: the element of
  // the entry it names, or one made for it holding the value an index naming
  // no entry reads, and nowhere where it holds x or z, which ignores the
  // write and says so.
  auto ElementPlace() -> mir::CallableCode {
    return Defined(
        Declaration(support::ElementAccessItem::kElementPlace),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId pointer = ElementPointer(mir::Mutability::kMutable);
          const mir::TypeId address = Address(mir::Mutability::kMutable);
          ReturnWhereNamesNone(block, p[1], [&](mir::Builder at) {
            Statement(
                at, at.Add(
                        mir::MakeCallExpr(
                            mir::Direct{
                                .target = support::BuiltinFn::
                                    kAssociativeArrayWrittenAtAnInvalidIndex},
                            {}, Unit().builtins.void_type)));
            return FoundElement(
                at, NoAddress(at, pointer),
                BuildMachineBool(Unit(), at, false));
          });
          const auto array = [&](mir::Builder at) { return Written(at, p[0]); };
          const mir::LocalId found = Found(
              code, block, array,
              [&](mir::Builder at) { return IndexAt(at, p[1]); });
          ReturnWhere(
              block, IsSome(block, block.ReadLocal(found, address)),
              [&](mir::Builder at) {
                return FoundElement(
                    at, CastTo(at, at.ReadLocal(found, address), pointer),
                    BuildMachineBool(Unit(), at, false));
              });
          const mir::TypeId absent_pointer =
              ElementPointer(mir::Mutability::kReadOnly);
          const mir::LocalId absent = Declared(
              code, block, absent_pointer, Absent(code, block, array(block)));
          const mir::LocalId made = Committed(
              code, block, array,
              [&](mir::Builder at) { return IndexAt(at, p[1]); },
              [&](mir::Builder at) {
                return at.AsValue(
                    Unit().types,
                    at.Deref(
                        Unit().types, at.ReadLocal(absent, absent_pointer),
                        element_));
              });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = FoundElement(
                      block,
                      CastTo(block, block.ReadLocal(made, address), pointer),
                      BuildMachineBool(Unit(), block, true))});
        });
  }

  // LRM 21.4.1: an associative memory loaded from the words a memory task
  // read, each landing on the entry its address names, built at the index type
  // so it is the entry an ordinary access of that key reaches, and made where
  // there is none.
  auto LoadWords() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kLoadWords),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          TellWords(block, p[1], support::BuiltinFn::kMemoryWordsReadKeyed, {});
          const mir::TypeId machine = Unit().builtins.machine_int64;
          const mir::TypeId found_type =
              mir::FoundElementType(Unit(), element_);
          const mir::TypeId pointer = ElementPointer(mir::Mutability::kMutable);
          ForEachIndex(
              code, block,
              AskWords(
                  block, p[1], support::BuiltinFn::kMemoryWordsCount, {},
                  machine),
              [&](mir::Builder body, mir::LocalId k) {
                const mir::LocalId key = Declared(
                    code, body, index_,
                    body.Add(
                        mir::MakeBuiltinCall(
                            Unit(), body.Body(),
                            support::BuiltinFn::kIntegralFromInt, std::nullopt,
                            {AskWords(
                                body, p[1], support::BuiltinFn::kMemoryWordsKey,
                                {CountNow(body, k)}, machine)},
                            index_)));
                const mir::LocalId found = Declared(
                    code, body, found_type,
                    Own(body, support::ElementAccessItem::kElementPlace,
                        Written(body, p[0]), {body.LocalPlace(key, index_)},
                        found_type));
                const mir::ExprId element = body.Read(body.Add(
                    mir::MakeFieldAccessExpr(
                        body.LocalPlace(found, found_type),
                        mir::ComponentFieldTarget{
                            .position = base::ComponentIndex{0}},
                        pointer)));
                Statement(
                    body,
                    body.Add(BuildStoreExpr(
                        Unit(), body,
                        AccessPath{
                            .owner =
                                body.Deref(Unit().types, element, element_),
                            .descent = {}},
                        ElementAt(
                            body, support::BuiltinFn::kMemoryWordsWord,
                            Words(body, p[1]), CountNow(body, k), element_))));
              });
          block.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
        });
  }

  // LRM 21.5.3: each entry of an associative memory handed to a memory task's
  // dump in the array's index order, then the dump written.
  auto StoreWords() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kStoreWords),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId address = Address(mir::Mutability::kMutable);
          const auto array = [&](mir::Builder at) { return Read(at, p[0]); };
          ForEachEntry(
              code, block, array, [&](mir::Builder body, mir::LocalId entry) {
                TellWords(
                    body, p[1], support::BuiltinFn::kMemoryWordsPutKeyed,
                    {BuildToInt64Call(
                         Unit(), body,
                         body.AsValue(
                             Unit().types,
                             IndexOf(
                                 body, array(body),
                                 body.ReadLocal(entry, address)))),
                     body.AsValue(
                         Unit().types, ElementOf(
                                           body, body.ReadLocal(entry, address),
                                           mir::Mutability::kReadOnly))});
              });
          TellWords(block, p[1], support::BuiltinFn::kMemoryWordsCommit, {});
          block.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
        });
  }

  // LRM 7.8.7: where the element the index names lies, to write it, made where
  // it names none, and storage no read reaches where it holds x or z.
  auto ElementWrite() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kAssocElementMut),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId found_type =
              mir::FoundElementType(Unit(), element_);
          const mir::TypeId pointer = ElementPointer(mir::Mutability::kMutable);
          const mir::LocalId found = Declared(
              code, block, found_type,
              Own(block, support::ElementAccessItem::kElementPlace,
                  Written(block, p[0]), {IndexAt(block, p[1])}, found_type));
          const auto element = [&](mir::Builder at) {
            return at.Read(at.Add(
                mir::MakeFieldAccessExpr(
                    at.LocalPlace(found, found_type),
                    mir::ComponentFieldTarget{
                        .position = base::ComponentIndex{0}},
                    pointer)));
          };
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Chosen(
                      block, IsNull(block, element(block)),
                      block.Add(
                          mir::MakeBuiltinCall(
                              Unit(), block.Body(),
                              support::BuiltinFn::kDiscardedWrite, std::nullopt,
                              std::nullopt, {element_}, {}, pointer)),
                      element(block), pointer)});
        });
  }

  // LRM 7.9.2: every entry removed, and the one the index names; an index
  // naming none removes nothing and says nothing.
  auto Delete() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kDelete),
        [&](mir::CallableCode&, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          CoreStatement(
              block, support::BuiltinFn::kAssociativeArrayClear,
              Written(block, p[0]), {});
        });
  }
  auto DeleteIndex() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kAssocDeleteIndex),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::TypeId address = Address(mir::Mutability::kMutable);
          const std::optional<mir::ExprId> names =
              Names(block, IndexAt(block, p[1]));
          mir::Block named;
          mir::Builder named_builder = block.Into(named);
          const auto array = [&](mir::Builder at) { return Written(at, p[0]); };
          const mir::LocalId found = Found(
              code, named_builder, array,
              [&](mir::Builder at) { return IndexAt(at, p[1]); });
          mir::Block removed;
          mir::Builder removed_builder = named_builder.Into(removed);
          CoreStatement(
              removed_builder, support::BuiltinFn::kAssociativeArrayRemove,
              array(removed_builder),
              {removed_builder.ReadLocal(found, address)});
          named_builder.AppendIfThen(
              IsSome(named_builder, named_builder.ReadLocal(found, address)),
              std::move(removed));
          if (names.has_value()) {
            block.AppendIfThen(
                ReduceToCondition(Unit(), block, *names), std::move(named));
            return;
          }
          block.AppendStmt(
              mir::BlockStmt{.scope = block.AddScope(std::move(named))});
        });
  }

  // LRM 7.9.4, 7.9.5: where the least or the greatest index the array holds
  // lies, none where it holds none.
  auto Edge(support::BuiltinFn method, support::BuiltinFn edge)
      -> mir::CallableCode {
    return Defined(
        Declaration(method), [&, edge](
                                 mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const mir::LocalId entry = Declared(
              code, block, Address(mir::Mutability::kMutable),
              CoreAddress(block, edge, Read(block, p[0]), {}));
          block.AppendStmt(
              mir::ReturnStmt{.value = IndexAnswered(block, p[0], entry)});
        });
  }

  // LRM 7.9.6, 7.9.7: where the least index after the one the parameter
  // `index` refers to lies, or the greatest before it, none where there is
  // none: found by the core where it orders the index, and otherwise as the
  // entry made after or before the one the index names, an index naming none
  // having no place in that order.
  auto Neighbour(
      support::BuiltinFn method, support::BuiltinFn by_order,
      support::BuiltinFn by_entry) -> mir::CallableCode {
    return Defined(
        Declaration(method), [&, by_order, by_entry](
                                 mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const mir::TypeId address = Address(mir::Mutability::kMutable);
          const auto array = [&](mir::Builder at) { return Read(at, p[0]); };
          if (kind_ != value::IndexKind::kOther) {
            const mir::LocalId entry = Declared(
                code, block, address,
                CoreAddress(
                    block, by_order, array(block),
                    {AddressOfIndex(block, IndexAt(block, p[1]))}));
            block.AppendStmt(
                mir::ReturnStmt{.value = IndexAnswered(block, p[0], entry)});
            return;
          }
          const mir::LocalId found = Found(
              code, block, array,
              [&](mir::Builder at) { return IndexAt(at, p[1]); });
          ReturnWhere(
              block, IsNull(block, block.ReadLocal(found, address)),
              [&](mir::Builder at) {
                return NoAddress(at, IndexPointer(mir::Mutability::kReadOnly));
              });
          const mir::LocalId entry = Declared(
              code, block, address,
              CoreAddress(
                  block, by_entry, array(block),
                  {block.ReadLocal(found, address)}));
          block.AppendStmt(
              mir::ReturnStmt{.value = IndexAnswered(block, p[0], entry)});
        });
  }

  // Where the index of the entry whose element lies where the local `entry`
  // says lies, or none where it says none, in the array the parameter `array`
  // refers to.
  auto IndexAnswered(mir::Builder block, mir::LocalId array, mir::LocalId entry)
      -> mir::ExprId {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    const mir::TypeId pointer = IndexPointer(mir::Mutability::kReadOnly);
    return Chosen(
        block, IsNull(block, block.ReadLocal(entry, address)),
        NoAddress(block, pointer),
        CastTo(
            block,
            CoreAddress(
                block, support::BuiltinFn::kAssociativeArrayIndexOf,
                Read(block, array), {block.ReadLocal(entry, address)}),
            pointer),
        pointer);
  }

  // LRM 7.9.11: where the value a `default:` clause gave the array lies, none
  // where none did; and that value set to a copy of another.
  auto StatedDefault() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kAssocStatedDefault,
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          return StatedDefaultAt(block, Read(block, p[0]));
        });
  }
  auto SetStatedDefault() -> mir::CallableCode {
    return Defined(
        Declaration(support::BuiltinFn::kAssocSetStatedDefault),
        [&](mir::CallableCode&, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          SetDefaultTo(
              block, [&](mir::Builder at) { return Written(at, p[0]); },
              [&](mir::Builder at) { return Referent(at, p[1], element_); });
        });
  }

  // A value taken whole is one position, so taking any of it takes all of it.
  auto TakePositions() -> mir::CallableCode {
    return Body(
        support::PositionsItem::kTakePositions,
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          return block.AsValue(Unit().types, Read(block, p[1]));
        });
  }

  // For each entry of the array the first parameter refers to, in order, the
  // element of the entry the second's index names, where the two hold as many
  // entries: `pair(body, element, other)` builds whether the pair holds,
  // `other` where the second array's element lies, or no address where it
  // holds none, and `answer(at, holds)` what the whole answers.
  template <typename Pair, typename Answer>
  void EveryPairHolds(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p, Pair pair, Answer answer) {
    const mir::TypeId address = Address(mir::Mutability::kMutable);
    ReturnWhere(
        block,
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kInequality,
            CountOf(block, Read(block, p[0])),
            CountOf(block, Read(block, p[1]))),
        [&](mir::Builder at) { return answer(at, false); });
    ForEachEntry(
        code, block, [&](mir::Builder at) { return Read(at, p[0]); },
        [&](mir::Builder body, mir::LocalId entry) {
          const mir::LocalId other = Found(
              code, body, [&](mir::Builder at) { return Read(at, p[1]); },
              [&](mir::Builder at) {
                return IndexOf(
                    at, Read(at, p[0]), at.ReadLocal(entry, address));
              });
          ReturnWhere(
              body, IsNull(body, body.ReadLocal(other, address)),
              [&](mir::Builder at) { return answer(at, false); });
          ReturnWhere(
              body,
              BuildConditionNot(
                  Unit(), body,
                  pair(
                      body,
                      ElementOf(
                          body, body.ReadLocal(entry, address),
                          mir::Mutability::kReadOnly),
                      ElementOf(
                          body, body.ReadLocal(other, address),
                          mir::Mutability::kReadOnly))),
              [&](mir::Builder at) { return answer(at, false); });
        });
    block.AppendStmt(mir::ReturnStmt{.value = answer(block, true)});
  }
  // How many entries the array the place `array` names holds, as a machine
  // count.
  auto CountOf(mir::Builder block, mir::ExprId array) -> mir::ExprId {
    return CastTo(
        block,
        block.Read(
            mir::LibraryRecordMemberPlace(
                Unit(), block, array, &value::AssociativeArrayCore::count)),
        Unit().builtins.machine_int64);
  }

  // LRM 11.4.5: equal where both hold one set of indices and every pair of
  // elements is equal, unknown where no pair is unequal and some pair is
  // unknown.
  auto Equality() -> mir::CallableCode {
    const mir::TypeId answer = EqualityAnswer();
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    // An element that holds no x or z compares known, so the answer is too.
    if (!mir::IsFourState(Unit().types.Get(answer).Integral().state_kind)) {
      return Defined(
          Declaration(support::EqualityItem::kEqual),
          [&](mir::CallableCode& code, mir::Builder block,
              const std::vector<mir::LocalId>& p) {
            EveryPairHolds(
                code, block, p,
                [&](mir::Builder pair, mir::ExprId lhs, mir::ExprId rhs) {
                  return ReduceToCondition(
                      Unit(), pair, PartEqual(pair, lhs, rhs, element_));
                },
                [&](mir::Builder at, bool holds) {
                  return KnownBit(at, answer, holds);
                });
          });
    }
    return Defined(
        Declaration(support::EqualityItem::kEqual),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const mir::LocalId unknown = Declared(
              code, block, boolean, BuildMachineBool(Unit(), block, false));
          EveryPairHolds(
              code, block, p,
              [&](mir::Builder pair, mir::ExprId lhs, mir::ExprId rhs) {
                const mir::LocalId equal = Declared(
                    code, pair, answer, PartEqual(pair, lhs, rhs, element_));
                Statement(
                    pair,
                    pair.Add(
                        mir::MakeAssignExpr(
                            Unit().builtins, pair.LocalPlace(unknown, boolean),
                            pair.Add(
                                mir::MakeBinary(
                                    Unit(), pair.Body(),
                                    mir::BinaryOp::kBitwiseOr,
                                    pair.ReadLocal(unknown, boolean),
                                    BuildValueOperation(
                                        Unit(), pair,
                                        support::UnknownItem::kHasUnknown,
                                        pair.ReadLocal(equal, answer), {},
                                        boolean),
                                    boolean)))));
                return BuildConditionNot(
                    Unit(), pair,
                    ReduceToCondition(
                        Unit(), pair,
                        pair.Add(
                            mir::MakeUnary(
                                Unit(), pair.Body(), mir::UnaryOp::kLogicalNot,
                                pair.ReadLocal(equal, answer), answer))));
              },
              [&](mir::Builder at, bool holds) -> mir::ExprId {
                if (!holds) {
                  return KnownBit(at, answer, false);
                }
                return Chosen(
                    at, at.ReadLocal(unknown, boolean),
                    mir::BuildIntegralLiteral(
                        Unit(), at, answer,
                        mir::IntegralConstant{
                            .value_words = {1U}, .state_words = {1U}}),
                    KnownBit(at, answer, true), answer);
              });
        });
  }

  // LRM 11.4.5 `===`, which is never unknown.
  auto CaseEqual() -> mir::CallableCode {
    return Defined(
        Declaration(support::EqualityItem::kCaseEqual),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          EveryPairHolds(
              code, block, p,
              [&](mir::Builder pair, mir::ExprId lhs, mir::ExprId rhs) {
                return ReduceToCondition(
                    Unit(), pair, BuildCaseEquality(Unit(), pair, lhs, rhs));
              },
              [&](mir::Builder at, bool holds) {
                return mir::BuildBit1Literal(Unit(), at, holds);
              });
        });
  }

  // LRM 9.4.2: whether a write over the array changed nothing: the value an
  // index naming no entry reads is the array's, so it is compared too.
  auto BitIdentical() -> mir::CallableCode {
    return Defined(
        Declaration(support::ChangeTestItem::kBitIdentical),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const auto absent = [&](mir::Builder at, mir::LocalId array) {
            return at.Deref(
                Unit().types, Absent(code, at, Read(at, array)), element_);
          };
          ReturnWhere(
              block,
              BuildConditionNot(
                  Unit(), block,
                  BuildBitIdentity(
                      Unit(), block, absent(block, p[0]), absent(block, p[1]))),
              [&](mir::Builder at) {
                return BuildMachineBool(Unit(), at, false);
              });
          EveryPairHolds(
              code, block, p,
              [&](mir::Builder pair, mir::ExprId lhs, mir::ExprId rhs) {
                return BuildBitIdentity(Unit(), pair, lhs, rhs);
              },
              [&](mir::Builder at, bool holds) {
                return BuildMachineBool(Unit(), at, holds);
              });
        });
  }

  // LRM 20.9: whether any element holds an x or a z, settled by the first that
  // does; an index never does, since a write through one holding either is
  // ignored (LRM 7.8.6).
  auto HasUnknown() -> mir::CallableCode {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    return Defined(
        Declaration(support::UnknownItem::kHasUnknown),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          ForEachEntry(
              code, block, [&](mir::Builder at) { return Read(at, p[0]); },
              [&](mir::Builder body, mir::LocalId entry) {
                ReturnWhere(
                    body,
                    BuildValueOperation(
                        Unit(), body, support::UnknownItem::kHasUnknown,
                        ElementOf(
                            body,
                            body.ReadLocal(
                                entry, Address(mir::Mutability::kMutable)),
                            mir::Mutability::kReadOnly),
                        {}, boolean),
                    [&](mir::Builder at) {
                      return BuildMachineBool(Unit(), at, true);
                    });
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = BuildMachineBool(Unit(), block, false)});
        });
  }

  // What `combine(body, kept, each)` makes of every element's `each(body,
  // element)`, in the array's order, kept in an `int` starting at 0.
  template <typename Each>
  auto Summed(
      mir::CallableCode& code, mir::Builder block, mir::LocalId array,
      Each each) -> mir::ExprId {
    const mir::TypeId count = Unit().builtins.int_type;
    const mir::LocalId kept =
        Declared(code, block, count, mir::BuildIntLiteral(Unit(), block, 0));
    ForEachEntry(
        code, block, [&](mir::Builder at) { return Read(at, array); },
        [&](mir::Builder body, mir::LocalId entry) {
          const mir::ExprId element = ElementOf(
              body, body.ReadLocal(entry, Address(mir::Mutability::kMutable)),
              mir::Mutability::kReadOnly);
          Statement(
              body, body.Add(
                        mir::MakeAssignExpr(
                            Unit().builtins, body.LocalPlace(kept, count),
                            body.Add(
                                mir::MakeBinary(
                                    Unit(), body.Body(), mir::BinaryOp::kAdd,
                                    body.ReadLocal(kept, count),
                                    each(body, element), count)))));
        });
    return block.ReadLocal(kept, count);
  }

  // LRM 20.6.2 `$bits`: the bits every element holds, laid end to end, an
  // index being no part of the stream (LRM 11.4.14.1).
  auto BitWidth() -> mir::CallableCode {
    return Defined(
        Declaration(support::StreamItem::kBitstreamWidth),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements(element_)) {
            RefuseTheStream(block, mir::BuildIntLiteral(Unit(), block, 0));
            return;
          }
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Summed(
                      code, block, p[0],
                      [&](mir::Builder body, mir::ExprId element) {
                        return BuildBitWidth(Unit(), body, element);
                      })});
        });
  }

  // LRM 20.9 `$countbits`: the bits of each element that hold one of the
  // control set's values.
  auto CountBits() -> mir::CallableCode {
    const mir::TypeId control = mir::BitCountControlType(Unit().types);
    return Defined(
        Declaration(support::StreamItem::kCountBits),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements(element_)) {
            RefuseTheStream(block, mir::BuildIntLiteral(Unit(), block, 0));
            return;
          }
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = Summed(
                      code, block, p[0],
                      [&](mir::Builder body, mir::ExprId element) {
                        return BuildBitCount(
                            Unit(), body, element,
                            Referent(body, p[1], control));
                      })});
        });
  }

  // LRM 11.4.14.1: the elements' streams laid end to end into a run of bits,
  // the least index's most significant. The run is as wide as the array's
  // stream before any element is written, so each element's bits are written
  // where they lie.
  auto ToBitVec() -> mir::CallableCode {
    const mir::TypeId run = Unit().builtins.bit_vec;
    const mir::TypeId machine = Unit().builtins.machine_int64;
    return Defined(
        Declaration(support::StreamItem::kToBitVec),
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          if (!StreamsItsElements(element_)) {
            RefuseTheStream(
                block, BuildRunOfZeros(
                           Unit(), block,
                           mir::BuildMachineIntLiteral(Unit(), block, 0)));
            return;
          }
          const mir::LocalId bits = Declared(
              code, block, run,
              BuildRunOfZeros(
                  Unit(), block,
                  BuildToInt64Call(
                      Unit(), block,
                      BuildBitWidth(Unit(), block, Read(block, p[0])))));
          const mir::LocalId at = Declared(
              code, block, machine,
              mir::BuildMachineIntLiteral(Unit(), block, 0));
          ForEachEntry(
              code, block, [&](mir::Builder in) { return Read(in, p[0]); },
              [&](mir::Builder body, mir::LocalId entry) {
                auto element = BuildToBitstream(
                    Unit(), body,
                    ElementOf(
                        body,
                        body.ReadLocal(
                            entry, Address(mir::Mutability::kMutable)),
                        mir::Mutability::kReadOnly),
                    {});
                if (!element) {
                  throw InternalError(
                      "hir_to_mir: an element of an associative array holding "
                      "a stream has none -- please report this as a bug");
                }
                const mir::LocalId part = Declared(
                    code, body, run,
                    BuildRunOf(
                        Unit(), body, body.AsValue(Unit().types, *element)));
                InsertIntoRun(
                    Unit(), body, bits, body.ReadLocal(at, machine),
                    body.ReadLocal(part, run));
                Statement(
                    body, body.Add(
                              mir::MakeAssignExpr(
                                  Unit().builtins, body.LocalPlace(at, machine),
                                  mir::BuildMachineIntBinary(
                                      Unit(), body, mir::BinaryOp::kAdd,
                                      body.ReadLocal(at, machine),
                                      BuildRunLength(
                                          Unit(), body,
                                          body.ReadLocal(part, run))))));
              });
          block.AppendStmt(
              mir::ReturnStmt{.value = block.ReadLocal(bits, run)});
        });
  }

  mir::TypeId element_;
  mir::TypeId index_;
  value::IndexKind kind_;
};

constexpr std::array<
    AssociativeBodies::OperationBody, mir::kAssociativeArrayMethods.size()>
    AssociativeBodies::kMethodBodies{{
        {.name = support::BuiltinFn::kSize,
         .build = [](AssociativeBodies& b) { return b.Size(); }},
        {.name = support::BuiltinFn::kExists,
         .build = [](AssociativeBodies& b) { return b.Exists(); }},
        {.name = support::BuiltinFn::kAssocElement,
         .build = [](AssociativeBodies& b) { return b.ElementRead(); }},
        {.name = support::BuiltinFn::kAssocElementMut,
         .build = [](AssociativeBodies& b) { return b.ElementWrite(); }},
        {.name = support::BuiltinFn::kDelete,
         .build = [](AssociativeBodies& b) { return b.Delete(); }},
        {.name = support::BuiltinFn::kAssocDeleteIndex,
         .build = [](AssociativeBodies& b) { return b.DeleteIndex(); }},
        {.name = support::BuiltinFn::kAssocFirst,
         .build =
             [](AssociativeBodies& b) {
               return b.Edge(
                   support::BuiltinFn::kAssocFirst,
                   support::BuiltinFn::kAssociativeArrayFirst);
             }},
        {.name = support::BuiltinFn::kAssocLast,
         .build =
             [](AssociativeBodies& b) {
               return b.Edge(
                   support::BuiltinFn::kAssocLast,
                   support::BuiltinFn::kAssociativeArrayLast);
             }},
        {.name = support::BuiltinFn::kAssocNext,
         .build =
             [](AssociativeBodies& b) {
               return b.Neighbour(
                   support::BuiltinFn::kAssocNext,
                   support::BuiltinFn::kAssociativeArrayAfter,
                   support::BuiltinFn::kAssociativeArrayNext);
             }},
        {.name = support::BuiltinFn::kAssocPrev,
         .build =
             [](AssociativeBodies& b) {
               return b.Neighbour(
                   support::BuiltinFn::kAssocPrev,
                   support::BuiltinFn::kAssociativeArrayBefore,
                   support::BuiltinFn::kAssociativeArrayPrevious);
             }},
        {.name = support::BuiltinFn::kAssocStatedDefault,
         .build = [](AssociativeBodies& b) { return b.StatedDefault(); }},
        {.name = support::BuiltinFn::kAssocSetStatedDefault,
         .build = [](AssociativeBodies& b) { return b.SetStatedDefault(); }},
    }};

// A memory's (LRM 21.4.1).
constexpr std::array<
    AssociativeBodies::OperationBody, support::kMemoryWordsRequirements.size()>
    AssociativeBodies::kLanguageTraitBodies{{
        {.name = support::BuiltinFn::kLoadWords,
         .build = [](AssociativeBodies& b) { return b.LoadWords(); }},
        {.name = support::BuiltinFn::kStoreWords,
         .build = [](AssociativeBodies& b) { return b.StoreWords(); }},
    }};

constexpr std::array<
    AssociativeBodies::OperationBody, mir::kAssociativeArrayOperations.size()>
    AssociativeBodies::kOperationBodies =
        Concatenated(kMethodBodies, kLanguageTraitBodies);

// The table builds exactly the operations MIR declares an associative array is
// called by.
static_assert(mir::NamesTheSameOperations(
    mir::kAssociativeArrayOperations, AssociativeBodies::kOperationBodies));

}  // namespace

auto BuildAssociativeOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode {
  const mir::Type& type = unit.types.Get(source.type);
  const std::optional<mir::TypeId> element = type.ContainerElementType();
  const std::optional<mir::TypeId> index = type.IndexType();
  if (!element.has_value() || !index.has_value()) {
    throw InternalError(
        "hir_to_mir: an associative array's operation is asked of a type that "
        "is no associative array -- please report this as a bug");
  }
  return AssociativeBodies(unit, source.type, *element, *index).Build(source);
}

}  // namespace lyra::lowering::hir_to_mir
