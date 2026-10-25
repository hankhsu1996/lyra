#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// `closure` added to `block` made in a box of its own.
auto BuildClosureBox(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId box = mir::BoxOf(unit.types, closure.type);
  return block.exprs.Add(
      mir::MakeNewExpr(block.exprs.Add(std::move(closure)), box));
}

// What a closure of `closure_type` is once its type is no longer known: a
// callable of its kind, taking what its body takes and answering what its
// body answers.
auto ErasedOf(const mir::CompilationUnit& unit, mir::TypeId closure_type)
    -> mir::TypeId {
  return mir::ErasedTypeOf(
      unit.types,
      unit.GetClosure(
          unit.types.Get(closure_type).Get<mir::ClosureType>().closure_id));
}

// `held`, a box or a borrow of a closure, made one of the closure of no known
// type `erased`, holding it as `type` does.
auto Unsized(
    mir::Block& block, mir::ExprId held, mir::TypeId erased, mir::TypeId type)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kUnsize,
              .type_arguments = {erased}},
          {held}, type));
}

}  // namespace

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing,
    support::ClosureKind kind, CapturePolicy policy)
    : unit_(&unit),
      outer_(enclosing.current_block),
      closure_decl_{},
      closure_id_(unit.DeclareClosure()),
      bindings_(
          unit, closure_decl_, closure_id_, kind, *enclosing.bindings, *outer_,
          std::move(policy)),
      frame_(enclosing.WithBlock(&closure_decl_.invoke.Body())
                 .WithBindings(&bindings_)
                 .WithReportingReachedTo(std::nullopt)) {
}

auto ClosureBuilder::AddParam(mir::TypeId type) -> mir::LocalId {
  const mir::LocalId binding = bindings_.DeclareAnonymous(type);
  invocation_params_.push_back(binding);
  return binding;
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  closure_decl_.invoke.result_type = result_type;
  closure_decl_.invoke.params = invocation_params_;
  std::vector<mir::FieldInit> field_inits = bindings_.Finalize();
  const mir::TypeId value_type = unit_->types.Intern(
      mir::Type{mir::ClosureType{.closure_id = closure_id_}});
  unit_->DefineClosure(closure_id_, std::move(closure_decl_));
  return mir::Expr{
      .data =
          mir::ClosureExpr{
              .closure = closure_id_, .field_inits = std::move(field_inits)},
      .type = value_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type =
      closure_decl_.invoke.Body().exprs.Get(result).type;
  closure_decl_.invoke.Body().AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildVoid() -> mir::Expr {
  return Finish(unit_->builtins.void_type);
}

auto ClosureBuilder::BuildCallOnce() -> mir::Expr {
  closure_decl_.invoke.Body().AppendStmt(
      mir::ReturnStmt{.value = std::nullopt});
  const mir::TypeId result_type = unit_->builtins.coroutine_void;
  const mir::ExprId box = BuildClosureBox(*unit_, *outer_, Finish(result_type));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = mir::ClosureTarget{.closure = closure_id_},
                      .receiver = box},
              .arguments = {}},
      .type = result_type};
}

auto BuildKeptClosure(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId erased = ErasedOf(unit, closure.type);
  const mir::ExprId box = BuildClosureBox(unit, block, std::move(closure));
  return Unsized(block, box, erased, mir::BoxOf(unit.types, erased));
}

auto BuildEmplacedClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Expr keeper,
    mir::Expr closure) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypePool& types = unit.types;
  mir::Block& block = *frame.current_block;
  auto& effect = std::get<mir::ClosureExpr>(closure.data);

  // The maker refers to each value the effect is made of where it lies, a
  // value made by its source first held where the maker's call outlasts it,
  // and its body reads each through that reference into the effect it answers.
  const mir::ClosureId maker_id = unit.DeclareClosure();
  const mir::TypeId maker_type =
      types.Intern(mir::Type{mir::ClosureType{.closure_id = maker_id}});
  mir::ClosureDecl maker;
  const mir::TypeId receiver_type =
      mir::ClosureReceiverType(types, maker_id, support::ClosureKind::kFn);
  const mir::LocalId receiver = maker.invoke.AddLocal(receiver_type);
  maker.invoke.receiver = receiver;
  maker.invoke.result_type = closure.type;
  mir::Block& body = maker.invoke.Body();
  std::vector<mir::FieldInit> referred;
  for (mir::FieldInit& init : effect.field_inits) {
    const mir::TypeId value_type = block.exprs.Get(init.value).type;
    const mir::ExprId lying =
        mir::ValueLiesInStorage(unit, block, init.value)
            ? init.value
            : block.ReadLocal(DeclareLocal(frame, init.value), value_type);
    const mir::ExprId reference = mir::Borrow(unit, block, lying);
    const mir::TypeId reference_type = block.exprs.Get(reference).type;
    const mir::FieldId field =
        maker.fields.Add(mir::FieldDecl{.type = reference_type});
    maker.field_order.push_back(field);
    referred.push_back(mir::FieldInit{.target = field, .value = reference});
    const mir::ExprId held = body.exprs.Add(
        mir::MakeFieldAccessExpr(
            body.Deref(
                types, body.LocalPlace(receiver, receiver_type), maker_type),
            mir::ClosureFieldTarget{.owner = maker_id, .slot = field},
            reference_type));
    init.value = body.Read(body.Deref(types, held, value_type));
  }
  const mir::TypeId effect_type = closure.type;
  body.AppendStmt(mir::ReturnStmt{.value = body.exprs.Add(std::move(closure))});
  unit.DefineClosure(maker_id, std::move(maker));

  // The maker lies in this frame for the call, which is handed its address.
  const mir::LocalId make = DeclareLocal(
      frame,
      block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ClosureExpr{
                      .closure = maker_id, .field_inits = std::move(referred)},
              .type = maker_type}));
  const mir::ExprId make_address = block.exprs.Add(
      mir::MakeAddressOfExpr(
          block.LocalPlace(make, maker_type),
          types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = maker_type,
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kReadOnly}})));
  const mir::TypeId keeper_type =
      types.Get(keeper.type).Get<mir::PointerType>().pointee;
  return mir::MakeCallExpr(
      mir::Direct{
          .target = unit_lowerer.EmplaceInto(),
          .type_arguments = {keeper_type, effect_type, maker_type}},
      {block.exprs.Add(std::move(keeper)), make_address},
      unit.builtins.void_type);
}

auto BuildBorrowedClosure(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId closure)
    -> mir::ExprId {
  const mir::TypeId closure_type = block.exprs.Get(closure).type;
  const mir::ExprId borrowed = mir::Borrow(unit, block, closure);
  const mir::TypeId erased = ErasedOf(unit, closure_type);
  return Unsized(
      block, borrowed, erased,
      unit.types.Intern(
          mir::Type{mir::ReferenceType{
              .referent = erased, .mutability = mir::Mutability::kReadOnly}}));
}

}  // namespace lyra::lowering::hir_to_mir
