#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/entry_walk.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/synthesized_function.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The callables of a covergroup's class, in the order its shape reserves them:
// the two virtual methods the first class of a lineage introduces, in the
// order another unit counts them, then the two of the class itself.
constexpr mir::CallableId kSample{0};
constexpr mir::CallableId kMakeRecord{1};
constexpr mir::CallableId kTypeRecord{2};
constexpr mir::CallableId kDeclareType{3};
constexpr std::uint32_t kSampleBehavior = 0;
constexpr std::uint32_t kMakeRecordBehavior = 1;

// The library's property `property`, as a member access names it.
auto LibraryField(
    UnitLowerer& unit_lowerer, const CoverageLibrary::Property& property)
    -> mir::ClassFieldTarget {
  return unit_lowerer.TranslateClassPropertyTarget(property.target);
}

// The member `name` of the library's options structure `path` names, whose
// type in the library is `type`.
auto OptionMember(
    UnitLowerer& unit_lowerer, AccessPath path, hir::TypeId type,
    std::string_view name) -> AccessPath {
  const auto& structure =
      unit_lowerer.Hir().types.Get(type).Get<hir::UnpackedStructType>();
  for (std::size_t i = 0; i < structure.fields.size(); ++i) {
    if (structure.fields[i].name != name) continue;
    return DescendInto(
        std::move(path),
        DescentStep{
            .to =
                StepToComponent{
                    .position =
                        base::ComponentIndex{static_cast<std::uint32_t>(i)}},
            .part_type = unit_lowerer.TranslateType(structure.fields[i].type)});
  }
  throw InternalError(
      std::format(
          "the coverage library's options have no member '{}' -- please "
          "report this as a bug",
          name));
}

// The type options of the item `key` names, the element of the type record's
// table `table`, of the library type `table_type`, keys it by.
auto ItemTypeOptions(
    UnitLowerer& unit_lowerer, AccessPath table, hir::TypeId table_type,
    mir::ExprId key) -> std::pair<AccessPath, hir::TypeId> {
  const hir::TypeId element = unit_lowerer.Hir()
                                  .types.Get(table_type)
                                  .Get<hir::AssociativeArrayType>()
                                  .element_type;
  return std::pair{
      DescendInto(
          std::move(table),
          DescentStep{
              .to = StepToAssocElement{.key = key},
              .part_type = unit_lowerer.TranslateType(element)}),
      element};
}

// The handle the library's registry answers a covergroup type's record as.
auto TypeRecordType(UnitLowerer& unit_lowerer, const CoverageLibrary& library)
    -> mir::TypeId {
  return unit_lowerer.TranslateType(
      library.MethodOf(library.Refs().registry, "type_named")
          .prototype.result_type);
}

auto PublishedOf(const UnitLowerer& unit_lowerer, hir::CovergroupId ref)
    -> const hir::PublishedCovergroup& {
  return unit_lowerer.Hir().covergroups.Published(ref);
}

// The first covergroup of the lineage `ref` belongs to (LRM 19.4.1).
auto FirstOf(const UnitLowerer& unit_lowerer, hir::CovergroupId ref)
    -> hir::CovergroupId {
  while (const auto& base = PublishedOf(unit_lowerer, ref).base) {
    ref = *base;
  }
  return ref;
}

auto HandleTo(UnitLowerer& unit_lowerer, hir::ClassId cls) -> mir::TypeId {
  const mir::TypeId object = unit_lowerer.ClassPointee(cls);
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ManagedRefType{.pointee = object}});
}

auto CovergroupHandle(UnitLowerer& unit_lowerer, hir::CovergroupId covergroup)
    -> mir::TypeId {
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ManagedRefType{
          .pointee = unit_lowerer.CovergroupObjectType(covergroup)}});
}

// The behavior `behavior` the first class of `ref`'s lineage introduced.
auto IntroducedSlot(
    const UnitLowerer& unit_lowerer, hir::CovergroupId ref,
    std::uint32_t behavior) -> mir::VirtualSlot {
  return mir::VirtualSlot{
      .owner_class =
          unit_lowerer.CovergroupClassIdentity(FirstOf(unit_lowerer, ref)),
      .slot = mir::CallableId{behavior}};
}

// The dispatch role of a class of `ref`'s lineage for the behavior
// `behavior`: introduced by the first class, overridden by every other.
auto RoleIn(
    UnitLowerer& unit_lowerer, hir::CovergroupId ref, std::uint32_t behavior)
    -> mir::VirtualDispatchRole {
  if (!PublishedOf(unit_lowerer, ref).base.has_value()) {
    return mir::IntroducesVirtualSlot{};
  }
  return mir::OverridesVirtualSlot{
      .overridden = IntroducedSlot(unit_lowerer, ref, behavior)};
}

// A method of the class itself, named as a call names it: by position, which
// is the same in the class this unit writes and in its declaration of another
// unit's.
auto TypeMethod(
    UnitLowerer& unit_lowerer, hir::CovergroupId ref, mir::CallableId slot)
    -> mir::DirectTarget {
  return mir::CallableTarget{
      .owner = unit_lowerer.CovergroupClassIdentity(ref), .slot = slot};
}

// Whether a class of `ref` belongs to the instance of the scope declaring its
// covergroup (LRM 6.22): one a design element declares does, and so does each
// type-associated body of it.
auto TakesDeclaringInstance(
    const UnitLowerer& unit_lowerer, hir::CovergroupId ref) -> bool {
  return unit_lowerer.Hir().covergroups.IsOwn(ref) &&
         hir::RootsAnObject(unit_lowerer.Hir().role);
}

// The item `name` along `ref`'s lineage: the class declaring it, its
// property there, and the library class counting it.
struct LineageItem {
  CovergroupClass owner;
  std::uint32_t property;
  hir::ClassId library_class;
  bool is_cross = false;
};

auto ItemOf(
    const UnitLowerer& unit_lowerer, const hir::CoverageLibraryRefs& refs,
    hir::CovergroupId ref, std::string_view name) -> LineageItem {
  for (std::optional<hir::CovergroupId> at = ref; at.has_value();) {
    CovergroupClass cls = CovergroupClassOf(unit_lowerer, *at);
    const hir::PublishedCovergroup& published = *cls.published;
    for (std::size_t i = 0; i < published.coverpoints.size(); ++i) {
      if (published.coverpoints[i].name != name) continue;
      const std::uint32_t property = cls.layout.coverpoints[i];
      return LineageItem{
          .owner = std::move(cls),
          .property = property,
          .library_class = published.coverpoints[i].library_class,
          .is_cross = false};
    }
    for (std::size_t i = 0; i < published.crosses.size(); ++i) {
      if (published.crosses[i] != name) continue;
      const std::uint32_t property = cls.layout.crosses[i];
      return LineageItem{
          .owner = std::move(cls),
          .property = property,
          .library_class = refs.cross,
          .is_cross = true};
    }
    at = published.base;
  }
  throw InternalError(
      std::format(
          "no covergroup along the lineage declares the item '{}' -- please "
          "report this as a bug",
          name));
}

// The property of the first class of `ref`'s lineage holding the instance's
// record.
auto RecordOf(const UnitLowerer& unit_lowerer, hir::CovergroupId ref)
    -> std::pair<CovergroupClass, std::uint32_t> {
  CovergroupClass first =
      CovergroupClassOf(unit_lowerer, FirstOf(unit_lowerer, ref));
  const std::uint32_t record = *first.layout.record;
  return {std::move(first), record};
}

// The MIR a covergroup's lowering writes, into the block one frame is writing:
// the coverage library's methods and properties, the class's own, and the
// declaration's expressions, lowered by the lowering of the declaration's
// body.
class Writer {
 public:
  Writer(
      UnitLowerer& unit_lowerer, const CoverageLibrary& library,
      ProcessLowerer& lowerer, const hir::CovergroupDecl& decl, WalkFrame frame)
      : unit_lowerer_(&unit_lowerer),
        library_(&library),
        lowerer_(&lowerer),
        decl_(&decl),
        frame_(std::move(frame)) {
  }

  // The same writer, writing where `frame` is.
  [[nodiscard]] auto At(const WalkFrame& frame) const -> Writer {
    Writer at = *this;
    at.frame_ = frame;
    return at;
  }

  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  [[nodiscard]] auto Block() const -> mir::Block& {
    return *frame_.current_block;
  }
  [[nodiscard]] auto Unit() const -> mir::CompilationUnit& {
    return unit_lowerer_->Unit();
  }
  [[nodiscard]] auto Lowering() const -> UnitLowerer& {
    return *unit_lowerer_;
  }
  [[nodiscard]] auto Library() const -> const CoverageLibrary& {
    return *library_;
  }
  [[nodiscard]] auto Body() const -> ProcessLowerer& {
    return *lowerer_;
  }
  [[nodiscard]] auto Decl() const -> const hir::CovergroupDecl& {
    return *decl_;
  }

  // The value of the declaration's expression `id`.
  [[nodiscard]] auto Value(hir::ExprId id) const -> diag::Result<mir::ExprId> {
    auto lowered = lowerer_->LowerExpr(decl_->body.exprs.Get(id), frame_);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return Block().AsValue(
        Unit().types, Block().exprs.Add(*std::move(lowered)));
  }

  [[nodiscard]] auto Int(std::int64_t value) const -> mir::ExprId {
    return mir::BuildIntLiteral(Unit(), Block(), value);
  }
  [[nodiscard]] auto Bit(bool value) const -> mir::ExprId {
    return mir::BuildBit1Literal(Unit(), Block(), value);
  }
  [[nodiscard]] auto String(std::string text) const -> mir::ExprId {
    return BuildStringValueExpr(Unit(), Block(), std::move(text));
  }

  // The enumerator `name` the formal `position` of the library method
  // `method` of `cls` takes.
  [[nodiscard]] auto LibraryEnum(
      hir::ClassId cls, std::string_view method, std::size_t position,
      std::string_view name) const -> mir::ExprId {
    const hir::TypeId type = library_->MethodOf(cls, method)
                                 .prototype.interface.params.at(position)
                                 .type;
    return mir::BuildIntegralLiteral(
        Unit(), Block(), unit_lowerer_->TranslateType(type),
        LowerHirIntegralConstant(library_->EnumeratorOf(type, name)));
  }

  void Do(mir::ExprId expr) const {
    Block().AppendStmt(mir::ExprStmt{.expr = expr});
  }
  void Do(mir::Expr expr) const {
    Do(Block().exprs.Add(std::move(expr)));
  }

  // `name` of the library class `cls`, called on what the handle `receiver`
  // holds, or on nothing for a type-associated one.
  [[nodiscard]] auto Call(
      std::optional<mir::ExprId> receiver, hir::ClassId cls,
      std::string_view name, std::vector<mir::ExprId> arguments = {}) const
      -> mir::ExprId {
    const CoverageLibrary::Method method = library_->MethodOf(cls, name);
    return BuildMethodCall(
        *unit_lowerer_, frame_, method.callee, receiver, arguments,
        method.prototype.result_type);
  }

  // The property `name` of the library class `cls` of the object the handle
  // `handle` holds.
  [[nodiscard]] auto LibraryProperty(
      mir::ExprId handle, hir::ClassId cls, std::string_view name) const
      -> AccessPath {
    const CoverageLibrary::Property property = library_->PropertyOf(cls, name);
    return ObjectPropertyPath(
        handle, LibraryField(*unit_lowerer_, property),
        unit_lowerer_->TranslateType(property.type));
  }

  // `property` of the object `object` reaches, which is the handle holding it
  // or the running body's own object.
  [[nodiscard]] auto ObjectPropertyPath(
      mir::ExprId object, const mir::ClassFieldTarget& property,
      mir::TypeId type) const -> AccessPath {
    return AccessPath{
        .owner =
            ObjectProperty{
                .object = Block().AsValue(Unit().types, object),
                .property = property,
                .type = type},
        .descent = {}};
  }

  // The property at `index` of the covergroup class `cls`, of the running
  // body's own object.
  [[nodiscard]] auto Own(const CovergroupClass& cls, std::uint32_t index) const
      -> AccessPath {
    return ObjectPropertyPath(
        Block().exprs.Add(MakeSelfRefExpr(frame_)),
        CovergroupPropertyTarget(*unit_lowerer_, cls, index),
        CovergroupPropertyType(*unit_lowerer_, cls, index));
  }

  // The member `name` of the unpacked structure `path` names, whose type in
  // the library is `type`.
  [[nodiscard]] auto Member(
      AccessPath path, hir::TypeId type, std::string_view name) const
      -> AccessPath {
    return OptionMember(*unit_lowerer_, std::move(path), type, name);
  }

  [[nodiscard]] auto Read(const AccessPath& path) const -> mir::ExprId {
    return PathValue(Unit(), Block(), path);
  }

  void Store(const AccessPath& path, mir::ExprId value) const {
    const mir::TypeId type = PathValueType(Unit(), Block(), path);
    Do(BuildStoreExpr(
        Unit(), Block(), path,
        ConvertToType(
            Unit(), Block(), Block().AsValue(Unit().types, value), type)));
  }

  // `if (condition) then`, `condition` being a value read as a condition is.
  void If(mir::ExprId condition, const std::function<void(const Writer&)>& then)
      const {
    const mir::ExprId reduced = ReduceToCondition(
        Unit(), Block(), Block().AsValue(Unit().types, condition));
    mir::Block then_block;
    then(At(frame_.WithBlock(&then_block)));
    Block().AppendIfThen(reduced, std::move(then_block));
  }

  void IfElse(
      mir::ExprId condition, const std::function<void(const Writer&)>& then,
      const std::function<void(const Writer&)>& otherwise) const {
    const mir::ExprId reduced = ReduceToCondition(
        Unit(), Block(), Block().AsValue(Unit().types, condition));
    mir::Block then_block;
    then(At(frame_.WithBlock(&then_block)));
    mir::Block else_block;
    otherwise(At(frame_.WithBlock(&else_block)));
    Block().AppendStmt(
        mir::IfStmt{
            .condition = reduced,
            .then_scope = Block().child_scopes.Add(std::move(then_block)),
            .else_scope = Block().child_scopes.Add(std::move(else_block))});
  }

  // Runs `visit` on each element of the array `array` holds, in order, the
  // array evaluated once, here.
  [[nodiscard]] auto ForEach(
      mir::ExprId array,
      const std::function<diag::Result<void>(const Writer&, mir::ExprId)>&
          visit) const -> diag::Result<void> {
    const mir::ExprId value = Block().AsValue(Unit().types, array);
    const mir::TypeId type = Block().exprs.Get(value).type;
    const mir::LocalId held = DeclareLocal(frame_, value);
    const WalkedArray walked = WalkedArrayOf(
        *unit_lowerer_, frame_, Block().LocalPlace(held, type), std::nullopt);
    auto loop = BuildEntryLoopStmt(
        *unit_lowerer_, frame_, walked, WalkOrder::kFromFirst, std::nullopt,
        [&](const WalkFrame& at,
            const VisitedEntry& entry) -> diag::Result<void> {
          const Writer inside = At(at);
          return visit(
              inside, inside.Block().AsValue(Unit().types, entry.element));
        });
    if (!loop) return std::unexpected(std::move(loop.error()));
    Block().AppendStmt(*std::move(loop));
    return {};
  }

  // Binds the declaration's variable `var` to a local of this body holding
  // `value`, which its expressions read it as.
  void Bind(
      hir::ProceduralVarId var, mir::TypeId type, mir::ExprId value) const {
    const hir::ProceduralVarDecl& declared =
        decl_->body.procedural_vars.Get(var);
    const mir::LocalId local = frame_.bindings->DeclareProcedural(
        ProceduralOrigin{.var = var}, declared.name, type);
    Block().AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
    lowerer_->MapProceduralVar(var, AutomaticVarBinding{.type = type});
  }

 private:
  UnitLowerer* unit_lowerer_;
  const CoverageLibrary* library_;
  ProcessLowerer* lowerer_;
  const hir::CovergroupDecl* decl_;
  WalkFrame frame_;
};

// A new object of the library class `cls`, made with `values`, each brought
// to the type its constructor's formal takes it as.
auto NewLibraryObject(
    const Writer& e, hir::ClassId cls, const std::vector<mir::ExprId>& values)
    -> mir::ExprId {
  mir::CompilationUnit& unit = e.Unit();
  mir::Block& block = e.Block();
  const std::vector<hir::TypeId> types = e.Library().ConstructorParamTypes(cls);
  std::vector<mir::ExprId> arguments;
  // A class of this unit's, specialized on a type of an instance, is a type of
  // that instance (LRM 6.22), which its construction is handed first.
  if (e.Library().TakesDeclaringInstance(cls)) {
    arguments.push_back(BuildImplicitInstanceArgument(
        e.Body(), e.Frame(),
        hir::DeclaringInstanceReach{hir::StructuralHops{0}}));
  }
  for (std::size_t i = 0; i < values.size(); ++i) {
    arguments.push_back(ConvertToType(
        unit, block, block.AsValue(unit.types, values[i]),
        e.Lowering().TranslateType(types.at(i))));
  }
  arguments.push_back(BuildReportHandedOn(unit, e.Frame()));
  const mir::TypeId handle = HandleTo(e.Lowering(), cls);
  return block.exprs.Add(
      mir::MakeNewExpr(
          block.exprs.Add(
              mir::MakeObjectConstructionExpr(
                  *mir::AllocatedType(unit.types, handle),
                  std::move(arguments))),
          handle));
}

// `entry` handed `closure`, a closure kept for the runtime, which runs it
// where the entry places it: an execution of its own that may wait, or after
// the final procedures (LRM 19.3, 19.9).
void HandToRuntime(
    const Writer& e, support::BuiltinFn entry, mir::ExprId closure) {
  const mir::ExprId runtime =
      e.Block().exprs.Add(BuildCurrentRuntimeCallExpr(e.Lowering()));
  e.Do(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry, .receiver = runtime},
                  .arguments = {closure}},
          .type = e.Unit().builtins.void_type});
}

}  // namespace

auto CovergroupClassOf(
    const UnitLowerer& unit_lowerer, hir::CovergroupId covergroup)
    -> CovergroupClass {
  const hir::PublishedCovergroup& published =
      PublishedOf(unit_lowerer, covergroup);
  return CovergroupClass{
      .covergroup = covergroup,
      .published = &published,
      .layout = CovergroupLayoutOf(published)};
}

auto CovergroupPropertyTarget(
    UnitLowerer& unit_lowerer, const CovergroupClass& cls, std::uint32_t index)
    -> mir::ClassFieldTarget {
  // A covergroup's class places its properties in the order its layout gives
  // them, wherever it is written, so the position counted out of the layout
  // is the slot.
  return mir::ClassFieldTarget{
      .owner = unit_lowerer.CovergroupClassIdentity(cls.covergroup),
      .slot = mir::FieldId{index}};
}

auto CovergroupPropertyType(
    UnitLowerer& unit_lowerer, const CovergroupClass& cls, std::uint32_t index)
    -> mir::TypeId {
  const hir::PublishedCovergroup& published = *cls.published;
  const hir::CoverageLibraryRefs& refs = *unit_lowerer.Hir().coverage_library;
  return std::visit(
      Overloaded{
          [&](const EmbeddingProperty&) {
            return HandleTo(unit_lowerer, *published.embedding);
          },
          [&](const FormalProperty& formal) {
            const hir::PublishedCovergroupFormal& declared =
                published.formals.at(formal.formal);
            const mir::TypeId value = unit_lowerer.TranslateType(declared.type);
            // What a `ref` argument names is held as a reference that only
            // reads it (LRM 19.3).
            const bool reference =
                declared.direction == hir::ParamDirection::kRef ||
                declared.direction == hir::ParamDirection::kConstRef;
            return reference
                       ? unit_lowerer.Unit().types.Intern(
                             mir::Type{mir::ReferenceWrapperType{
                                 .pointee = value,
                                 .mutability = mir::Mutability::kReadOnly}})
                       : value;
          },
          [&](const RecordProperty&) {
            return HandleTo(unit_lowerer, refs.group_instance);
          },
          [&](const CoverpointProperty& coverpoint) {
            return HandleTo(
                unit_lowerer,
                published.coverpoints.at(coverpoint.coverpoint).library_class);
          },
          [&](const CrossProperty&) {
            return HandleTo(unit_lowerer, refs.cross);
          }},
      cls.layout.properties.at(index).role);
}

auto PublishedCovergroupClass(
    UnitLowerer& unit_lowerer, hir::CovergroupId published) -> mir::Class {
  const hir::CovergroupKey& key =
      unit_lowerer.Hir().covergroups.KeyOf(published);
  const CovergroupClass cls = CovergroupClassOf(unit_lowerer, published);
  const hir::PublishedCovergroup& stated = *cls.published;
  mir::Class declared = unit_lowerer.IncompleteClass(
      unit_lowerer.CovergroupClassIdentity(published), key.unit_name, key.path);
  declared.base =
      stated.base.has_value()
          ? mir::ClassRef{unit_lowerer.CovergroupClassIdentity(*stated.base)}
          : mir::ClassRef{mir::ManagedObjectRootRef{}};
  for (std::size_t i = 0; i < cls.layout.properties.size(); ++i) {
    const auto index = static_cast<std::uint32_t>(i);
    declared.named_fields.push_back(
        mir::NamedField{
            .name = cls.layout.properties[i].name,
            .slot = declared.fields.Add(
                mir::FieldDecl{
                    .type =
                        CovergroupPropertyType(unit_lowerer, cls, index)})});
  }
  // `sample` and the record of the object's type, in that order: introduced by
  // the first class of a lineage and overridden by every other, whose first
  // covergroup may be this unit's own where the lineage another unit published
  // passes back through it.
  for (const auto& [name, behavior] :
       {std::pair{kCovergroupSample, kSampleBehavior},
        std::pair{kCovergroupMakeRecord, kMakeRecordBehavior}}) {
    const mir::CallableId callable = declared.callables.Add(
        mir::CallableDecl{
            .code = {},
            .foreign = std::nullopt,
            .virtual_dispatch =
                stated.base.has_value()
                    ? mir::VirtualDispatchRole{mir::OverridesVirtualSlot{
                          .overridden = IntroducedSlot(
                              unit_lowerer, published, behavior)}}
                    : mir::VirtualDispatchRole{mir::IntroducesVirtualSlot{}},
            .defining_unit = std::nullopt});
    declared.named_callables.push_back(
        mir::NamedCallable{.name = std::string{name}, .body = callable});
  }
  // Then the two methods of the class itself, which dispatch on nothing.
  for (const std::string_view name :
       {kCovergroupTypeRecord, kCovergroupDeclareType}) {
    declared.named_callables.push_back(
        mir::NamedCallable{
            .name = std::string{name},
            .body = declared.callables.Add(
                mir::CallableDecl{
                    .code = {},
                    .foreign = std::nullopt,
                    .virtual_dispatch = std::nullopt,
                    .defining_unit = std::nullopt})});
  }
  return declared;
}

namespace {

// Writes the bodies of one covergroup's class.
class ClassWriter {
 public:
  ClassWriter(
      UnitLowerer& unit_lowerer, hir::CovergroupId covergroup,
      const hir::CovergroupDecl& decl,
      const StructuralScopeLowerer* declaring_scope,
      const DeclaredScopes& scopes, mir::Class& cls, mir::ClassId class_id,
      WalkFrame owner, const std::optional<StaticStorageHome>& type_event)
      : u_(&unit_lowerer),
        decl_(&decl),
        declaring_scope_(declaring_scope),
        scopes_(&scopes),
        cls_(&cls),
        class_id_(class_id),
        owner_(std::move(owner)),
        type_event_(&type_event),
        library_(unit_lowerer.Hir()),
        own_(CovergroupClassOf(unit_lowerer, covergroup)),
        first_(CovergroupClassOf(
            unit_lowerer, FirstOf(unit_lowerer, covergroup))) {
  }

  auto Write() -> diag::Result<void> {
    for (const auto& [slot, name] :
         {std::pair{kSample, kCovergroupSample},
          std::pair{kMakeRecord, kCovergroupMakeRecord},
          std::pair{kTypeRecord, kCovergroupTypeRecord},
          std::pair{kDeclareType, kCovergroupDeclareType}}) {
      cls_->named_callables.push_back(
          mir::NamedCallable{.name = std::string{name}, .body = slot});
    }
    if (auto r = WriteConstructor(); !r) return r;
    if (auto r = WriteSample(); !r) return r;
    WriteMakeRecord();
    WriteTypeRecord();
    if (auto r = WriteDeclareType(); !r) return r;
    return {};
  }

 private:
  [[nodiscard]] auto Unit() const -> mir::CompilationUnit& {
    return u_->Unit();
  }

  [[nodiscard]] auto Refs() const -> const hir::CoverageLibraryRefs& {
    return library_.Refs();
  }

  [[nodiscard]] auto Self() const -> hir::CovergroupId {
    return own_.covergroup;
  }

  [[nodiscard]] auto Shape() const -> const ClassShape& {
    return u_->GetClassShape(class_id_);
  }

  [[nodiscard]] auto Lowerer() const -> ProcessLowerer {
    return {
        *u_,
        declaring_scope_,
        declaring_scope_ != nullptr
            ? declaring_scope_->HirScope().time_resolution
            : u_->Hir().root_scope.time_resolution,
        decl_->body,
        std::nullopt,
        owner_,
        *scopes_,
        {}};
  }

  // The name the run's registry knows the covergroup's type by (LRM 19.11.3).
  [[nodiscard]] auto TypeName() const -> std::string {
    const std::string& home = u_->Hir().source_name;
    return std::format(
        "{}::{}", home.empty() ? std::string{"$unit"} : home,
        support::DisplayOf(own_.published->path));
  }

  // The record of the instance, kept by the first class of the lineage.
  [[nodiscard]] auto Record(const Writer& e) const -> AccessPath {
    return e.Own(first_, *first_.layout.record);
  }

  // The item `name` of the lineage, of the running object.
  [[nodiscard]] auto Item(const Writer& e, std::string_view name) const
      -> std::pair<AccessPath, LineageItem> {
    LineageItem item = ItemOf(*u_, Refs(), Self(), name);
    AccessPath path = e.Own(item.owner, item.property);
    return {std::move(path), std::move(item)};
  }

  // The instance a body of the class reaches its declaring scope through, as
  // a call of a type-associated body of a class of an instance is handed it
  // (LRM 6.22).
  [[nodiscard]] auto InstanceArgument(const Writer& e) const
      -> std::vector<mir::ExprId> {
    if (!Shape().declaring_instance.has_value()) return {};
    return {BuildImplicitInstanceArgument(
        e.Body(), e.Frame(),
        hir::DeclaringInstanceReach{hir::StructuralHops{0}})};
  }

  // The record of the type of the covergroup `of`, as its class answers it.
  [[nodiscard]] auto TypeRecordOf(const Writer& e, hir::CovergroupId of) const
      -> mir::ExprId {
    std::vector<mir::ExprId> arguments;
    if (TakesDeclaringInstance(*u_, of)) arguments = InstanceArgument(e);
    return BuildFunctionCall(
        Unit(), e.Frame(),
        mir::Direct{.target = TypeMethod(*u_, of, kTypeRecord)},
        std::move(arguments), TypeRecordType(*u_, library_));
  }

  // A virtual method of the lineage, the first class's `behavior`, called on
  // the running object.
  [[nodiscard]] auto CallVirtual(
      const Writer& e, std::uint32_t behavior,
      std::vector<mir::ExprId> arguments,
      std::optional<mir::TypeId> result) const -> mir::ExprId {
    mir::Block& block = e.Block();
    const mir::VirtualSlot slot = IntroducedSlot(*u_, Self(), behavior);
    const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(e.Frame()));
    return BuildFunctionCall(
        Unit(), e.Frame(),
        mir::Virtual{
            .receiver = BuildObjectDeref(
                Unit(), block, AsIntroducer(Unit().types, block, self, slot)),
            .slot = slot},
        std::move(arguments), result);
  }

  // Binds what the covergroup's expressions name of the object: each
  // argument, kept by the first class of the lineage, and the object embedding
  // the covergroup. An input is read from its property, and a `ref` one is a
  // reference to what its property names, so a wait watches that storage as
  // it watches any `ref` formal (LRM 9.4.2). Nothing a covergroup evaluates
  // assigns either, so what is read here is what the property holds.
  void BindKept(const Writer& e) const {
    mir::CompilationUnit& unit = Unit();
    mir::Block& block = e.Block();
    for (std::size_t i = 0; i < decl_->formals.size(); ++i) {
      const hir::SubroutineParam& formal = decl_->formals[i];
      const std::uint32_t index = first_.layout.formals.at(i);
      const mir::TypeId value =
          u_->TranslateType(decl_->body.procedural_vars.Get(formal.var).type);
      if (formal.direction == hir::ParamDirection::kRef ||
          formal.direction == hir::ParamDirection::kConstRef) {
        const mir::TypeId type = *ParamTypeOf(unit, value, formal.direction);
        const mir::ExprId held = block.exprs.Add(PropertyStorage(
            unit, block, block.exprs.Add(MakeSelfRefExpr(e.Frame())),
            CovergroupPropertyTarget(*u_, first_, index),
            CovergroupPropertyType(*u_, first_, index)));
        const mir::ExprId reference = AtType(
            unit, block,
            PathReference(
                unit, block, AccessPath{.owner = held, .descent = {}}),
            type);
        e.Bind(formal.var, type, reference);
        continue;
      }
      e.Bind(formal.var, value, e.Read(e.Own(first_, index)));
    }
    if (decl_->embedding.has_value()) {
      const std::uint32_t index = *own_.layout.embedding;
      e.Bind(
          *decl_->embedding, CovergroupPropertyType(*u_, own_, index),
          e.Read(e.Own(own_, index)));
    }
  }

  // The option assignments `settings` of one kind, each into the structure
  // `options` names, whose type in the library is `type` (LRM 19.7).
  static auto WriteOptions(
      const Writer& e, std::span<const hir::OptionSetting> settings,
      bool type_options, const AccessPath& options, hir::TypeId type)
      -> diag::Result<void> {
    for (const hir::OptionSetting& setting : settings) {
      if (setting.type_option != type_options) continue;
      auto value = e.Value(setting.value);
      if (!value) return std::unexpected(std::move(value.error()));
      e.Store(e.Member(options, type, setting.field), *value);
    }
    return {};
  }

  // The `option` of the item or record `keeper` holds, of the library class
  // `cls`.
  [[nodiscard]] auto OptionsOf(
      const Writer& e, mir::ExprId keeper, hir::ClassId cls) const
      -> std::pair<AccessPath, hir::TypeId> {
    return {
        e.LibraryProperty(keeper, cls, "option"),
        library_.PropertyOf(cls, "option").type};
  }

  // One element of a bin's range list (LRM 19.5.1), handed to the library as
  // its value, its range or its tolerance.
  static auto WriteElement(
      const Writer& e, mir::ExprId item, hir::ClassId cls, bool wildcard,
      const hir::BinElement& element) -> diag::Result<void> {
    return std::visit(
        Overloaded{
            [&](const hir::BinValue& value) -> diag::Result<void> {
              auto v = e.Value(value.value);
              if (!v) return std::unexpected(std::move(v.error()));
              e.Do(e.Call(
                  item, cls, wildcard ? "add_wildcard" : "add_value", {*v}));
              return {};
            },
            [&](const hir::BinRange& range) -> diag::Result<void> {
              if (!range.low.has_value() && !range.high.has_value()) {
                e.Do(e.Call(item, cls, "add_every_value"));
                return {};
              }
              std::optional<mir::ExprId> low;
              std::optional<mir::ExprId> high;
              if (range.low.has_value()) {
                auto v = e.Value(*range.low);
                if (!v) return std::unexpected(std::move(v.error()));
                low = *v;
              }
              if (range.high.has_value()) {
                auto v = e.Value(*range.high);
                if (!v) return std::unexpected(std::move(v.error()));
                high = *v;
              }
              if (!low.has_value()) {
                e.Do(e.Call(item, cls, "add_range_from_least", {*high}));
              } else if (!high.has_value()) {
                e.Do(e.Call(item, cls, "add_range_to_greatest", {*low}));
              } else {
                e.Do(e.Call(item, cls, "add_range", {*low, *high}));
              }
              return {};
            },
            [&](const hir::BinTolerance& tolerance) -> diag::Result<void> {
              auto center = e.Value(tolerance.center);
              if (!center) return std::unexpected(std::move(center.error()));
              auto spread = e.Value(tolerance.tolerance);
              if (!spread) return std::unexpected(std::move(spread.error()));
              e.Do(e.Call(
                  item, cls, "add_tolerance",
                  {*center, *spread, e.Bit(tolerance.in_percent)}));
              return {};
            }},
        element);
  }

  // The bins one construct of a coverpoint makes, numbered `construct` (LRM
  // 19.5.1, 19.5.2).
  [[nodiscard]] auto WriteBins(
      const Writer& e, std::string_view point, hir::ClassId cls,
      const hir::BinConstruct& bin, std::int64_t construct) const
      -> diag::Result<void> {
    const auto item = [&](const Writer& at) {
      return at.Read(Item(at, point).first);
    };
    const mir::ExprId number = e.Int(construct);
    const mir::ExprId name = e.String(bin.name);
    return std::visit(
        Overloaded{
            [&](const hir::DefaultSequenceBins&) -> diag::Result<void> {
              e.Do(e.Call(
                  item(e), cls, "add_default_sequence_bin", {number, name}));
              return {};
            },
            [&](const hir::DefaultBins& bins) -> diag::Result<void> {
              e.Do(e.Call(
                  item(e), cls, "add_default_bin",
                  {number, name, e.Bit(bins.per_value)}));
              return {};
            },
            [&](const hir::TransitionBins& bins) -> diag::Result<void> {
              e.Do(e.Call(
                  item(e), cls, "begin_transition_bins",
                  {number, name,
                   e.LibraryEnum(
                       cls, "begin_transition_bins", 2,
                       LibraryKindOf(bin.kind)),
                   e.Bit(bins.per_sequence)}));
              for (const std::vector<hir::TransitionStep>& sequence :
                   bins.sequences) {
                e.Do(e.Call(item(e), cls, "begin_sequence"));
                for (const hir::TransitionStep& step : sequence) {
                  e.Do(e.Call(item(e), cls, "begin_step"));
                  for (const hir::BinElement& value : step.values) {
                    if (auto r =
                            WriteElement(e, item(e), cls, bins.wildcard, value);
                        !r) {
                      return r;
                    }
                  }
                  // A step written with no count is taken once, and one with
                  // a single count is taken exactly that many times, the
                  // count evaluated once (LRM 19.5.2).
                  mir::ExprId least = e.Int(1);
                  if (step.least.has_value()) {
                    auto written = e.Value(*step.least);
                    if (!written) {
                      return std::unexpected(std::move(written.error()));
                    }
                    least = *written;
                  }
                  const mir::TypeId count = e.Block().exprs.Get(least).type;
                  const mir::LocalId kept = DeclareLocal(e.Frame(), least);
                  mir::ExprId most = e.Block().ReadLocal(kept, count);
                  if (step.most.has_value()) {
                    auto written = e.Value(*step.most);
                    if (!written) {
                      return std::unexpected(std::move(written.error()));
                    }
                    most = *written;
                  }
                  e.Do(e.Call(
                      item(e), cls, "end_step",
                      {e.LibraryEnum(
                           cls, "end_step", 0,
                           LibraryRepetitionOf(step.repetition)),
                       e.Block().ReadLocal(kept, count), most}));
                }
                e.Do(e.Call(item(e), cls, "end_sequence"));
              }
              e.Do(e.Call(item(e), cls, "end_transition_bins"));
              return {};
            },
            [&](const hir::ValueBins& bins) -> diag::Result<void> {
              mir::ExprId count = e.Int(0);
              if (const auto* fixed =
                      std::get_if<hir::FixedBins>(&bins.array)) {
                auto written = e.Value(fixed->count);
                if (!written) {
                  return std::unexpected(std::move(written.error()));
                }
                count = *written;
              }
              e.Do(e.Call(
                  item(e), cls, "begin_bins",
                  {number, name,
                   e.LibraryEnum(cls, "begin_bins", 2, LibraryKindOf(bin.kind)),
                   e.LibraryEnum(
                       cls, "begin_bins", 3, LibraryArrayOf(bins.array)),
                   count}));
              for (const hir::BinElement& element : bins.elements) {
                if (auto r =
                        WriteElement(e, item(e), cls, bins.wildcard, element);
                    !r) {
                  return r;
                }
              }
              // A set expression is an array of values evaluated as the
              // instance is made, each added as a value of a range list is
              // (LRM 19.5.1.2).
              if (bins.set.has_value()) {
                auto set = e.Value(*bins.set);
                if (!set) return std::unexpected(std::move(set.error()));
                auto added = e.ForEach(
                    *set,
                    [&](const Writer& at,
                        mir::ExprId value) -> diag::Result<void> {
                      at.Do(at.Call(
                          item(at), cls,
                          bins.wildcard ? "add_wildcard" : "add_value",
                          {value}));
                      return {};
                    });
                if (!added) return added;
              }
              // A bin naming the coverpoint alone stands for all its values
              // (LRM 19.5.1.1).
              if (bins.elements.empty() && !bins.set.has_value()) {
                e.Do(e.Call(item(e), cls, "add_every_value"));
              }
              // A `with` filter keeps those of the values given that it holds
              // for, each value being the filter's `item` in turn (LRM
              // 19.5.1.1).
              if (bins.filter.has_value()) {
                const hir::BinFilter& filter = *bins.filter;
                const mir::TypeId item_type = u_->TranslateType(
                    decl_->body.procedural_vars.Get(filter.item).type);
                auto kept = e.ForEach(
                    e.Call(item(e), cls, "pending_values"),
                    [&](const Writer& at,
                        mir::ExprId value) -> diag::Result<void> {
                      const mir::LocalId held = DeclareLocal(
                          at.Frame(),
                          ConvertToType(Unit(), at.Block(), value, item_type));
                      at.Bind(
                          filter.item, item_type,
                          at.Block().ReadLocal(held, item_type));
                      auto keeps = at.Value(filter.keeps);
                      if (!keeps) {
                        return std::unexpected(std::move(keeps.error()));
                      }
                      at.Do(at.Call(
                          item(at), cls, "keep_value",
                          {at.Block().ReadLocal(held, item_type), *keeps}));
                      return {};
                    });
                if (!kept) return kept;
              }
              e.Do(e.Call(item(e), cls, "end_bins"));
              return {};
            }},
        bin.form);
  }

  // The least number of a candidate tuple's value tuples a `with` filter or a
  // set expression has to hold for it to be kept (LRM 19.6.1.2): one unless
  // `matches` says otherwise, and every one, which the library takes as a
  // negative number, for `matches $`.
  static auto LeastMatches(const Writer& e, const hir::MatchesRequired& matches)
      -> diag::Result<mir::ExprId> {
    return std::visit(
        Overloaded{
            [&](const hir::OneMatch&) -> diag::Result<mir::ExprId> {
              return e.Int(1);
            },
            [&](const hir::EveryMatch&) -> diag::Result<mir::ExprId> {
              return e.Int(-1);
            },
            [&](hir::ExprId written) -> diag::Result<mir::ExprId> {
              return e.Value(written);
            }},
        matches);
  }

  // The tuples a cross's select expression names (LRM 19.6.1), as a
  // selection of the cross's.
  [[nodiscard]] auto WriteSelection(
      const Writer& e, const hir::Cross& cross, hir::CrossSelectId id) const
      -> diag::Result<mir::ExprId> {
    hir::ClassId cross_class = Refs().cross;
    const auto item = [&](const Writer& at) {
      return at.Read(Item(at, cross.name).first);
    };
    // The coverpoint the cross's dimension `d` is, of the running object.
    const auto point = [&](const Writer& at, std::string_view name) {
      auto [path, found] = Item(at, name);
      return std::pair{at.Read(path), found.library_class};
    };
    return std::visit(
        Overloaded{
            [&](const hir::SelectAll&) -> diag::Result<mir::ExprId> {
              return e.Call(item(e), cross_class, "select_all");
            },
            [&](const hir::SelectBinsOf& condition)
                -> diag::Result<mir::ExprId> {
              const auto [handle, library] = point(e, condition.point);
              mir::ExprId numbers =
                  condition.bin.has_value()
                      ? e.Call(
                            handle, library, "numbers_named",
                            {e.String(*condition.bin)})
                      : e.Call(handle, library, "all_numbers");
              if (!condition.intersect.empty()) {
                e.Do(e.Call(handle, library, "begin_selection"));
                for (const hir::BinElement& element : condition.intersect) {
                  if (auto r = WriteElement(e, handle, library, false, element);
                      !r) {
                    return std::unexpected(std::move(r.error()));
                  }
                }
                numbers = e.Call(
                    std::nullopt, cross_class, "both_numbers",
                    {numbers, e.Call(handle, library, "selected_bins")});
              }
              std::int64_t dimension = 0;
              while (cross.targets.at(dimension) != condition.point) {
                ++dimension;
              }
              return e.Call(
                  item(e), cross_class, "select_numbers",
                  {e.Int(dimension), numbers});
            },
            [&](const hir::SelectNot& negated) -> diag::Result<mir::ExprId> {
              auto operand = WriteSelection(e, cross, negated.operand);
              if (!operand) return operand;
              return e.Call(item(e), cross_class, "select_outside", {*operand});
            },
            [&](const hir::SelectAnd& both) -> diag::Result<mir::ExprId> {
              auto left = WriteSelection(e, cross, both.left);
              if (!left) return left;
              auto right = WriteSelection(e, cross, both.right);
              if (!right) return right;
              return e.Call(*left, Refs().selection, "both", {*right});
            },
            [&](const hir::SelectOr& either) -> diag::Result<mir::ExprId> {
              auto left = WriteSelection(e, cross, either.left);
              if (!left) return left;
              auto right = WriteSelection(e, cross, either.right);
              if (!right) return right;
              return e.Call(*left, Refs().selection, "either", {*right});
            },
            [&](const hir::SelectWith& with) -> diag::Result<mir::ExprId> {
              // Evaluated as the instance is made over every value tuple of
              // every tuple the subordinate expression selects, each answer
              // handed to the cross (LRM 19.6.1.2).
              auto candidates = WriteSelection(e, cross, with.candidates);
              if (!candidates) return candidates;
              e.Do(e.Call(item(e), cross_class, "begin_filter", {*candidates}));
              auto offered = e.ForEach(
                  e.Call(item(e), cross_class, "candidate_tuples"),
                  [&](const Writer& at,
                      mir::ExprId candidate) -> diag::Result<void> {
                    const mir::TypeId tuple_type =
                        at.Block().exprs.Get(candidate).type;
                    const mir::LocalId tuple =
                        at.Frame().bindings->DeclareAnonymous(tuple_type);
                    at.Block().AppendStmt(
                        mir::LocalDeclStmt{.target = tuple, .init = candidate});
                    std::function<diag::Result<void>(
                        const Writer&, std::size_t)>
                        over = [&](const Writer& in,
                                   std::size_t d) -> diag::Result<void> {
                      if (d == cross.targets.size()) {
                        auto filter = in.Value(with.filter);
                        if (!filter) {
                          return std::unexpected(std::move(filter.error()));
                        }
                        in.Do(in.Call(
                            item(in), cross_class, "offer",
                            {in.Block().ReadLocal(tuple, tuple_type),
                             *filter}));
                        return {};
                      }
                      const auto [handle, library] =
                          point(in, cross.targets[d]);
                      const mir::ExprId bin = in.Call(
                          item(in), cross_class, "component",
                          {in.Block().ReadLocal(tuple, tuple_type),
                           in.Int(static_cast<std::int64_t>(d))});
                      return in.ForEach(
                          in.Call(handle, library, "values_in_bin", {bin}),
                          [&](const Writer& inner,
                              mir::ExprId value) -> diag::Result<void> {
                            if (const auto& var = with.values.at(d)) {
                              const mir::TypeId type = u_->TranslateType(
                                  decl_->body.procedural_vars.Get(*var).type);
                              inner.Bind(
                                  *var, type,
                                  ConvertToType(
                                      Unit(), inner.Block(), value, type));
                            }
                            return over(inner, d + 1);
                          });
                    };
                    return over(at, 0);
                  });
              if (!offered) return std::unexpected(std::move(offered.error()));
              auto least = LeastMatches(e, with.matches);
              if (!least) return least;
              return e.Call(item(e), cross_class, "end_filter", {*least});
            },
            [&](const hir::SelectSet& set) -> diag::Result<mir::ExprId> {
              // A queue of value tuples, evaluated as the instance is made,
              // each counted toward every tuple whose bins hold it (LRM
              // 19.6.1.4).
              e.Do(e.Call(
                  item(e), cross_class, "begin_filter",
                  {e.Call(item(e), cross_class, "select_all")}));
              auto tuples = e.Value(set.tuples);
              if (!tuples) return tuples;
              auto counted = e.ForEach(
                  *tuples,
                  [&](const Writer& at,
                      mir::ExprId value) -> diag::Result<void> {
                    const mir::TypeId tuple_type =
                        at.Block().exprs.Get(value).type;
                    const mir::LocalId tuple =
                        at.Frame().bindings->DeclareAnonymous(tuple_type);
                    at.Block().AppendStmt(
                        mir::LocalDeclStmt{.target = tuple, .init = value});
                    const std::span<const mir::TypeId> fields =
                        mir::RecordElements(
                            Unit(),
                            *mir::RecordOf(Unit().types.Get(tuple_type)));
                    for (std::size_t d = 0; d < cross.targets.size(); ++d) {
                      const auto [handle, library] =
                          point(at, cross.targets[d]);
                      const auto field = [&] {
                        return at.Read(
                            AccessPath{
                                .owner =
                                    at.Block().LocalPlace(tuple, tuple_type),
                                .descent = {DescentStep{
                                    .to =
                                        StepToComponent{
                                            .position =
                                                base::ComponentIndex{
                                                    static_cast<std::uint32_t>(
                                                        d)}},
                                    .part_type = fields[d]}}});
                      };
                      at.Do(at.Call(
                          item(at), cross_class, "value_component",
                          {at.Call(
                               handle, library, "numbers_containing",
                               {field()}),
                           at.Call(handle, library, "text_of", {field()})}));
                    }
                    at.Do(at.Call(item(at), cross_class, "end_value_tuple"));
                    return {};
                  });
              if (!counted) return std::unexpected(std::move(counted.error()));
              auto least = LeastMatches(e, set.matches);
              if (!least) return least;
              return e.Call(item(e), cross_class, "end_filter", {*least});
            }},
        cross.selects.Get(id));
  }

  // The constructor (LRM 19.3): what `new` is given, the instance's record and
  // options, and each coverpoint's and cross's item with its bins, every
  // expression evaluated as the instance is made. A covergroup extending
  // another constructs the base's part first, stating which of the base's
  // items it declares again, which the base builds but does not count (LRM
  // 19.4.1). The first class of the lineage makes the record of the object's
  // own type, which a virtual method answers even while the base is
  // constructed (LRM 8.17), and starts the sampling.
  auto WriteConstructor() -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    mir::CallableCode code = mir::CallableCode::Defined();
    CallableBindings bindings(unit, code);
    BoundImplicitParameters bound = BindImplicitParameters(
        *u_,
        owner_.WithBlock(&code.Body())
            .WithBindings(&bindings)
            .WithBodyCanWait(false),
        Shape(), CallableForm::kConstructor);
    std::vector<mir::LocalId> params = std::move(bound.params);
    mir::Block& block = code.Body();
    ProcessLowerer lowerer = Lowerer();

    // The instance the object belongs to, recorded before anything else, since
    // a method the base's construction dispatches to may read it.
    if (const std::optional<DeclaringInstance>& instance =
            Shape().declaring_instance) {
      const mir::ExprId value = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CastExpr{
                      .operand = BuildEnclosingScopeReceiver(
                          bound.frame, unit, mir::EnclosingHops{})},
              .type = instance->type});
      const mir::ExprId target = block.exprs.Add(
          mir::MakeFieldAccessExpr(
              BuildObjectDeref(
                  unit, block, block.exprs.Add(MakeSelfRefExpr(bound.frame))),
              mir::ClassFieldTarget{
                  .owner = class_id_, .slot = instance->member},
              instance->type));
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  mir::MakeAssignExpr(unit.builtins, target, value))});
    }

    // `new(embedding, formals..., counted...)`: the object embedding the
    // covergroup, what the source passed, and whether each item the class
    // declares counts toward the instance.
    std::optional<std::pair<mir::LocalId, mir::TypeId>> embedding;
    if (decl_->embedding.has_value()) {
      const hir::ProceduralVarId var = *decl_->embedding;
      const mir::TypeId type =
          CovergroupPropertyType(*u_, own_, *own_.layout.embedding);
      const mir::LocalId local = bindings.DeclareProcedural(
          ProceduralOrigin{.var = var},
          decl_->body.procedural_vars.Get(var).name, type);
      lowerer.MapProceduralVar(var, AutomaticVarBinding{.type = type});
      params.push_back(local);
      embedding = std::pair{local, type};
    }
    std::vector<std::optional<std::pair<mir::LocalId, mir::TypeId>>> inputs;
    for (const hir::SubroutineParam& formal : decl_->formals) {
      const hir::ProceduralVarDecl& var =
          decl_->body.procedural_vars.Get(formal.var);
      const mir::TypeId value = u_->TranslateType(var.type);
      if (formal.direction == hir::ParamDirection::kRef ||
          formal.direction == hir::ParamDirection::kConstRef) {
        params.push_back(lowerer.DeclareReferenceFormal(
            bindings, formal.var, var,
            *ParamTypeOf(unit, value, formal.direction)));
        inputs.emplace_back();
        continue;
      }
      const mir::LocalId local = bindings.DeclareProcedural(
          ProceduralOrigin{.var = formal.var}, var.name, value);
      lowerer.MapProceduralVar(formal.var, AutomaticVarBinding{.type = value});
      params.push_back(local);
      inputs.emplace_back(std::pair{local, value});
    }
    const mir::TypeId bit = u_->TranslateType(u_->Hir().builtins.scalar_bit);
    std::map<std::string, mir::LocalId> counted;
    for (const hir::Coverpoint& point : decl_->coverpoints) {
      counted[point.name] = bindings.DeclareAnonymous(bit);
      params.push_back(counted[point.name]);
    }
    for (const hir::Cross& cross : decl_->crosses) {
      counted[cross.name] = bindings.DeclareAnonymous(bit);
      params.push_back(counted[cross.name]);
    }
    const mir::LocalId report =
        bindings.DeclareAnonymous(unit.builtins.read_report_ptr);
    params.push_back(report);
    const Writer e(
        *u_, library_, lowerer, *decl_,
        bound.frame.WithReportingReachedTo(report));

    if (decl_->base.has_value()) {
      if (auto r = ConstructBase(e, embedding, report); !r) return r;
    } else {
      for (std::size_t i = 0; i < decl_->formals.size(); ++i) {
        const std::uint32_t index = own_.layout.formals.at(i);
        if (inputs[i].has_value()) {
          e.Store(
              e.Own(own_, index),
              block.ReadLocal(inputs[i]->first, inputs[i]->second));
          continue;
        }
        // What a `ref` argument names is bound as the object is constructed.
        const hir::ProceduralVarId var = decl_->formals[i].var;
        auto named = lowerer.LowerLhsExpr(
            hir::MakeRefExpr(
                hir::ProceduralVarRef{.var = var},
                decl_->body.procedural_vars.Get(var).type, diag::SourceSpan{}),
            e.Frame());
        if (!named) return std::unexpected(std::move(named.error()));
        e.Do(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kBindReference},
                {block.AsValue(
                     unit.types,
                     block.exprs.Add(
                         mir::MakeFieldAccessExpr(
                             BuildObjectDeref(
                                 unit, block,
                                 block.exprs.Add(MakeSelfRefExpr(e.Frame()))),
                             CovergroupPropertyTarget(*u_, own_, index),
                             CovergroupPropertyType(*u_, own_, index)))),
                 PathReference(unit, block, *named)},
                unit.builtins.void_type));
      }
      e.Store(
          Record(e),
          CallVirtual(
              e, kMakeRecordBehavior, {},
              CovergroupPropertyType(*u_, first_, *first_.layout.record)));
    }
    if (embedding.has_value()) {
      e.Store(
          e.Own(own_, *own_.layout.embedding),
          block.ReadLocal(embedding->first, embedding->second));
    }

    const auto [record_options, record_options_type] =
        OptionsOf(e, e.Read(Record(e)), Refs().group_instance);
    if (auto r = WriteOptions(
            e, decl_->options, false, record_options, record_options_type);
        !r) {
      return r;
    }

    for (std::size_t p = 0; p < decl_->coverpoints.size(); ++p) {
      const hir::Coverpoint& point = decl_->coverpoints[p];
      const hir::PublishedCoverpoint& counted_by =
          own_.published->coverpoints.at(p);
      hir::ClassId cls = counted_by.library_class;
      e.Store(
          e.Own(own_, own_.layout.coverpoints.at(p)),
          NewLibraryObject(
              e, cls,
              {e.Read(Record(e)), e.String(point.name),
               block.ReadLocal(counted.at(point.name), bit)}));
      const mir::ExprId item =
          e.Read(e.Own(own_, own_.layout.coverpoints.at(p)));
      const auto [options, options_type] = OptionsOf(e, item, cls);
      if (auto r = WriteOptions(e, point.options, false, options, options_type);
          !r) {
        return r;
      }
      // An enumeration's automatic bins are its values, named by theirs (LRM
      // 19.5.3).
      if (const auto* enumeration =
              u_->Hir().types.Get(counted_by.value_type).As<hir::EnumType>()) {
        for (const hir::EnumMember& member : enumeration->members) {
          e.Do(e.Call(
              item, cls, "name_enumeration_value",
              {mir::BuildIntegralLiteral(
                   unit, block, u_->TranslateType(counted_by.value_type),
                   LowerHirIntegralConstant(member.value)),
               e.String(member.name)}));
        }
      }
      for (std::size_t b = 0; b < point.bins.size(); ++b) {
        if (auto r = WriteBins(
                e, point.name, cls, point.bins[b],
                static_cast<std::int64_t>(b));
            !r) {
          return r;
        }
      }
      e.Do(e.Call(item, cls, "finish"));
    }
    for (std::size_t c = 0; c < decl_->crosses.size(); ++c) {
      const hir::Cross& cross = decl_->crosses[c];
      hir::ClassId cls = Refs().cross;
      e.Store(
          e.Own(own_, own_.layout.crosses.at(c)),
          NewLibraryObject(
              e, cls,
              {e.Read(Record(e)), e.String(cross.name),
               block.ReadLocal(counted.at(cross.name), bit)}));
      const mir::ExprId item = e.Read(e.Own(own_, own_.layout.crosses.at(c)));
      const auto [options, options_type] = OptionsOf(e, item, cls);
      if (auto r = WriteOptions(e, cross.options, false, options, options_type);
          !r) {
        return r;
      }
      for (const std::string& target : cross.targets) {
        e.Do(e.Call(
            item, cls, "cross_coverpoint", {e.Read(Item(e, target).first)}));
      }
      for (const hir::CrossBin& bin : cross.bins) {
        auto selection = WriteSelection(e, cross, bin.select);
        if (!selection) return std::unexpected(std::move(selection.error()));
        e.Do(e.Call(
            item, cls, "add_bin",
            {e.String(bin.name),
             e.LibraryEnum(cls, "add_bin", 1, LibraryKindOf(bin.kind)),
             *selection}));
      }
      e.Do(e.Call(item, cls, "finish"));
    }

    if (!decl_->base.has_value()) {
      if (auto r = StartSampling(e); !r) return r;
    }

    code.params = std::move(params);
    code.result_type = unit.builtins.void_type;
    cls_->constructor = mir::ConstructorDecl{.code = std::move(code)};
    return {};
  }

  // The base's part of the object constructed, with the instance its class
  // belongs to, the object embedding it, the arguments, and which of the
  // base's items count -- each one this class does not declare again.
  [[nodiscard]] auto ConstructBase(
      const Writer& e,
      const std::optional<std::pair<mir::LocalId, mir::TypeId>>& embedding,
      mir::LocalId report) const -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    mir::Block& block = e.Block();
    hir::CovergroupId base = *decl_->base;
    const hir::PublishedCovergroup& published = PublishedOf(*u_, base);
    std::vector<mir::ExprId> arguments;
    if (TakesDeclaringInstance(*u_, base)) {
      arguments = InstanceArgument(e);
    }
    if (published.embedding.has_value()) {
      arguments.push_back(ConvertToType(
          unit, block, block.ReadLocal(embedding->first, embedding->second),
          HandleTo(*u_, *published.embedding)));
    }
    for (std::size_t i = 0; i < decl_->formals.size(); ++i) {
      const hir::SubroutineParam& formal = decl_->formals[i];
      const hir::Expr named = hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = formal.var},
          decl_->body.procedural_vars.Get(formal.var).type, diag::SourceSpan{});
      auto bound = EmitBoundActual(
          e.Body(), e.Frame(), named, published.formals.at(i).direction,
          u_->TranslateType(published.formals.at(i).type));
      if (!bound) return std::unexpected(std::move(bound.error()));
      arguments.push_back(*bound);
    }
    const auto declared_again = [&](std::string_view name) {
      return std::ranges::any_of(
                 decl_->coverpoints,
                 [&](const hir::Coverpoint& point) {
                   return point.name == name;
                 }) ||
             std::ranges::any_of(decl_->crosses, [&](const hir::Cross& cross) {
               return cross.name == name;
             });
    };
    for (const hir::PublishedCoverpoint& point : published.coverpoints) {
      arguments.push_back(e.Bit(!declared_again(point.name)));
    }
    for (const std::string& cross : published.crosses) {
      arguments.push_back(e.Bit(!declared_again(cross)));
    }
    arguments.push_back(block.ReadLocal(report, unit.builtins.read_report_ptr));
    e.Do(
        mir::MakeCallExpr(
            mir::Direct{
                .target =
                    mir::ConstructorTarget{
                        .cls = u_->CovergroupClassIdentity(base)},
                .receiver = BuildObjectDeref(
                    unit, block, block.exprs.Add(MakeSelfRefExpr(e.Frame())))},
            std::move(arguments), unit.builtins.void_type));
    return {};
  }

  // The event a block of the scope or class declaring the covergroup triggers
  // for it (LRM 19.3): the record's own where a class embeds the covergroup,
  // and the one kept for the covergroup's type otherwise.
  [[nodiscard]] auto BlockEventOf(const Writer& e) const -> mir::ExprId {
    mir::CompilationUnit& unit = Unit();
    mir::Block& block = e.Block();
    if (decl_->embedding.has_value()) {
      return e.Read(e.LibraryProperty(
          e.Read(Record(e)), Refs().group_instance, "block_event"));
    }
    const mir::TypeId cell_type =
        mir::ObservableCellOf(unit.types, unit.builtins.event);
    const mir::ExprId cell = block.exprs.Add(BuildStaticStorageAccess(
        unit, e.Frame(), **type_event_, cell_type, mir::EnclosingHops{}));
    return block.AsValue(
        unit.types, block.exprs.Add(block.WrapperContents(
                        unit.types, cell, unit.builtins.event)));
  }

  // Starts the execution that samples the covergroup at each occurrence of
  // what it is sampled at, for as long as the run goes on (LRM 19.3): woken,
  // it runs inside the occurrence that woke it, as if the process making it
  // happen called `sample`. Under `strobe` the first occurrence of a time slot
  // places one sample in its Postponed region instead (LRM 19.7).
  [[nodiscard]] auto StartSampling(const Writer& e) const
      -> diag::Result<void> {
    // What the execution waits for each time round, and whether an occurrence
    // samples under `strobe`, which a clocking event's does (LRM 19.7); a
    // covergroup sampled only by calls has no execution.
    struct Sampler {
      std::function<diag::Result<mir::Stmt>(
          ProcessLowerer&, const Writer&, mir::Block&)>
          wait;
      bool strobes = false;
    };
    const std::optional<Sampler> sampler = std::visit(
        Overloaded{
            [](const hir::SampledByCall&) -> std::optional<Sampler> {
              return std::nullopt;
            },
            [](const hir::SampledOnEvent& on) -> std::optional<Sampler> {
              return Sampler{
                  .wait =
                      [&on](
                          ProcessLowerer& lowerer, const Writer& at,
                          mir::Block& block) {
                        return BuildEventWaitStmt(
                            lowerer, at.Frame(), block, on.event);
                      },
                  .strobes = true};
            },
            [this](const hir::SampledAtBlocks&) -> std::optional<Sampler> {
              return Sampler{
                  .wait =
                      [this](
                          ProcessLowerer& lowerer, const Writer& at,
                          mir::Block& block) {
                        return BuildEventObjectWaitStmt(
                            lowerer, at.Frame(), block, BlockEventOf(at));
                      },
                  .strobes = false};
            }},
        decl_->sampling);
    if (!sampler.has_value()) return {};
    if (!decl_->sample_formals.empty()) {
      return diag::Fail(
          decl_->span, diag::DiagCode::kUnsupportedClassFeature,
          "a covergroup sampled on an event whose 'sample' takes arguments is "
          "not yet supported");
    }
    mir::CompilationUnit& unit = Unit();
    ClosureBuilder closure(
        unit, e.Frame(), support::ClosureKind::kFnOnce, CapturePolicy{});
    ProcessLowerer lowerer = Lowerer();
    const Writer inside(
        *u_, library_, lowerer, *decl_, closure.Frame().WithBodyCanWait(true));
    BindKept(inside);
    mir::Block each;
    const Writer round = inside.At(inside.Frame().WithBlock(&each));
    const auto sample = [&](const Writer& at) {
      at.Do(CallVirtual(at, kSampleBehavior, {}, std::nullopt));
    };
    auto waited = sampler->wait(lowerer, round, each);
    if (!waited) return std::unexpected(std::move(waited.error()));
    each.AppendStmt(*std::move(waited));
    if (sampler->strobes) {
      round.IfElse(
          round.Call(
              round.Read(Record(round)), Refs().group_instance, "strobed"),
          [&](const Writer& strobed) {
            strobed.If(
                strobed.Call(
                    strobed.Read(Record(strobed)), Refs().group_instance,
                    "claim_strobe"),
                [&](const Writer& claimed) {
                  ClosureBuilder postponed(
                      unit, claimed.Frame(), support::ClosureKind::kFn,
                      CapturePolicy{});
                  const Writer later = claimed.At(postponed.Frame());
                  later.Do(later.Call(
                      later.Read(Record(later)), Refs().group_instance,
                      "strobe_taken"));
                  sample(later);
                  mir::Expr keeper = BuildRegionEffectsCallExpr(
                      claimed.Lowering(), claimed.Block(),
                      support::BuiltinFn::kPostponedEffects);
                  claimed.Do(BuildEmplacedClosure(
                      claimed.Lowering().Unit(), claimed.Frame(),
                      std::move(keeper), postponed.BuildVoid()));
                });
          },
          sample);
    } else {
      sample(round);
    }
    closure.Body().AppendStmt(
        mir::ForStmt{
            .init = {},
            .condition = std::nullopt,
            .step = {},
            .scope = closure.Body().child_scopes.Add(std::move(each))});
    HandToRuntime(
        e, support::BuiltinFn::kRunObserving,
        e.Block().exprs.Add(closure.BuildCallOnce()));
    return {};
  }

  // `sample` (LRM 19.8): each coverpoint's guard and expression evaluated and
  // its value counted, then each cross over what its coverpoints counted. A
  // covergroup extending another samples the base's first (LRM 19.4.1).
  auto WriteSample() -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    SynthesizedFunction function(
        *u_, owner_, Shape(), CallableForm::kInstanceMember);
    ProcessLowerer lowerer = Lowerer();
    const Writer e(*u_, library_, lowerer, *decl_, function.Frame());
    std::vector<mir::ExprId> forwarded;
    for (const hir::SubroutineParam& formal : decl_->sample_formals) {
      const hir::ProceduralVarDecl& var =
          decl_->body.procedural_vars.Get(formal.var);
      const mir::TypeId type =
          *ParamTypeOf(unit, u_->TranslateType(var.type), formal.direction);
      const mir::LocalId local = function.Bindings().DeclareProcedural(
          ProceduralOrigin{.var = formal.var}, var.name, type);
      lowerer.MapProceduralVar(formal.var, AutomaticVarBinding{.type = type});
      function.AddFormalLocal(local);
      forwarded.push_back(e.Block().ReadLocal(local, type));
    }
    if (decl_->base.has_value()) {
      e.Do(BuildFunctionCall(
          unit, e.Frame(),
          mir::Direct{
              .target = TypeMethod(*u_, *decl_->base, kSample),
              .receiver = BuildObjectDeref(
                  unit, e.Block(),
                  e.Block().exprs.Add(MakeSelfRefExpr(e.Frame())))},
          std::move(forwarded), std::nullopt));
    }
    BindKept(e);
    diag::Result<void> written;
    e.If(
        e.Read(e.LibraryProperty(
            e.Read(Record(e)), Refs().group_instance, "collecting")),
        [&](const Writer& collecting) { written = SampleItems(collecting); });
    if (!written) return written;
    cls_->callables.Define(
        kSample, mir::CallableDecl{
                     .code = function.Finish(std::nullopt),
                     .foreign = std::nullopt,
                     .virtual_dispatch = RoleIn(*u_, Self(), kSampleBehavior),
                     .defining_unit = std::nullopt});
    return {};
  }

  // What `sample` does while the instance collects.
  [[nodiscard]] auto SampleItems(const Writer& e) const -> diag::Result<void> {
    // Each bin construct guarded by an `iff` is told whether its guard holds
    // (LRM 19.5).
    const auto gates = [&](std::string_view name, hir::ClassId cls,
                           const auto& constructs) -> diag::Result<void> {
      for (std::size_t i = 0; i < constructs.size(); ++i) {
        if (!constructs[i].iff.has_value()) continue;
        auto guard = e.Value(*constructs[i].iff);
        if (!guard) return std::unexpected(std::move(guard.error()));
        e.Do(e.Call(
            e.Read(Item(e, name).first), cls, "gate",
            {e.Int(static_cast<std::int64_t>(i)), *guard}));
      }
      return {};
    };
    for (std::size_t p = 0; p < decl_->coverpoints.size(); ++p) {
      const hir::Coverpoint& point = decl_->coverpoints[p];
      hir::ClassId cls = own_.published->coverpoints.at(p).library_class;
      if (auto r = gates(point.name, cls, point.bins); !r) {
        return r;
      }
      const auto count = [&](const Writer& at) -> diag::Result<void> {
        auto value = at.Value(point.expression);
        if (!value) return std::unexpected(std::move(value.error()));
        at.Do(at.Call(
            at.Read(at.Own(own_, own_.layout.coverpoints.at(p))), cls, "sample",
            {*value}));
        return {};
      };
      if (!point.iff.has_value()) {
        if (auto r = count(e); !r) return r;
        continue;
      }
      // A coverpoint whose guard is false is ignored (LRM 19.5), so nothing
      // it hit reaches a cross either.
      auto guard = e.Value(*point.iff);
      if (!guard) return std::unexpected(std::move(guard.error()));
      diag::Result<void> counted;
      e.IfElse(
          *guard, [&](const Writer& at) { counted = count(at); },
          [&](const Writer& at) {
            at.Do(at.Call(
                at.Read(at.Own(own_, own_.layout.coverpoints.at(p))), cls,
                "skip"));
          });
      if (!counted) return counted;
    }
    for (std::size_t c = 0; c < decl_->crosses.size(); ++c) {
      const hir::Cross& cross = decl_->crosses[c];
      if (auto r = gates(cross.name, Refs().cross, cross.bins); !r) return r;
      const auto count = [&](const Writer& at) {
        at.Do(at.Call(
            at.Read(at.Own(own_, own_.layout.crosses.at(c))), Refs().cross,
            "sample"));
      };
      if (!cross.iff.has_value()) {
        count(e);
        continue;
      }
      auto guard = e.Value(*cross.iff);
      if (!guard) return std::unexpected(std::move(guard.error()));
      e.If(*guard, count);
    }
    return {};
  }

  // The record of the object's own type (LRM 19.11.3), which the first class
  // of the lineage makes the instance's record of.
  void WriteMakeRecord() {
    SynthesizedFunction function(
        *u_, owner_, Shape(), CallableForm::kInstanceMember);
    ProcessLowerer lowerer = Lowerer();
    const Writer e(*u_, library_, lowerer, *decl_, function.Frame());
    const mir::TypeId record =
        CovergroupPropertyType(*u_, first_, *first_.layout.record);
    const mir::ExprId made =
        e.Call(TypeRecordOf(e, Self()), Refs().group_type, "make_instance");
    cls_->callables.Define(
        kMakeRecord,
        mir::CallableDecl{
            .code =
                function.Finish(ConvertToType(Unit(), e.Block(), made, record)),
            .foreign = std::nullopt,
            .virtual_dispatch = RoleIn(*u_, Self(), kMakeRecordBehavior),
            .defining_unit = std::nullopt});
  }

  // The type's record (LRM 19.11.3), found in the run's registry by the
  // declaration's name. The first time it is reached the declaration states
  // its items and type options, and the run writes the database at its end
  // (LRM 19.9): owed because the simulation is over, so held as a call run
  // after the final procedures.
  void WriteTypeRecord() {
    mir::CompilationUnit& unit = Unit();
    SynthesizedFunction function(
        *u_, owner_, Shape(), CallableForm::kTypeAssociated);
    ProcessLowerer lowerer = Lowerer();
    const Writer e(*u_, library_, lowerer, *decl_, function.Frame());
    const mir::TypeId type = TypeRecordType(*u_, library_);
    const mir::LocalId found = function.Bindings().DeclareAnonymous(type);
    e.Block().AppendStmt(
        mir::LocalDeclStmt{
            .target = found,
            .init = e.Call(
                std::nullopt, Refs().registry, "type_named",
                {e.String(TypeName())})});
    const AccessPath declared = e.LibraryProperty(
        e.Block().ReadLocal(found, type), Refs().group_type, "declared");
    const mir::ExprId undeclared = BuildConditionNot(
        unit, e.Block(), ReduceToCondition(unit, e.Block(), e.Read(declared)));
    mir::Block first;
    const Writer then = e.At(e.Frame().WithBlock(&first));
    then.Store(
        then.LibraryProperty(
            first.ReadLocal(found, type), Refs().group_type, "declared"),
        then.Bit(true));
    std::vector<mir::ExprId> arguments = InstanceArgument(then);
    arguments.push_back(first.ReadLocal(found, type));
    then.Do(BuildFunctionCall(
        unit, then.Frame(),
        mir::Direct{
            .target =
                mir::CallableTarget{.owner = class_id_, .slot = kDeclareType}},
        std::move(arguments), std::nullopt));
    then.If(
        then.Call(std::nullopt, Refs().registry, "claim_database_writing"),
        [&](const Writer& claimed) {
          ClosureBuilder at_end(
              unit, claimed.Frame(), support::ClosureKind::kFn,
              CapturePolicy{});
          const Writer last = claimed.At(at_end.Frame());
          last.Do(last.Call(std::nullopt, Refs().registry, "write_database"));
          HandToRuntime(
              claimed, support::BuiltinFn::kSubmitAtRunEnd,
              BuildKeptClosure(unit, claimed.Block(), at_end.BuildVoid()));
        });
    e.Block().AppendIfThen(undeclared, std::move(first));
    cls_->callables.Define(
        kTypeRecord,
        mir::CallableDecl{
            .code = function.Finish(e.Block().ReadLocal(found, type)),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
  }

  // What the declaration states of its type: its items, and the type options
  // set in it (LRM 19.7), after those of what it extends (LRM 19.4.1).
  auto WriteDeclareType() -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    SynthesizedFunction function(
        *u_, owner_, Shape(), CallableForm::kTypeAssociated);
    ProcessLowerer lowerer = Lowerer();
    const Writer e(*u_, library_, lowerer, *decl_, function.Frame());
    const mir::TypeId type = TypeRecordType(*u_, library_);
    const mir::LocalId given = function.AddFormal(type);
    const auto record = [&] { return e.Block().ReadLocal(given, type); };
    if (decl_->base.has_value()) {
      std::vector<mir::ExprId> arguments;
      if (TakesDeclaringInstance(*u_, *decl_->base)) {
        arguments = InstanceArgument(e);
      }
      arguments.push_back(record());
      e.Do(BuildFunctionCall(
          unit, e.Frame(),
          mir::Direct{.target = TypeMethod(*u_, *decl_->base, kDeclareType)},
          std::move(arguments), std::nullopt));
    }
    for (const hir::Coverpoint& point : decl_->coverpoints) {
      e.Do(e.Call(
          record(), Refs().group_type, "declare_coverpoint",
          {e.String(point.name)}));
    }
    for (const hir::Cross& cross : decl_->crosses) {
      e.Do(e.Call(
          record(), Refs().group_type, "declare_cross",
          {e.String(cross.name)}));
    }
    if (auto r = WriteOptions(
            e, decl_->options, true,
            e.LibraryProperty(record(), Refs().group_type, "type_option"),
            library_.PropertyOf(Refs().group_type, "type_option").type);
        !r) {
      return r;
    }
    // Each item's type options are the element of the type's table its name
    // keys.
    const auto item_options = [&](std::string_view table,
                                  const std::string& name) {
      return ItemTypeOptions(
          *u_, e.LibraryProperty(record(), Refs().group_type, table),
          library_.PropertyOf(Refs().group_type, table).type, e.String(name));
    };
    for (const hir::Coverpoint& point : decl_->coverpoints) {
      const auto [options, options_type] =
          item_options("coverpoint_type_options", point.name);
      if (auto r = WriteOptions(e, point.options, true, options, options_type);
          !r) {
        return r;
      }
    }
    for (const hir::Cross& cross : decl_->crosses) {
      const auto [options, options_type] =
          item_options("cross_type_options", cross.name);
      if (auto r = WriteOptions(e, cross.options, true, options, options_type);
          !r) {
        return r;
      }
    }
    cls_->callables.Define(
        kDeclareType, mir::CallableDecl{
                          .code = function.Finish(std::nullopt),
                          .foreign = std::nullopt,
                          .virtual_dispatch = std::nullopt,
                          .defining_unit = std::nullopt});
    return {};
  }

  UnitLowerer* u_;
  const hir::CovergroupDecl* decl_;
  const StructuralScopeLowerer* declaring_scope_;
  const DeclaredScopes* scopes_;
  mir::Class* cls_;
  mir::ClassId class_id_;
  WalkFrame owner_;
  const std::optional<StaticStorageHome>* type_event_;
  CoverageLibrary library_;
  CovergroupClass own_;
  CovergroupClass first_;
};

}  // namespace

CovergroupLowerer::CovergroupLowerer(
    UnitLowerer& unit_lowerer, hir::CovergroupId covergroup,
    const StructuralScopeLowerer* declaring_scope)
    : unit_lowerer_(&unit_lowerer),
      covergroup_(covergroup),
      decl_(&unit_lowerer.Hir().covergroups.Get(covergroup)),
      declaring_scope_(declaring_scope),
      class_id_(unit_lowerer.CovergroupClassIdentity(covergroup)) {
}

void CovergroupLowerer::DeclareShape(
    ClassShape* declaring_shape, std::optional<StaticStorageHome> type_event) {
  UnitLowerer& unit_lowerer = *unit_lowerer_;
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::CovergroupId self = covergroup_;
  const CovergroupClass own = CovergroupClassOf(unit_lowerer, self);
  ClassShape shape{
      .identity = unit_lowerer.Hir().covergroups.PathOf(covergroup_),
      .base = decl_->base.has_value()
                  ? mir::ClassRef(
                        unit_lowerer.CovergroupClassIdentity(*decl_->base))
                  : mir::ClassRef{mir::ManagedObjectRootRef{}},
      .implements = {},
      .self_pointer_type = unit_lowerer.SelfPointerTo(class_id_),
      .declaring_instance = std::nullopt,
      .time_resolution = {},
      .execution = {},
      .dumped_as = {},
      .ctor_prefix_params = {},
      .fields = {},
      .named_fields = {},
      .static_properties = {},
      .named_static_properties = {},
      .callable_signatures = {},
      .library_callbacks = {},
      .static_property_translation = {},
      .contained = {},
      .is_final = false,
      .is_interface_class = false};
  // The properties sit where the layout every unit computes places them, so
  // a property's position is its index in the layout.
  for (std::size_t i = 0; i < own.layout.properties.size(); ++i) {
    const auto index = static_cast<std::uint32_t>(i);
    shape.AddNamedField(
        own.layout.properties[i].name,
        CovergroupPropertyType(unit_lowerer, own, index));
  }
  // The instance the class belongs to (LRM 6.22), after every property,
  // since nothing outside the unit reads it.
  if (TakesDeclaringInstance(unit_lowerer, self)) {
    const mir::TypeId instance_type = unit.builtins.scope_ptr;
    shape.declaring_instance = DeclaringInstance{
        .type = instance_type, .member = shape.AddField(instance_type)};
  }
  shape.callable_signatures = {
      4,
      {CallableSignature{
           .virtual_dispatch = RoleIn(unit_lowerer, self, kSampleBehavior)},
       CallableSignature{
           .virtual_dispatch = RoleIn(unit_lowerer, self, kMakeRecordBehavior)},
       CallableSignature{.virtual_dispatch = std::nullopt},
       CallableSignature{.virtual_dispatch = std::nullopt}}};

  // What the class keeps for its type goes where what replicates the
  // covergroup's declaration puts it (LRM 6.22, 8.9): a cell of the scope's
  // instance where a scope declares it, and the class's own otherwise.
  const StaticStorageOwner class_storage =
      declaring_shape != nullptr ? StaticStorageOwner{InstanceStorage{
                                       .owner = declaring_scope_->ClassId(),
                                       .shape = declaring_shape,
                                       .placed = {}}}
                                 : StaticStorageOwner{ClassStorage{
                                       .owner = class_id_, .shape = &shape}};
  if (std::holds_alternative<hir::SampledAtBlocks>(decl_->sampling) &&
      !decl_->embedding.has_value()) {
    type_event_ =
        type_event.has_value()
            ? *std::move(type_event)
            : DeclareStaticCell(
                  class_storage,
                  mir::ObservableCellOf(unit.types, unit.builtins.event));
  }
  scopes_ = ScopesOwningDisableTargets(
      decl_->procedural_scopes, decl_->declared_in, class_storage,
      unit.types.Intern(
          mir::Type{mir::RuntimeLibraryType{
              .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));
  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
}

auto CovergroupLowerer::PopulateBodies(
    const WalkFrame& declaring_frame, const StorageBringUp& bring_up)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *unit_lowerer_;
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  mir::Class cls = shape.OpenClass();
  ScopeChainNode link{};
  WalkFrame owner = declaring_frame.WithClass(
      {.cls = &cls,
       .id = class_id_,
       .enclosing = std::nullopt,
       .published = std::nullopt},
      link);
  owner =
      shape.declaring_instance.has_value()
          ? owner.WithStructuralBase(
                ScopeThroughMember{.member = shape.declaring_instance->member})
          : owner.WithStructuralBase(NoScope{});

  // The event kept for the type names a synchronization object of its own from
  // before any process runs, as a declared event does (LRM 6.17), brought up
  // with the rest of what replicates the declaration (LRM 10.5).
  if (type_event_.has_value()) {
    mir::Block& install = *bring_up.install.current_block;
    const mir::TypeId cell_type =
        mir::ObservableCellOf(unit.types, unit.builtins.event);
    install.AppendStmt(
        mir::ExprStmt{
            .expr = install.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    install.exprs.Add(BuildStaticStorageAccess(
                        unit, bring_up.install, *type_event_, cell_type,
                        mir::EnclosingHops{})),
                    install.exprs.Add(
                        mir::MakeCallExpr(
                            mir::Direct{
                                .target = support::BuiltinFn::kEventNew},
                            {}, unit.builtins.event)),
                    support::BuiltinFn::kInitialize,
                    unit.builtins.void_type))});
  }

  ClassWriter writer(
      unit_lowerer, covergroup_, *decl_, declaring_scope_, scopes_, cls,
      class_id_, owner, type_event_);
  if (auto r = writer.Write(); !r) return r;
  unit.DefineClass(class_id_, std::move(cls));
  return {};
}

auto FactorySlot(const UnitLowerer& unit_lowerer, hir::CovergroupId first)
    -> mir::VirtualSlot {
  const hir::ClassId embedding = *PublishedOf(unit_lowerer, first).embedding;
  const hir::ClassRegistry& classes = unit_lowerer.Hir().classes;
  const hir::DeclaredMethods& methods = classes.Declaration(embedding).methods;
  for (const hir::MethodId method : methods.Ids()) {
    if (methods.Get(method).makes == first) {
      return unit_lowerer.VirtualSlotOf(
          hir::ClassMethodTarget{.owner = embedding, .method = method});
    }
  }
  throw InternalError(
      "the class embedding a covergroup declares no method making it -- "
      "please report this as a bug");
}

namespace {

// The instance of the scope the class whose body `frame` writes belongs to,
// handed on as the scope every instance is (LRM 6.22).
auto DeclaringInstanceOf(mir::CompilationUnit& unit, const WalkFrame& frame)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand = block.AsValue(
                      unit.types, BuildEnclosingScopeReceiver(
                                      frame, unit, mir::EnclosingHops{0}))},
          .type = unit.builtins.scope_ptr});
}

// Whether the handle `handle` holds an object, as a condition.
auto HoldsObject(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId handle)
    -> mir::ExprId {
  const mir::TypeId type = block.exprs.Get(handle).type;
  return ReduceToCondition(
      unit, block,
      BuildEquality(
          unit, block, support::ValueOperator::kInequality, handle,
          block.exprs.Add(mir::Expr{.data = mir::NullLiteral{}, .type = type}),
          unit.builtins.bit1));
}

}  // namespace

auto LowerCovergroupFactory(
    UnitLowerer& unit_lowerer, const WalkFrame& owner, const ClassShape& shape,
    hir::CovergroupId covergroup) -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::CovergroupDecl& decl =
      unit_lowerer.Hir().covergroups.Get(covergroup);
  const hir::CovergroupId self = covergroup;
  const CovergroupClass own = CovergroupClassOf(unit_lowerer, self);
  SynthesizedFunction function(
      unit_lowerer, owner, shape, CallableForm::kInstanceMember);
  const WalkFrame& frame = function.Frame();
  mir::Block& block = function.Body();

  // What `new` of the covergroup is handed, as a construction no extension
  // takes part in makes it: the object, the arguments, and every item
  // counted.
  std::vector<mir::ExprId> arguments;
  if (TakesDeclaringInstance(unit_lowerer, self)) {
    arguments.push_back(DeclaringInstanceOf(unit, frame));
  }
  arguments.push_back(ConvertToType(
      unit, block, block.exprs.Add(BuildThisHandle(frame)),
      HandleTo(unit_lowerer, *own.published->embedding)));
  for (const hir::SubroutineParam& formal : decl.formals) {
    const mir::TypeId type = *ParamTypeOf(
        unit,
        unit_lowerer.TranslateType(
            decl.body.procedural_vars.Get(formal.var).type),
        formal.direction);
    arguments.push_back(block.ReadLocal(function.AddFormal(type), type));
  }
  for (std::size_t i = 0;
       i < own.published->coverpoints.size() + own.published->crosses.size();
       ++i) {
    arguments.push_back(mir::BuildBit1Literal(unit, block, true));
  }
  arguments.push_back(BuildReportHandedOn(unit, frame));
  const mir::TypeId made_type = CovergroupHandle(unit_lowerer, self);
  const mir::LocalId made = function.Bindings().DeclareAnonymous(made_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = made,
          .init = block.exprs.Add(
              mir::MakeNewExpr(
                  block.exprs.Add(
                      mir::MakeObjectConstructionExpr(
                          *mir::AllocatedType(unit.types, made_type),
                          std::move(arguments))),
                  made_type))});

  const mir::TypeId answered =
      CovergroupHandle(unit_lowerer, FirstOf(unit_lowerer, self));
  return function.Finish(
      ConvertToType(unit, block, block.ReadLocal(made, made_type), answered));
}

void AddEmbeddedBlockEvents(
    UnitLowerer& unit_lowerer, hir::ClassId embedding,
    BlockEventTriggers& triggers) {
  const hir::ClassDeclaration& holder =
      unit_lowerer.Hir().classes.Declaration(embedding);
  for (const hir::CovergroupId covergroup :
       hir::EmbeddedCovergroupsOf(holder.methods)) {
    const auto* at_blocks = std::get_if<hir::SampledAtBlocks>(
        &unit_lowerer.Hir().covergroups.Get(covergroup).sampling);
    if (at_blocks == nullptr) continue;
    for (const hir::FieldId field : holder.properties.Ids()) {
      const hir::TypeId type = holder.properties.Get(field).type;
      const auto* handle =
          unit_lowerer.Hir().types.Get(type).As<hir::CovergroupHandleType>();
      if (handle == nullptr || handle->covergroup != covergroup) continue;
      const HeldCovergroupEvent trigger{
          .holder =
              mir::ClassFieldTarget{
                  .owner = unit_lowerer.ClassIdentity(embedding),
                  .slot = mir::FieldId{field.value}},
          .handle_type = unit_lowerer.TranslateType(type),
          .covergroup = covergroup};
      for (const hir::BlockEvent& event : at_blocks->events) {
        ScopeBlockEvents& at = triggers[event.block];
        (event.at_begin ? at.at_begin : at.at_end).emplace_back(trigger);
      }
    }
  }
}

void CovergroupLowerer::AddTypeBlockEvents(BlockEventTriggers& triggers) const {
  if (!type_event_.has_value()) return;
  const auto* at_blocks = std::get_if<hir::SampledAtBlocks>(&decl_->sampling);
  if (at_blocks == nullptr) return;
  for (const hir::BlockEvent& event : at_blocks->events) {
    ScopeBlockEvents& at = triggers[event.block];
    (event.at_begin ? at.at_begin : at.at_end)
        .emplace_back(TypeEventCell{.home = *type_event_});
  }
}

void AppendBlockEventTriggers(
    ProcessLowerer& lowerer, const WalkFrame& frame,
    std::span<const BlockEventTrigger> triggers) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  for (const BlockEventTrigger& trigger : triggers) {
    std::visit(
        Overloaded{
            [&](const TypeEventCell& cell) {
              const mir::ExprId held = block.exprs.Add(BuildStaticStorageAccess(
                  unit, frame, cell.home,
                  mir::ObservableCellOf(unit.types, unit.builtins.event),
                  mir::EnclosingHops{}));
              block.AppendStmt(
                  mir::ExprStmt{
                      .expr = block.exprs.Add(BuildTriggerCallExpr(
                          unit_lowerer, block,
                          block.exprs.Add(block.WrapperContents(
                              unit.types, held, unit.builtins.event))))});
            },
            [&](const HeldCovergroupEvent& held) {
              // The holder's handle, read in `at`.
              const auto holder = [&](mir::Block& at) {
                return at.AsValue(
                    unit.types,
                    at.exprs.Add(PropertyStorage(
                        unit, at,
                        at.exprs.Add(MakeSelfRefExpr(frame.WithBlock(&at))),
                        held.holder, held.handle_type)));
              };
              const mir::ExprId holds = HoldsObject(unit, block, holder(block));
              mir::Block told;
              const mir::ExprId handle = holder(told);
              const auto [first, record] =
                  RecordOf(unit_lowerer, held.covergroup);
              const mir::ExprId record_handle = told.AsValue(
                  unit.types,
                  told.exprs.Add(PropertyStorage(
                      unit, told, handle,
                      CovergroupPropertyTarget(unit_lowerer, first, record),
                      CovergroupPropertyType(unit_lowerer, first, record))));
              const CoverageLibrary library(unit_lowerer.Hir());
              const CoverageLibrary::Property event = library.PropertyOf(
                  library.Refs().group_instance, "block_event");
              told.AppendStmt(
                  mir::ExprStmt{
                      .expr = told.exprs.Add(BuildTriggerCallExpr(
                          unit_lowerer, told,
                          told.exprs.Add(PropertyStorage(
                              unit, told, record_handle,
                              LibraryField(unit_lowerer, event),
                              unit_lowerer.TranslateType(event.type)))))});
              block.AppendIfThen(holds, std::move(told));
            }},
        trigger);
  }
}

namespace {

// The actuals the source passed for `formals`, each bound as its formal takes
// it.
template <ExprLowerer Lowerer>
auto BindCovergroupActuals(
    Lowerer& lowerer, const WalkFrame& frame,
    std::span<const hir::PublishedCovergroupFormal> formals,
    std::span<const hir::ExprId> actuals)
    -> diag::Result<std::vector<mir::ExprId>> {
  std::vector<mir::ExprId> bound;
  for (std::size_t i = 0; i < formals.size(); ++i) {
    auto actual = EmitBoundActual(
        lowerer, frame, lowerer.HirExprs().Get(actuals[i]),
        formals[i].direction, lowerer.Owner().TranslateType(formals[i].type));
    if (!actual) return std::unexpected(std::move(actual.error()));
    bound.push_back(*actual);
  }
  return bound;
}

// The record of `covergroup`'s type, as its class answers it, reaching the
// instance its class belongs to as `reach` says.
template <ExprLowerer Lowerer>
auto TypeRecordCall(
    Lowerer& lowerer, const WalkFrame& frame, hir::CovergroupId covergroup,
    const std::optional<hir::DeclaringInstanceReach>& reach) -> mir::ExprId {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  std::vector<mir::ExprId> arguments;
  if (reach.has_value()) {
    arguments.push_back(BuildImplicitInstanceArgument(lowerer, frame, *reach));
  }
  const CoverageLibrary library(unit_lowerer.Hir());
  return BuildFunctionCall(
      unit_lowerer.Unit(), frame,
      mir::Direct{.target = TypeMethod(unit_lowerer, covergroup, kTypeRecord)},
      std::move(arguments), TypeRecordType(unit_lowerer, library));
}

// The instance `handle` holds, as an instance of `covergroup`. A use names
// the covergroup the source wrote, which a variable a base class declares
// holds as the first of its lineage, since the derived covergroup's instance
// replaces the base's (LRM 19.4.1); the object is one of that covergroup, so
// it is seen as one.
auto InstanceAs(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId handle,
    hir::CovergroupId covergroup) -> mir::ExprId {
  const mir::TypeId type = CovergroupHandle(unit_lowerer, covergroup);
  if (block.exprs.Get(handle).type == type) return handle;
  return block.exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = handle}, .type = type});
}

// The library object a method or an option of an instance asks about: the
// item where one is named, and the instance's record otherwise.
struct Keeper {
  mir::ExprId object;
  hir::ClassId cls;
};

auto KeeperOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    hir::CovergroupId covergroup, mir::ExprId instance,
    const std::optional<std::string>& item) -> Keeper {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const CoverageLibrary library(unit_lowerer.Hir());
  const auto read = [&](const CovergroupClass& owner, std::uint32_t property) {
    return block.AsValue(
        unit.types,
        block.exprs.Add(PropertyStorage(
            unit, block, ReportedObject(unit, frame, instance),
            CovergroupPropertyTarget(unit_lowerer, owner, property),
            CovergroupPropertyType(unit_lowerer, owner, property))));
  };
  if (item.has_value()) {
    const LineageItem found =
        ItemOf(unit_lowerer, library.Refs(), covergroup, *item);
    return Keeper{
        .object = read(found.owner, found.property),
        .cls = found.library_class};
  }
  const auto [first, record] = RecordOf(unit_lowerer, covergroup);
  return Keeper{
      .object = read(first, record), .cls = library.Refs().group_instance};
}

// The arguments the source passed a library method, each bound as the
// method's formal takes it; one left to its default picks the form of the
// method taking none.
template <ExprLowerer Lowerer>
auto BindLibraryActuals(
    Lowerer& lowerer, const WalkFrame& frame,
    const CoverageLibrary::Method& method, std::span<const hir::ExprId> actuals,
    std::size_t leading) -> diag::Result<std::vector<mir::ExprId>> {
  std::vector<mir::ExprId> bound;
  for (std::size_t i = 0; i < actuals.size(); ++i) {
    const hir::CalleeParam& formal =
        method.prototype.interface.params.at(leading + i);
    auto actual = EmitBoundActual(
        lowerer, frame, lowerer.HirExprs().Get(actuals[i]), formal.direction,
        lowerer.Owner().TranslateType(formal.type));
    if (!actual) return std::unexpected(std::move(actual.error()));
    bound.push_back(*actual);
  }
  return bound;
}

// The given actuals of a call whose arguments the source may leave to their
// defaults, in order.
auto GivenActuals(std::span<const std::optional<hir::ExprId>> arguments)
    -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> given;
  for (const std::optional<hir::ExprId>& argument : arguments) {
    if (argument.has_value()) given.push_back(*argument);
  }
  return given;
}

// The instance `through` names, as the class of `covergroup` it is used as.
template <ExprLowerer Lowerer>
auto BuildReceivedInstance(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupInstanceReceiver& through,
    hir::CovergroupId covergroup) -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  auto lowered =
      lowerer.LowerExpr(lowerer.HirExprs().Get(through.instance), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return InstanceAs(
      unit_lowerer, block,
      block.AsValue(
          unit_lowerer.Unit().types, block.exprs.Add(*std::move(lowered))),
      covergroup);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerCovergroupNewExpr(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupNewExpr& created, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const CovergroupClass cls =
      CovergroupClassOf(unit_lowerer, created.covergroup);
  const hir::PublishedCovergroup& published = *cls.published;
  // The first covergroup of a lineage a class embeds is made by that object's
  // method, which a class extending the embedding one overrides where its
  // covergroup extends this one (LRM 19.4.1).
  if (created.embedding.has_value() && !published.base.has_value()) {
    auto embedding =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*created.embedding), frame);
    if (!embedding) return std::unexpected(std::move(embedding.error()));
    const mir::ExprId handle =
        block.AsValue(unit.types, block.exprs.Add(*std::move(embedding)));
    auto actuals = BindCovergroupActuals(
        lowerer, frame, published.formals, created.arguments);
    if (!actuals) return std::unexpected(std::move(actuals.error()));
    const mir::VirtualSlot slot = FactorySlot(unit_lowerer, created.covergroup);
    return block.exprs.Get(BuildFunctionCall(
        unit, frame,
        mir::Virtual{
            .receiver = BuildObjectDeref(
                unit, block,
                AsIntroducer(
                    unit.types, block, BuildObjectPointer(unit, block, handle),
                    slot)),
            .slot = slot},
        *std::move(actuals), result_type));
  }
  // Any other is made by its class's construction, handed the object
  // embedding it, the arguments, and every item counted (LRM 19.3).
  std::vector<mir::ExprId> arguments;
  if (created.declaring_instance.has_value()) {
    arguments.push_back(BuildImplicitInstanceArgument(
        lowerer, frame, *created.declaring_instance));
  }
  if (created.embedding.has_value()) {
    auto embedding =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*created.embedding), frame);
    if (!embedding) return std::unexpected(std::move(embedding.error()));
    arguments.push_back(ConvertToType(
        unit, block,
        block.AsValue(unit.types, block.exprs.Add(*std::move(embedding))),
        HandleTo(unit_lowerer, *published.embedding)));
  }
  const hir::CovergroupId lineage_first =
      FirstOf(unit_lowerer, created.covergroup);
  const hir::PublishedCovergroup& first =
      PublishedOf(unit_lowerer, lineage_first);
  auto actuals =
      BindCovergroupActuals(lowerer, frame, first.formals, created.arguments);
  if (!actuals) return std::unexpected(std::move(actuals.error()));
  arguments.insert(arguments.end(), actuals->begin(), actuals->end());
  for (std::size_t i = 0;
       i < published.coverpoints.size() + published.crosses.size(); ++i) {
    arguments.push_back(mir::BuildBit1Literal(unit, block, true));
  }
  arguments.push_back(BuildReportHandedOn(unit, frame));
  return mir::MakeNewExpr(
      block.exprs.Add(
          mir::MakeObjectConstructionExpr(
              *mir::AllocatedType(unit.types, result_type),
              std::move(arguments))),
      result_type);
}

template <ExprLowerer Lowerer>
auto LowerCovergroupMethodCallExpr(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupMethodCallExpr& call) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const CoverageLibrary library(unit_lowerer.Hir());
  const std::vector<hir::ExprId> given = GivenActuals(call.arguments);
  const bool counted = !given.empty();
  // Every method but `get_coverage` is an instance's (LRM 19.8), and the
  // front end reaches it through one.
  const auto* through =
      std::get_if<hir::CovergroupInstanceReceiver>(&call.receiver);
  const auto instance = [&]() -> diag::Result<mir::ExprId> {
    if (through == nullptr) {
      throw InternalError(
          "a method of a covergroup instance is called on none -- please "
          "report this as a bug");
    }
    return BuildReceivedInstance(lowerer, frame, *through, call.covergroup);
  };
  const auto library_call =
      [&](mir::ExprId receiver, hir::ClassId cls, std::string_view name,
          std::vector<mir::ExprId> leading) -> diag::Result<mir::Expr> {
    const CoverageLibrary::Method method = library.MethodOf(cls, name);
    auto bound =
        BindLibraryActuals(lowerer, frame, method, given, leading.size());
    if (!bound) return std::unexpected(std::move(bound.error()));
    leading.insert(leading.end(), bound->begin(), bound->end());
    return block.exprs.Get(BuildMethodCall(
        unit_lowerer, frame, method.callee, receiver, leading,
        method.prototype.result_type));
  };
  // The library method `name` of what keeps the instance's, or its item's,
  // coverage.
  const auto on_keeper = [&](std::string_view name) -> diag::Result<mir::Expr> {
    auto object = instance();
    if (!object) return std::unexpected(std::move(object.error()));
    const Keeper keeper =
        KeeperOf(unit_lowerer, frame, call.covergroup, *object, call.item);
    return library_call(keeper.object, keeper.cls, name, {});
  };
  switch (call.method) {
    case hir::CovergroupMethod::kSample: {
      const CovergroupClass cls =
          CovergroupClassOf(unit_lowerer, call.covergroup);
      if (given.size() != call.arguments.size()) {
        return diag::Fail(
            lowerer.HirExprs().Get(through->instance).span,
            diag::DiagCode::kUnsupportedClassFeature,
            "a default argument of a covergroup's 'sample' is not yet "
            "supported");
      }
      auto object = instance();
      if (!object) return std::unexpected(std::move(object.error()));
      auto actuals = BindCovergroupActuals(
          lowerer, frame, cls.published->sample_formals, given);
      if (!actuals) return std::unexpected(std::move(actuals.error()));
      const mir::VirtualSlot slot =
          IntroducedSlot(unit_lowerer, call.covergroup, kSampleBehavior);
      return block.exprs.Get(BuildFunctionCall(
          unit, frame,
          mir::Virtual{
              .receiver = BuildObjectDeref(
                  unit, block,
                  AsIntroducer(
                      unit.types, block,
                      BuildObjectPointer(unit, block, *object), slot)),
              .slot = slot},
          *std::move(actuals), std::nullopt));
    }
    case hir::CovergroupMethod::kGetCoverage: {
      const auto* of_type =
          std::get_if<hir::CovergroupTypeReceiver>(&call.receiver);
      if (of_type == nullptr) {
        throw InternalError(
            "'get_coverage' is a method of a covergroup's type and is reached "
            "through no instance -- please report this as a bug");
      }
      const mir::ExprId type = TypeRecordCall(
          lowerer, frame, call.covergroup, of_type->declaring_instance);
      hir::ClassId cls = library.Refs().group_type;
      if (!call.item.has_value()) {
        return library_call(
            type, cls, counted ? "get_coverage" : "coverage", {});
      }
      return library_call(
          type, cls, counted ? "get_item_coverage" : "item_coverage",
          {BuildStringValueExpr(unit, block, *call.item)});
    }
    case hir::CovergroupMethod::kGetInstCoverage:
      return on_keeper(counted ? "get_inst_coverage" : "inst_coverage");
    case hir::CovergroupMethod::kSetInstName:
      return on_keeper("set_inst_name");
    case hir::CovergroupMethod::kStart:
      return on_keeper("start");
    case hir::CovergroupMethod::kStop:
      return on_keeper("stop");
  }
  throw InternalError("unknown covergroup method");
}

template <ExprLowerer Lowerer>
auto CovergroupOptionPath(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupOptionExpr& option) -> diag::Result<AccessPath> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const CoverageLibrary library(unit_lowerer.Hir());
  // The member `field` of the options structure `path` names, of `type`.
  const auto member = [&](AccessPath path, hir::TypeId type) -> AccessPath {
    return OptionMember(unit_lowerer, std::move(path), type, option.field);
  };
  // The library property `name` of `cls` of the object `object` holds, and
  // the type it holds in the library.
  const auto property = [&](mir::ExprId object, hir::ClassId cls,
                            std::string_view name) {
    const CoverageLibrary::Property found = library.PropertyOf(cls, name);
    return std::pair{
        AccessPath{
            .owner =
                ObjectProperty{
                    .object = object,
                    .property = LibraryField(unit_lowerer, found),
                    .type = unit_lowerer.TranslateType(found.type)},
            .descent = {}},
        found.type};
  };
  return std::visit(
      Overloaded{
          // A type's options (LRM 19.10), whatever names them.
          [&](const hir::CovergroupTypeReceiver& of_type)
              -> diag::Result<AccessPath> {
            const mir::ExprId type = TypeRecordCall(
                lowerer, frame, option.covergroup, of_type.declaring_instance);
            hir::ClassId cls = library.Refs().group_type;
            if (!option.item.has_value()) {
              auto [path, held] = property(type, cls, "type_option");
              return member(std::move(path), held);
            }
            const bool is_cross = ItemOf(
                                      unit_lowerer, library.Refs(),
                                      option.covergroup, *option.item)
                                      .is_cross;
            auto [table, held] = property(
                type, cls,
                is_cross ? "cross_type_options" : "coverpoint_type_options");
            auto [options, options_type] = ItemTypeOptions(
                unit_lowerer, std::move(table), held,
                BuildStringValueExpr(unit, block, *option.item));
            return member(std::move(options), options_type);
          },
          // An instance's (LRM 19.7).
          [&](const hir::CovergroupInstanceReceiver& through)
              -> diag::Result<AccessPath> {
            auto object = BuildReceivedInstance(
                lowerer, frame, through, option.covergroup);
            if (!object) return std::unexpected(std::move(object.error()));
            const Keeper keeper = KeeperOf(
                unit_lowerer, frame, option.covergroup, *object, option.item);
            auto [path, held] = property(keeper.object, keeper.cls, "option");
            return member(std::move(path), held);
          }},
      option.receiver);
}

template <ExprLowerer Lowerer>
auto LowerCoverageDatabaseCall(
    Lowerer& lowerer, const WalkFrame& frame,
    support::CoverageDatabaseCall database, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const CoverageLibrary library(unit_lowerer.Hir());
  const std::string_view name = [&] {
    switch (database) {
      case support::CoverageDatabaseCall::kGetCoverage:
        return "get_coverage";
      case support::CoverageDatabaseCall::kSetCoverageDbName:
        return "set_coverage_db_name";
      case support::CoverageDatabaseCall::kLoadCoverageDb:
        return "load_coverage_db";
    }
    throw InternalError("unknown coverage database function");
  }();
  const CoverageLibrary::Method method =
      library.MethodOf(library.Refs().registry, name);
  auto bound = BindLibraryActuals(
      lowerer, frame, method, GivenActuals(call.arguments), 0);
  if (!bound) return std::unexpected(std::move(bound.error()));
  return frame.current_block->exprs.Get(BuildMethodCall(
      unit_lowerer, frame, method.callee, std::nullopt, *bound,
      method.prototype.result_type));
}

template auto LowerCovergroupNewExpr(
    ProcessLowerer&, const WalkFrame&, const hir::CovergroupNewExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerCovergroupNewExpr(
    const StructuralScopeLowerer&, const WalkFrame&,
    const hir::CovergroupNewExpr&, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerCovergroupMethodCallExpr(
    ProcessLowerer&, const WalkFrame&, const hir::CovergroupMethodCallExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerCovergroupMethodCallExpr(
    const StructuralScopeLowerer&, const WalkFrame&,
    const hir::CovergroupMethodCallExpr&) -> diag::Result<mir::Expr>;
template auto CovergroupOptionPath(
    ProcessLowerer&, const WalkFrame&, const hir::CovergroupOptionExpr&)
    -> diag::Result<AccessPath>;
template auto CovergroupOptionPath(
    const StructuralScopeLowerer&, const WalkFrame&,
    const hir::CovergroupOptionExpr&) -> diag::Result<AccessPath>;
template auto LowerCoverageDatabaseCall(
    ProcessLowerer&, const WalkFrame&, support::CoverageDatabaseCall,
    const hir::CallExpr&) -> diag::Result<mir::Expr>;
template auto LowerCoverageDatabaseCall(
    const StructuralScopeLowerer&, const WalkFrame&,
    support::CoverageDatabaseCall, const hir::CallExpr&)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
