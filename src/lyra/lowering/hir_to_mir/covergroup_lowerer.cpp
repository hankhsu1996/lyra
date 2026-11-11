#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"

#include <array>
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
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_code.hpp"
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
#include "lyra/mir/builder.hpp"
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
#include "lyra/support/library_decl.hpp"
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

// The member at `member` of the library's options structure `path` names,
// whose type in the library is `type`.
auto OptionMember(
    UnitLowerer& unit_lowerer, AccessPath path, hir::TypeId type,
    base::ComponentIndex member) -> AccessPath {
  const hir::CompilationUnit& hir = unit_lowerer.Hir();
  const hir::UnpackedStructDefinition& structure =
      hir.DefinitionOf(hir.types.Get(type).Get<hir::UnpackedStructType>());
  if (member.value >= structure.fields.size()) {
    throw InternalError(
        "an option names a member the library's options structure lacks -- "
        "please report this as a bug");
  }
  return DescendInto(
      std::move(path), DescentStep{
                           .to = StepToComponent{.position = member},
                           .part_type = unit_lowerer.TranslateType(
                               structure.fields[member.value].type)});
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
      library.MethodOf(library.Members().registry.type_named)
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
         hir::RootsAnObject(unit_lowerer.Hir().definition);
}

// Where an object holds the item `item` names: the class of the lineage
// declaring it, its property there, and the library class counting it.
struct HeldItem {
  CovergroupClass owner;
  std::uint32_t property;
  hir::ClassId library_class;
};

auto HeldItemOf(
    const UnitLowerer& unit_lowerer, const CoverageLibrary& library,
    const hir::CoverageItemRef& item) -> HeldItem {
  return std::visit(
      Overloaded{
          [&](const hir::CoverpointRef& point) {
            CovergroupClass cls =
                CovergroupClassOf(unit_lowerer, point.covergroup);
            const std::uint32_t property =
                cls.layout.coverpoints.at(point.coverpoint.value);
            const hir::ClassId counted_by =
                cls.published->coverpoints.at(point.coverpoint.value)
                    .library_class;
            return HeldItem{
                .owner = std::move(cls),
                .property = property,
                .library_class = counted_by};
          },
          [&](const hir::CrossRef& cross) {
            CovergroupClass cls =
                CovergroupClassOf(unit_lowerer, cross.covergroup);
            const std::uint32_t property =
                cls.layout.crosses.at(cross.cross.value);
            return HeldItem{
                .owner = std::move(cls),
                .property = property,
                .library_class = library.Members().cross.cls};
          }},
      item);
}

// How many items one covergroup declares itself.
auto ItemCountOf(const hir::PublishedCovergroup& published) -> std::uint32_t {
  return static_cast<std::uint32_t>(published.coverpoints.size()) +
         published.cross_count;
}

// The place of `item` among every item of its covergroup's lineage, as an
// instance's record holds them (LRM 19.4.1): the first covergroup's
// coverpoints then its crosses, then the next one's, to the one declaring it.
auto LineagePosition(
    const UnitLowerer& unit_lowerer, const hir::CoverageItemRef& item)
    -> std::uint32_t {
  const auto [owner, own_place] = std::visit(
      Overloaded{
          [&](const hir::CoverpointRef& point) {
            return std::pair{point.covergroup, point.coverpoint.value};
          },
          [&](const hir::CrossRef& cross) {
            return std::pair{
                cross.covergroup,
                static_cast<std::uint32_t>(
                    PublishedOf(unit_lowerer, cross.covergroup)
                        .coverpoints.size()) +
                    cross.cross.value};
          }},
      item);
  std::uint32_t before = 0;
  for (std::optional<hir::CovergroupId> at =
           PublishedOf(unit_lowerer, owner).base;
       at.has_value(); at = PublishedOf(unit_lowerer, *at).base) {
    before += ItemCountOf(PublishedOf(unit_lowerer, *at));
  }
  return before + own_place;
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
  [[nodiscard]] auto Block() const -> mir::Builder {
    return frame_.Builder();
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
    return Block().AsValue(Unit().types, Block().Add(*std::move(lowered)));
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

  // The library's members, and those of `counted_by`, the class counting one
  // of the unit's coverpoints.
  [[nodiscard]] auto Members() const -> const hir::CoverageLibraryMembers& {
    return library_->Members();
  }
  [[nodiscard]] auto CoverpointClass(hir::ClassId counted_by) const
      -> const hir::CoverpointClassMembers& {
    return library_->CoverpointClass(counted_by);
  }
  [[nodiscard]] auto IntegralCoverpointClass(hir::ClassId counted_by) const
      -> const hir::IntegralCoverpointClassMembers& {
    return library_->IntegralCoverpointClass(counted_by);
  }

  // The value `enumerator` of one of the library's enumerations.
  [[nodiscard]] auto LibraryEnum(const hir::LibraryEnumerator& enumerator) const
      -> mir::ExprId {
    return mir::BuildIntegralLiteral(
        Unit(), Block(), unit_lowerer_->TranslateType(enumerator.type),
        LowerHirIntegralConstant(enumerator.value));
  }

  void Do(mir::ExprId expr) const {
    Block().AppendStmt(mir::ExprStmt{.expr = expr});
  }
  void Do(mir::Expr expr) const {
    Do(Block().Add(std::move(expr)));
  }

  // The library method `method`, called on what the handle `receiver` holds,
  // or on nothing for a type-associated one.
  [[nodiscard]] auto Call(
      std::optional<mir::ExprId> receiver, const hir::ClassMethodTarget& method,
      std::vector<mir::ExprId> arguments = {}) const -> mir::ExprId {
    const CoverageLibrary::Method called = library_->MethodOf(method);
    return BuildMethodCall(
        *unit_lowerer_, frame_, called.callee, receiver, arguments,
        called.prototype.result_type);
  }

  // The library property `property` of the object the handle `handle` holds.
  [[nodiscard]] auto LibraryProperty(
      mir::ExprId handle, const hir::ClassPropertyTarget& property) const
      -> AccessPath {
    const CoverageLibrary::Property found = library_->PropertyOf(property);
    const mir::ClassFieldTarget field = LibraryField(*unit_lowerer_, found);
    const mir::TypeId type = unit_lowerer_->TranslateType(found.type);
    return ObjectPropertyPath(handle, field, type);
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
    const mir::ExprId self = Block().Add(MakeSelfRefExpr(frame_));
    const mir::ClassFieldTarget property =
        CovergroupPropertyTarget(*unit_lowerer_, cls, index);
    const mir::TypeId type = CovergroupPropertyType(*unit_lowerer_, cls, index);
    return ObjectPropertyPath(self, property, type);
  }

  // The member at `member` of the unpacked structure `path` names, whose type
  // in the library is `type`.
  [[nodiscard]] auto Member(
      AccessPath path, hir::TypeId type, base::ComponentIndex member) const
      -> AccessPath {
    return OptionMember(*unit_lowerer_, std::move(path), type, member);
  }

  [[nodiscard]] auto Read(const AccessPath& path) const -> mir::ExprId {
    return PathValue(Unit(), Block(), path);
  }

  void Store(const AccessPath& path, mir::ExprId value) const {
    const mir::TypeId type = PathValueType(Unit(), Block().Body(), path);
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
            .then_scope = Block().AddScope(std::move(then_block)),
            .else_scope = Block().AddScope(std::move(else_block))});
  }

  // Runs `visit` on each element of the array `array` holds, in order, the
  // array evaluated once, here.
  [[nodiscard]] auto ForEach(
      mir::ExprId array,
      const std::function<diag::Result<void>(const Writer&, mir::ExprId)>&
          visit) const -> diag::Result<void> {
    const mir::ExprId value = Block().AsValue(Unit().types, array);
    const mir::TypeId type = Block().Get(value).type;
    const mir::LocalId held = DeclareLocal(frame_, value);
    const WalkedArray walked = WalkedArrayOf(
        unit_lowerer_->Unit(), frame_, Block().LocalPlace(held, type),
        std::nullopt);
    auto loop = BuildEntryLoopStmt(
        unit_lowerer_->Unit(), frame_, walked, WalkOrder::kFromFirst,
        std::nullopt,
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
  mir::Builder block = e.Block();
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
    const mir::ExprId value = block.AsValue(unit.types, values[i]);
    const mir::TypeId formal = e.Lowering().TranslateType(types.at(i));
    arguments.push_back(ConvertToType(unit, block, value, formal));
  }
  arguments.push_back(BuildReportHandedOn(unit, e.Frame()));
  const mir::TypeId handle = HandleTo(e.Lowering(), cls);
  return block.Add(
      mir::MakeNewExpr(
          block.Add(
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
      e.Block().Add(BuildCurrentRuntimeCallExpr(e.Lowering().Unit()));
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
  const hir::CoverageLibraryMembers& library =
      *unit_lowerer.Hir().coverage_library;
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
            return HandleTo(unit_lowerer, library.group_instance.cls);
          },
          [&](const CoverpointProperty& coverpoint) {
            return HandleTo(
                unit_lowerer,
                published.coverpoints.at(coverpoint.coverpoint).library_class);
          },
          [&](const CrossProperty&) {
            return HandleTo(unit_lowerer, library.cross.cls);
          }},
      cls.layout.properties.at(index).role);
}

namespace {

// What every unit states of the class `cls` is written as from what its unit
// published alone: what it extends, and each property where the layout every
// unit computes places it.
void StateCovergroupLayout(
    UnitLowerer& unit_lowerer, const CovergroupClass& cls, mir::Class& stated) {
  stated.base = cls.published->base.has_value()
                    ? mir::ClassRef{unit_lowerer.CovergroupClassIdentity(
                          *cls.published->base)}
                    : mir::ClassRef{mir::ClassObjectRootRef{}};
  for (std::size_t i = 0; i < cls.layout.properties.size(); ++i) {
    stated.named_fields.push_back(
        mir::NamedField{
            .name = cls.layout.properties[i].name,
            .slot = stated.fields.Add(
                mir::FieldDecl{
                    .type = CovergroupPropertyType(
                        unit_lowerer, cls, static_cast<std::uint32_t>(i))})});
  }
}

}  // namespace

auto PublishedCovergroupClass(
    UnitLowerer& unit_lowerer, hir::CovergroupId published) -> mir::Class {
  const hir::CovergroupKey& key =
      unit_lowerer.Hir().covergroups.KeyOf(published);
  const CovergroupClass cls = CovergroupClassOf(unit_lowerer, published);
  const hir::PublishedCovergroup& stated = *cls.published;
  mir::Class declared = unit_lowerer.IncompleteClass(
      unit_lowerer.CovergroupClassIdentity(published), key.unit_name, key.path);
  StateCovergroupLayout(unit_lowerer, cls, declared);
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
        // Every body the class is written of is lowered from the covergroup.
        owner_(owner.WithSpan(decl.span)),
        type_event_(&type_event),
        library_(unit_lowerer.Hir()),
        own_(CovergroupClassOf(unit_lowerer, covergroup)),
        first_(CovergroupClassOf(
            unit_lowerer, FirstOf(unit_lowerer, covergroup))) {
  }

  auto Write() -> diag::Result<void> {
    for (const CovergroupOperation& operation : CovergroupOperations()) {
      cls_->named_callables.push_back(
          mir::NamedCallable{
              .name = std::string{operation.name}, .body = operation.callable});
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

  [[nodiscard]] auto Members() const -> const hir::CoverageLibraryMembers& {
    return library_.Members();
  }

  [[nodiscard]] auto Self() const -> hir::CovergroupId {
    return own_.covergroup;
  }

  [[nodiscard]] auto Shape() const -> const ClassShape& {
    return u_->GetClassShape(class_id_);
  }

  [[nodiscard]] auto Lowerer() const -> ProcessLowerer {
    return {*u_,    declaring_scope_, decl_->body, std::nullopt,
            owner_, *scopes_,         {}};
  }

  // The name the run's registry knows the covergroup's type by (LRM 19.11.3).
  // A covergroup a scope of the design hierarchy declares is a type of each
  // instance of that scope (LRM 6.22, 19.3), so it is named by its
  // hierarchical name (LRM 23.6), whose part below the instance its text
  // fixes and whose instance answers the rest at run time. One no instance
  // holds is named by its declaration.
  [[nodiscard]] auto TypeName(const Writer& e) const -> mir::ExprId {
    const support::DefPath& path = own_.published->path;
    const std::string own_name =
        support::DisplayOf(support::DefPath{.data = {path.data.back()}});
    return std::visit(
        Overloaded{
            [&](const hir::NameBelowInstance& below) {
              return BuildNameBelowInstanceExpr(
                  Unit(), e.Frame(), mir::EnclosingHops{below.scopes_out},
                  below.text.empty()
                      ? own_name
                      : std::format("{}.{}", below.text, own_name));
            },
            [&](const hir::WholeName&) {
              return e.String(
                  std::format(
                      "{}::{}", u_->Hir().source_name,
                      support::DisplayOf(path)));
            }},
        decl_->declared_in);
  }

  // The record of the instance, kept by the first class of the lineage.
  [[nodiscard]] auto Record(const Writer& e) const -> AccessPath {
    return e.Own(first_, *first_.layout.record);
  }

  // The item `ref` names, of the running object.
  [[nodiscard]] auto Item(const Writer& e, const hir::CoverageItemRef& ref)
      const -> std::pair<AccessPath, HeldItem> {
    HeldItem item = HeldItemOf(*u_, library_, ref);
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
    const mir::DirectTarget target = TypeMethod(*u_, of, kTypeRecord);
    const mir::TypeId record = TypeRecordType(*u_, library_);
    return BuildFunctionCall(
        Unit(), e.Frame(), mir::Direct{.target = target}, std::move(arguments),
        record);
  }

  // A virtual method of the lineage, the first class's `behavior`, called on
  // the running object.
  [[nodiscard]] auto CallVirtual(
      const Writer& e, std::uint32_t behavior,
      std::vector<mir::ExprId> arguments,
      std::optional<mir::TypeId> result) const -> mir::ExprId {
    mir::Builder block = e.Block();
    const mir::VirtualSlot slot = IntroducedSlot(*u_, Self(), behavior);
    const mir::ExprId self = block.Add(MakeSelfRefExpr(e.Frame()));
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
    mir::Builder block = e.Block();
    for (std::size_t i = 0; i < decl_->formals.size(); ++i) {
      const hir::CovergroupFormal& formal = decl_->formals[i];
      const std::uint32_t index = first_.layout.formals.at(i);
      const mir::TypeId value =
          u_->TranslateType(decl_->body.procedural_vars.Get(formal.var).type);
      if (formal.direction == hir::ParamDirection::kRef ||
          formal.direction == hir::ParamDirection::kConstRef) {
        const mir::TypeId type = *ParamTypeOf(unit, value, formal.direction);
        const mir::ExprId self = block.Add(MakeSelfRefExpr(e.Frame()));
        const mir::ClassFieldTarget property =
            CovergroupPropertyTarget(*u_, first_, index);
        const mir::TypeId property_type =
            CovergroupPropertyType(*u_, first_, index);
        const mir::ExprId held = block.Add(
            PropertyStorage(unit, block, self, property, property_type));
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
      const mir::TypeId type = CovergroupPropertyType(*u_, own_, index);
      const mir::ExprId embedding = e.Read(e.Own(own_, index));
      e.Bind(*decl_->embedding, type, embedding);
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
      e.Store(e.Member(options, type, setting.member), *value);
    }
    return {};
  }

  // The options `option` names of the item or record `keeper` holds.
  [[nodiscard]] auto OptionsOf(
      const Writer& e, mir::ExprId keeper,
      const hir::ClassPropertyTarget& option) const
      -> std::pair<AccessPath, hir::TypeId> {
    return {
        e.LibraryProperty(keeper, option), library_.PropertyOf(option).type};
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
                  item,
                  wildcard ? e.IntegralCoverpointClass(cls).add_wildcard
                           : e.CoverpointClass(cls).add_value,
                  {*v}));
              return {};
            },
            [&](const hir::BinRange& range) -> diag::Result<void> {
              if (!range.low.has_value() && !range.high.has_value()) {
                e.Do(e.Call(item, e.CoverpointClass(cls).add_every_value));
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
                e.Do(e.Call(
                    item, e.CoverpointClass(cls).add_range_from_least,
                    {*high}));
              } else if (!high.has_value()) {
                e.Do(e.Call(
                    item, e.CoverpointClass(cls).add_range_to_greatest,
                    {*low}));
              } else {
                e.Do(e.Call(
                    item, e.CoverpointClass(cls).add_range, {*low, *high}));
              }
              return {};
            },
            [&](const hir::BinTolerance& tolerance) -> diag::Result<void> {
              auto center = e.Value(tolerance.center);
              if (!center) return std::unexpected(std::move(center.error()));
              auto spread = e.Value(tolerance.tolerance);
              if (!spread) return std::unexpected(std::move(spread.error()));
              e.Do(e.Call(
                  item, e.CoverpointClass(cls).add_tolerance,
                  {*center, *spread, e.Bit(tolerance.in_percent)}));
              return {};
            }},
        element);
  }

  // The bins one construct of a coverpoint makes, numbered `construct` (LRM
  // 19.5.1, 19.5.2).
  [[nodiscard]] auto WriteBins(
      const Writer& e, const hir::CoverpointRef& point, hir::ClassId cls,
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
                  item(e),
                  e.IntegralCoverpointClass(cls).add_default_sequence_bin,
                  {number, name}));
              return {};
            },
            [&](const hir::DefaultBins& bins) -> diag::Result<void> {
              const mir::ExprId point_item = item(e);
              const mir::ExprId per_value = e.Bit(bins.per_value);
              e.Do(e.Call(
                  point_item, e.CoverpointClass(cls).add_default_bin,
                  {number, name, per_value}));
              return {};
            },
            [&](const hir::TransitionBins& bins) -> diag::Result<void> {
              const mir::ExprId point_item = item(e);
              const mir::ExprId kind = e.LibraryEnum(
                  LibraryKindOf(e.Members().enumerators, bin.kind));
              const mir::ExprId per_sequence = e.Bit(bins.per_sequence);
              e.Do(e.Call(
                  point_item,
                  e.IntegralCoverpointClass(cls).begin_transition_bins,
                  {number, name, kind, per_sequence}));
              for (const std::vector<hir::TransitionStep>& sequence :
                   bins.sequences) {
                e.Do(e.Call(
                    item(e), e.IntegralCoverpointClass(cls).begin_sequence));
                for (const hir::TransitionStep& step : sequence) {
                  e.Do(e.Call(
                      item(e), e.IntegralCoverpointClass(cls).begin_step));
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
                  const mir::TypeId count = e.Block().Get(least).type;
                  const mir::LocalId kept = DeclareLocal(e.Frame(), least);
                  mir::ExprId most = e.Block().ReadLocal(kept, count);
                  if (step.most.has_value()) {
                    auto written = e.Value(*step.most);
                    if (!written) {
                      return std::unexpected(std::move(written.error()));
                    }
                    most = *written;
                  }
                  const mir::ExprId step_item = item(e);
                  const mir::ExprId repetition =
                      e.LibraryEnum(LibraryRepetitionOf(
                          e.Members().enumerators, step.repetition));
                  const mir::ExprId least_kept =
                      e.Block().ReadLocal(kept, count);
                  e.Do(e.Call(
                      step_item, e.IntegralCoverpointClass(cls).end_step,
                      {repetition, least_kept, most}));
                }
                e.Do(e.Call(
                    item(e), e.IntegralCoverpointClass(cls).end_sequence));
              }
              e.Do(e.Call(
                  item(e), e.IntegralCoverpointClass(cls).end_transition_bins));
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
              const mir::ExprId point_item = item(e);
              const mir::ExprId kind = e.LibraryEnum(
                  LibraryKindOf(e.Members().enumerators, bin.kind));
              const mir::ExprId array = e.LibraryEnum(
                  LibraryArrayOf(e.Members().enumerators, bins.array));
              e.Do(e.Call(
                  point_item, e.CoverpointClass(cls).begin_bins,
                  {number, name, kind, array, count}));
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
                          item(at),
                          bins.wildcard
                              ? at.IntegralCoverpointClass(cls).add_wildcard
                              : at.CoverpointClass(cls).add_value,
                          {value}));
                      return {};
                    });
                if (!added) return added;
              }
              // A bin naming the coverpoint alone stands for all its values
              // (LRM 19.5.1.1).
              if (bins.elements.empty() && !bins.set.has_value()) {
                e.Do(e.Call(item(e), e.CoverpointClass(cls).add_every_value));
              }
              // A `with` filter keeps those of the values given that it holds
              // for, each value being the filter's `item` in turn (LRM
              // 19.5.1.1).
              if (bins.filter.has_value()) {
                const hir::BinFilter& filter = *bins.filter;
                const mir::TypeId item_type = u_->TranslateType(
                    decl_->body.procedural_vars.Get(filter.item).type);
                const mir::ExprId pending = e.Call(
                    item(e), e.IntegralCoverpointClass(cls).pending_values);
                auto kept = e.ForEach(
                    pending,
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
                      const mir::ExprId point_item = item(at);
                      const mir::ExprId candidate =
                          at.Block().ReadLocal(held, item_type);
                      at.Do(at.Call(
                          point_item,
                          at.IntegralCoverpointClass(cls).keep_value,
                          {candidate, *keeps}));
                      return {};
                    });
                if (!kept) return kept;
              }
              e.Do(e.Call(item(e), e.CoverpointClass(cls).end_bins));
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
      const Writer& e, const hir::CrossRef& self, const hir::Cross& cross,
      hir::CrossSelectId id) const -> diag::Result<mir::ExprId> {
    const auto item = [&](const Writer& at) {
      return at.Read(Item(at, self).first);
    };
    // The coverpoint of the running object one of the cross's dimensions is,
    // read once for every call a selection makes on it.
    const auto point = [&](const Writer& at, const hir::CoverpointRef& target) {
      auto [path, found] = Item(at, target);
      return std::pair{
          EvaluatedOnce(at.Frame(), at.Read(path)), found.library_class};
    };
    const auto& select = cross.selects.Get(id);
    return std::visit(
        Overloaded{
            [&](const hir::SelectAll&) -> diag::Result<mir::ExprId> {
              return e.Call(item(e), e.Members().cross.select_all);
            },
            [&](const hir::SelectBinsOf& condition)
                -> diag::Result<mir::ExprId> {
              const auto [handle, counted_by] = point(e, condition.point);
              mir::ExprId numbers =
                  condition.construct.has_value()
                      ? e.Call(
                            handle, e.Members().coverpoint.numbers_of_construct,
                            {e.Int(condition.construct->value)})
                      : e.Call(handle, e.Members().coverpoint.all_numbers);
              if (!condition.intersect.empty()) {
                e.Do(e.Call(
                    handle, e.CoverpointClass(counted_by).begin_selection));
                for (const hir::BinElement& element : condition.intersect) {
                  if (auto r =
                          WriteElement(e, handle, counted_by, false, element);
                      !r) {
                    return std::unexpected(std::move(r.error()));
                  }
                }
                numbers = e.Call(
                    std::nullopt, e.Members().cross.both_numbers,
                    {numbers,
                     e.Call(
                         handle, e.CoverpointClass(counted_by).selected_bins)});
              }
              const auto dimension =
                  std::ranges::find(cross.targets, condition.point) -
                  cross.targets.begin();
              const mir::ExprId cross_item = item(e);
              const mir::ExprId position = e.Int(dimension);
              return e.Call(
                  cross_item, e.Members().cross.select_numbers,
                  {position, numbers});
            },
            [&](const hir::SelectNot& negated) -> diag::Result<mir::ExprId> {
              auto operand = WriteSelection(e, self, cross, negated.operand);
              if (!operand) return operand;
              return e.Call(
                  item(e), e.Members().cross.select_outside, {*operand});
            },
            [&](const hir::SelectAnd& both) -> diag::Result<mir::ExprId> {
              auto left = WriteSelection(e, self, cross, both.left);
              if (!left) return left;
              auto right = WriteSelection(e, self, cross, both.right);
              if (!right) return right;
              return e.Call(*left, e.Members().selection.both, {*right});
            },
            [&](const hir::SelectOr& either) -> diag::Result<mir::ExprId> {
              auto left = WriteSelection(e, self, cross, either.left);
              if (!left) return left;
              auto right = WriteSelection(e, self, cross, either.right);
              if (!right) return right;
              return e.Call(*left, e.Members().selection.either, {*right});
            },
            [&](const hir::SelectWith& with) -> diag::Result<mir::ExprId> {
              // Evaluated as the instance is made over every value tuple of
              // every tuple the subordinate expression selects, each answer
              // handed to the cross (LRM 19.6.1.2).
              auto candidates = WriteSelection(e, self, cross, with.candidates);
              if (!candidates) return candidates;
              e.Do(e.Call(
                  item(e), e.Members().cross.begin_filter, {*candidates}));
              const mir::ExprId candidate_tuples =
                  e.Call(item(e), e.Members().cross.candidate_tuples);
              auto offered = e.ForEach(
                  candidate_tuples,
                  [&](const Writer& at,
                      mir::ExprId candidate) -> diag::Result<void> {
                    const mir::TypeId tuple_type =
                        at.Block().Get(candidate).type;
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
                        const mir::ExprId cross_item = item(in);
                        const mir::ExprId offered_tuple =
                            in.Block().ReadLocal(tuple, tuple_type);
                        in.Do(in.Call(
                            cross_item, in.Members().cross.offer,
                            {offered_tuple, *filter}));
                        return {};
                      }
                      const auto [handle, counted_by] =
                          point(in, cross.targets[d]);
                      const mir::ExprId cross_item = item(in);
                      const mir::ExprId held_tuple =
                          in.Block().ReadLocal(tuple, tuple_type);
                      const mir::ExprId dimension =
                          in.Int(static_cast<std::int64_t>(d));
                      const mir::ExprId bin = in.Call(
                          cross_item, in.Members().cross.component,
                          {held_tuple, dimension});
                      const mir::ExprId values = in.Call(
                          handle,
                          in.IntegralCoverpointClass(counted_by).values_in_bin,
                          {bin});
                      return in.ForEach(
                          values,
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
              return e.Call(item(e), e.Members().cross.end_filter, {*least});
            },
            [&](const hir::SelectSet& set) -> diag::Result<mir::ExprId> {
              // A queue of value tuples, evaluated as the instance is made,
              // each counted toward every tuple whose bins hold it (LRM
              // 19.6.1.4).
              const mir::ExprId filter_item = item(e);
              const mir::ExprId all_item = item(e);
              const mir::ExprId all =
                  e.Call(all_item, e.Members().cross.select_all);
              e.Do(e.Call(filter_item, e.Members().cross.begin_filter, {all}));
              auto tuples = e.Value(set.tuples);
              if (!tuples) return tuples;
              auto counted = e.ForEach(
                  *tuples,
                  [&](const Writer& at,
                      mir::ExprId value) -> diag::Result<void> {
                    const mir::TypeId tuple_type = at.Block().Get(value).type;
                    const mir::LocalId tuple =
                        at.Frame().bindings->DeclareAnonymous(tuple_type);
                    at.Block().AppendStmt(
                        mir::LocalDeclStmt{.target = tuple, .init = value});
                    const std::span<const mir::TypeId> fields =
                        mir::RecordElements(
                            Unit(),
                            *mir::RecordOf(Unit().types.Get(tuple_type)));
                    for (std::size_t d = 0; d < cross.targets.size(); ++d) {
                      const auto [handle, counted_by] =
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
                      const mir::ExprId cross_item = item(at);
                      const mir::ExprId contained = field();
                      const mir::ExprId numbers = at.Call(
                          handle,
                          at.CoverpointClass(counted_by).numbers_containing,
                          {contained});
                      const mir::ExprId named = field();
                      const mir::ExprId text = at.Call(
                          handle, at.CoverpointClass(counted_by).text_of,
                          {named});
                      at.Do(at.Call(
                          cross_item, at.Members().cross.value_component,
                          {numbers, text}));
                    }
                    at.Do(
                        at.Call(item(at), at.Members().cross.end_value_tuple));
                    return {};
                  });
              if (!counted) return std::unexpected(std::move(counted.error()));
              auto least = LeastMatches(e, set.matches);
              if (!least) return least;
              return e.Call(item(e), e.Members().cross.end_filter, {*least});
            }},
        select);
  }

  // The constructor (LRM 19.3): what `new` is given, the instance's record and
  // options, and each coverpoint's and cross's item with its bins, every
  // expression evaluated as the instance is made. A covergroup extending
  // another constructs the base's part first; an item the base builds that
  // this one replaces is built still, and its type says it does not count (LRM
  // 19.4.1). The first class of the lineage makes the record of the object's
  // own type, which a virtual method answers even while the base is
  // constructed (LRM 8.17), and starts the sampling.
  auto WriteConstructor() -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    mir::CallableCode code = mir::CallableCode::Defined(owner_.span);
    CallableBindings bindings(unit, code);
    BoundImplicitParameters bound = BindImplicitParameters(
        *u_,
        owner_.WithBlock(&code.Body())
            .WithBindings(&bindings)
            .WithRunsAsFunctionCall(true),
        Shape(), CallableForm::kConstructor);
    std::vector<mir::LocalId> params = std::move(bound.params);
    mir::Builder block = bound.frame.Builder();
    ProcessLowerer lowerer = Lowerer();

    // `new(embedding, formals...)`: the object embedding the covergroup and
    // what the source passed.
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
    for (const hir::CovergroupFormal& formal : decl_->formals) {
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
          const AccessPath property = e.Own(own_, index);
          const mir::ExprId input =
              block.ReadLocal(inputs[i]->first, inputs[i]->second);
          e.Store(property, input);
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
        const mir::ExprId object = BuildObjectDeref(
            unit, block, block.Add(MakeSelfRefExpr(e.Frame())));
        const mir::ClassFieldTarget property =
            CovergroupPropertyTarget(*u_, own_, index);
        const mir::TypeId property_type =
            CovergroupPropertyType(*u_, own_, index);
        e.Do(
            mir::MakeCallExpr(
                mir::Direct{.target = support::BuiltinFn::kBindReference},
                {block.AsValue(
                     unit.types, block.Add(
                                     mir::MakeFieldAccessExpr(
                                         object, property, property_type))),
                 PathReference(unit, block, *named)},
                unit.builtins.void_type));
      }
      const AccessPath record = Record(e);
      const mir::TypeId record_type =
          CovergroupPropertyType(*u_, first_, *first_.layout.record);
      e.Store(record, CallVirtual(e, kMakeRecordBehavior, {}, record_type));
    }
    if (embedding.has_value()) {
      const AccessPath property = e.Own(own_, *own_.layout.embedding);
      const mir::ExprId embedded =
          block.ReadLocal(embedding->first, embedding->second);
      e.Store(property, embedded);
    }

    const auto [record_options, record_options_type] =
        OptionsOf(e, e.Read(Record(e)), Members().group_instance.option);
    if (auto r = WriteOptions(
            e, decl_->options, false, record_options, record_options_type);
        !r) {
      return r;
    }

    for (const hir::CoverpointId p : decl_->coverpoints.Ids()) {
      const hir::Coverpoint& point = decl_->coverpoints.Get(p);
      const hir::CoverpointRef self{.covergroup = Self(), .coverpoint = p};
      const hir::PublishedCoverpoint& counted_by =
          own_.published->coverpoints.at(p.value);
      hir::ClassId cls = counted_by.library_class;
      const AccessPath property =
          e.Own(own_, own_.layout.coverpoints.at(p.value));
      const std::uint32_t position = LineagePosition(*u_, self);
      const mir::ExprId made = NewLibraryObject(
          e, cls, {e.Read(Record(e)), e.String(point.name), e.Int(position)});
      e.Store(property, made);
      // The item is named by every statement setting it up, so it is read
      // once.
      const mir::ExprId item = EvaluatedOnce(e.Frame(), e.Read(property));
      const auto [options, options_type] =
          OptionsOf(e, item, Members().coverpoint.option);
      if (auto r = WriteOptions(e, point.options, false, options, options_type);
          !r) {
        return r;
      }
      // An enumeration's automatic bins are its values, named by theirs (LRM
      // 19.5.3).
      if (const auto* enumeration =
              u_->Hir().types.Get(counted_by.value_type).As<hir::EnumType>()) {
        for (const hir::EnumMember& member :
             u_->Hir().DefinitionOf(*enumeration).members) {
          e.Do(e.Call(
              item, e.IntegralCoverpointClass(cls).name_enumeration_value,
              {mir::BuildIntegralLiteral(
                   unit, block, u_->TranslateType(counted_by.value_type),
                   LowerHirIntegralConstant(member.value)),
               e.String(member.name)}));
        }
      }
      for (std::size_t b = 0; b < point.bins.size(); ++b) {
        if (auto r = WriteBins(
                e, self, cls, point.bins[b], static_cast<std::int64_t>(b));
            !r) {
          return r;
        }
      }
      e.Do(e.Call(item, e.CoverpointClass(cls).finish));
    }
    for (const hir::CrossId c : decl_->crosses.Ids()) {
      const hir::Cross& cross = decl_->crosses.Get(c);
      const hir::CrossRef self{.covergroup = Self(), .cross = c};
      hir::ClassId cls = Members().cross.cls;
      const AccessPath property = e.Own(own_, own_.layout.crosses.at(c.value));
      const std::uint32_t position = LineagePosition(*u_, self);
      const mir::ExprId made = NewLibraryObject(
          e, cls, {e.Read(Record(e)), e.String(cross.name), e.Int(position)});
      e.Store(property, made);
      // The item is named by every statement setting it up, so it is read
      // once.
      const mir::ExprId item = EvaluatedOnce(e.Frame(), e.Read(property));
      const auto [options, options_type] =
          OptionsOf(e, item, Members().cross.option);
      if (auto r = WriteOptions(e, cross.options, false, options, options_type);
          !r) {
        return r;
      }
      for (const hir::CoverpointRef& target : cross.targets) {
        e.Do(e.Call(
            item, e.Members().cross.cross_coverpoint,
            {e.Read(Item(e, target).first)}));
      }
      for (const hir::CrossBin& bin : cross.bins) {
        auto selection = WriteSelection(e, self, cross, bin.select);
        if (!selection) return std::unexpected(std::move(selection.error()));
        e.Do(e.Call(
            item, e.Members().cross.add_bin,
            {e.String(bin.name),
             e.LibraryEnum(LibraryKindOf(e.Members().enumerators, bin.kind)),
             *selection}));
      }
      e.Do(e.Call(item, e.Members().cross.finish));
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
  // belongs to, the object embedding it and the arguments.
  [[nodiscard]] auto ConstructBase(
      const Writer& e,
      const std::optional<std::pair<mir::LocalId, mir::TypeId>>& embedding,
      mir::LocalId report) const -> diag::Result<void> {
    mir::CompilationUnit& unit = Unit();
    mir::Builder block = e.Block();
    hir::CovergroupId base = *decl_->base;
    const hir::PublishedCovergroup& published = PublishedOf(*u_, base);
    std::vector<mir::ExprId> arguments;
    if (TakesDeclaringInstance(*u_, base)) {
      arguments = InstanceArgument(e);
    }
    if (published.embedding.has_value()) {
      const mir::ExprId embedded =
          block.ReadLocal(embedding->first, embedding->second);
      const mir::TypeId handle = HandleTo(*u_, *published.embedding);
      arguments.push_back(ConvertToType(unit, block, embedded, handle));
    }
    for (std::size_t i = 0; i < decl_->formals.size(); ++i) {
      const hir::CovergroupFormal& formal = decl_->formals[i];
      const hir::Expr named = hir::MakeRefExpr(
          hir::ProceduralVarRef{.var = formal.var},
          decl_->body.procedural_vars.Get(formal.var).type, diag::SourceSpan{});
      auto bound = EmitBoundActual(
          e.Body(), e.Frame(), named,
          *ParamPassingOf(published.formals.at(i).direction),
          u_->TranslateType(published.formals.at(i).type));
      if (!bound) return std::unexpected(std::move(bound.error()));
      arguments.push_back(*bound);
    }
    arguments.push_back(block.ReadLocal(report, unit.builtins.read_report_ptr));
    e.Do(
        mir::MakeCallExpr(
            mir::Direct{
                .target =
                    mir::ConstructorTarget{
                        .cls = u_->CovergroupClassIdentity(base)},
                .receiver = BuildObjectDeref(
                    unit, block, block.Add(MakeSelfRefExpr(e.Frame())))},
            std::move(arguments), unit.builtins.void_type));
    return {};
  }

  // The event a block of the scope or class declaring the covergroup triggers
  // for it (LRM 19.3): the record's own where a class embeds the covergroup,
  // and the one kept for the covergroup's type otherwise.
  [[nodiscard]] auto BlockEventOf(const Writer& e) const -> mir::ExprId {
    mir::CompilationUnit& unit = Unit();
    mir::Builder block = e.Block();
    if (decl_->embedding.has_value()) {
      return e.Read(e.LibraryProperty(
          e.Read(Record(e)), e.Members().group_instance.block_event));
    }
    const mir::TypeId cell_type =
        mir::ObservableCellOf(unit.types, unit.builtins.event);
    const mir::ExprId cell = block.Add(BuildStaticStorageAccess(
        unit, e.Frame(), **type_event_, cell_type, mir::EnclosingHops{}));
    return block.AsValue(
        unit.types, block.Add(block.WrapperContents(
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
          ProcessLowerer&, const Writer&, mir::Builder)>
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
                          mir::Builder block) {
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
                          mir::Builder block) {
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
        *u_, library_, lowerer, *decl_,
        closure.Frame().WithRunsAsFunctionCall(false));
    BindKept(inside);
    mir::Block each;
    const Writer round = inside.At(inside.Frame().WithBlock(&each));
    mir::Builder each_builder = round.Block();
    const auto sample = [&](const Writer& at) {
      at.Do(CallVirtual(at, kSampleBehavior, {}, std::nullopt));
    };
    auto waited = sampler->wait(lowerer, round, each_builder);
    if (!waited) return std::unexpected(std::move(waited.error()));
    each_builder.AppendStmt(*std::move(waited));
    if (sampler->strobes) {
      const mir::ExprId is_strobed = round.Call(
          round.Read(Record(round)), round.Members().group_instance.strobed);
      round.IfElse(
          is_strobed,
          [&](const Writer& strobed) {
            const mir::ExprId strobe_claimed = strobed.Call(
                strobed.Read(Record(strobed)),
                strobed.Members().group_instance.claim_strobe);
            strobed.If(strobe_claimed, [&](const Writer& claimed) {
              ClosureBuilder postponed(
                  unit, claimed.Frame(), support::ClosureKind::kFn,
                  CapturePolicy{});
              RunDeferredBodyOnNoThread(
                  claimed.Lowering(), claimed.Frame(), postponed);
              const Writer later = claimed.At(postponed.Frame());
              later.Do(later.Call(
                  later.Read(Record(later)),
                  later.Members().group_instance.strobe_taken));
              sample(later);
              mir::Expr keeper = BuildRegionEffectsCallExpr(
                  claimed.Lowering(), claimed.Block(),
                  support::BuiltinFn::kPostponedEffects);
              claimed.Do(BuildEmplacedClosure(
                  claimed.Lowering().Unit(), claimed.Frame(), std::move(keeper),
                  postponed.BuildVoid()));
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
            .scope = closure.Body().AddScope(std::move(each))});
    HandToRuntime(
        e, support::BuiltinFn::kRunObserving,
        e.Block().Add(closure.BuildCallOnce()));
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
    for (const hir::CovergroupFormal& formal : decl_->sample_formals) {
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
                  unit, e.Block(), e.Block().Add(MakeSelfRefExpr(e.Frame())))},
          std::move(forwarded), std::nullopt));
    }
    BindKept(e);
    diag::Result<void> written;
    const mir::ExprId is_collecting = e.Read(e.LibraryProperty(
        e.Read(Record(e)), e.Members().group_instance.collecting));
    e.If(is_collecting, [&](const Writer& collecting) {
      written = SampleItems(collecting);
    });
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
    const auto gates = [&](const hir::CoverageItemRef& item,
                           const auto& constructs,
                           const auto& gate) -> diag::Result<void> {
      for (std::size_t i = 0; i < constructs.size(); ++i) {
        if (!constructs[i].iff.has_value()) continue;
        auto guard = e.Value(*constructs[i].iff);
        if (!guard) return std::unexpected(std::move(guard.error()));
        const mir::ExprId object = e.Read(Item(e, item).first);
        const mir::ExprId construct = e.Int(static_cast<std::int64_t>(i));
        e.Do(gate(object, construct, *guard));
      }
      return {};
    };
    for (const hir::CoverpointId p : decl_->coverpoints.Ids()) {
      const hir::Coverpoint& point = decl_->coverpoints.Get(p);
      const hir::CoverpointRef self{.covergroup = Self(), .coverpoint = p};
      hir::ClassId cls = own_.published->coverpoints.at(p.value).library_class;
      if (auto r = gates(
              self, point.bins,
              [&](mir::ExprId object, mir::ExprId construct,
                  mir::ExprId guard) {
                return e.Call(
                    object, e.CoverpointClass(cls).gate, {construct, guard});
              });
          !r) {
        return r;
      }
      const auto count = [&](const Writer& at) -> diag::Result<void> {
        auto value = at.Value(point.expression);
        if (!value) return std::unexpected(std::move(value.error()));
        at.Do(at.Call(
            at.Read(at.Own(own_, own_.layout.coverpoints.at(p.value))),
            at.CoverpointClass(cls).sample, {*value}));
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
      const auto counts = [&](const Writer& at) { counted = count(at); };
      const auto skips = [&](const Writer& at) {
        at.Do(at.Call(
            at.Read(at.Own(own_, own_.layout.coverpoints.at(p.value))),
            at.Members().coverpoint.skip));
      };
      e.IfElse(*guard, counts, skips);
      if (!counted) return counted;
    }
    for (const hir::CrossId c : decl_->crosses.Ids()) {
      const hir::Cross& cross = decl_->crosses.Get(c);
      const hir::CrossRef self{.covergroup = Self(), .cross = c};
      if (auto r = gates(
              self, cross.bins,
              [&](mir::ExprId object, mir::ExprId construct,
                  mir::ExprId guard) {
                return e.Call(
                    object, e.Members().cross.gate, {construct, guard});
              });
          !r) {
        return r;
      }
      const auto count = [&](const Writer& at) {
        at.Do(at.Call(
            at.Read(at.Own(own_, own_.layout.crosses.at(c.value))),
            at.Members().cross.sample));
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
        e.Call(TypeRecordOf(e, Self()), e.Members().group_type.make_instance);
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
                std::nullopt, e.Members().registry.type_named, {TypeName(e)})});
    const AccessPath declared = e.LibraryProperty(
        e.Block().ReadLocal(found, type), e.Members().group_type.declared);
    const mir::ExprId undeclared = BuildConditionNot(
        unit, e.Block(), ReduceToCondition(unit, e.Block(), e.Read(declared)));
    mir::Block first;
    const Writer then = e.At(e.Frame().WithBlock(&first));
    mir::Builder first_builder = then.Block();
    const AccessPath now_declared = then.LibraryProperty(
        first_builder.ReadLocal(found, type),
        then.Members().group_type.declared);
    then.Store(now_declared, then.Bit(true));
    std::vector<mir::ExprId> arguments = InstanceArgument(then);
    arguments.push_back(first_builder.ReadLocal(found, type));
    then.Do(BuildFunctionCall(
        unit, then.Frame(),
        mir::Direct{
            .target =
                mir::CallableTarget{.owner = class_id_, .slot = kDeclareType}},
        std::move(arguments), std::nullopt));
    const mir::ExprId writing_claimed =
        then.Call(std::nullopt, then.Members().registry.claim_database_writing);
    then.If(writing_claimed, [&](const Writer& claimed) {
      ClosureBuilder at_end(
          unit, claimed.Frame(), support::ClosureKind::kFn, CapturePolicy{});
      const Writer last = claimed.At(at_end.Frame());
      last.Do(last.Call(std::nullopt, last.Members().registry.write_database));
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

  // What the declaration states of its type: its items, each at its place in
  // the lineage, the items of what it extends that they replace, which no
  // longer count toward the type, and the type options set in it (LRM 19.7),
  // after what it extends states (LRM 19.4.1).
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
    const auto declare = [&](const hir::ClassMethodTarget& method,
                             const hir::CoverageItemRef& item,
                             const std::string& name) {
      const mir::ExprId declaring = record();
      e.Do(e.Call(
          declaring, method,
          {e.Int(LineagePosition(*u_, item)), e.String(name)}));
    };
    const auto replaced = [&](const hir::CoverageItemRef& item) {
      const mir::ExprId declaring = record();
      e.Do(e.Call(
          declaring, Members().group_type.declare_replaced,
          {e.Int(LineagePosition(*u_, item))}));
    };
    for (const hir::CoverpointId p : decl_->coverpoints.Ids()) {
      declare(
          Members().group_type.declare_coverpoint,
          hir::CoverpointRef{.covergroup = Self(), .coverpoint = p},
          decl_->coverpoints.Get(p).name);
    }
    for (const hir::CrossId c : decl_->crosses.Ids()) {
      declare(
          Members().group_type.declare_cross,
          hir::CrossRef{.covergroup = Self(), .cross = c},
          decl_->crosses.Get(c).name);
    }
    for (const hir::Coverpoint& point : decl_->coverpoints) {
      if (point.replaces.has_value()) replaced(*point.replaces);
    }
    for (const hir::Cross& cross : decl_->crosses) {
      if (cross.replaces.has_value()) replaced(*cross.replaces);
    }
    if (auto r = WriteOptions(
            e, decl_->options, true,
            e.LibraryProperty(record(), e.Members().group_type.type_option),
            library_.PropertyOf(Members().group_type.type_option).type);
        !r) {
      return r;
    }
    // Each item's type options are the element of the type's table its place
    // in the lineage keys.
    const auto item_options = [&](const hir::ClassPropertyTarget& table,
                                  const hir::CoverageItemRef& item) {
      AccessPath options = e.LibraryProperty(record(), table);
      const mir::ExprId key = e.Int(LineagePosition(*u_, item));
      return ItemTypeOptions(
          *u_, std::move(options), library_.PropertyOf(table).type, key);
    };
    for (const hir::CoverpointId p : decl_->coverpoints.Ids()) {
      const auto [options, options_type] = item_options(
          Members().group_type.coverpoint_type_options,
          hir::CoverpointRef{.covergroup = Self(), .coverpoint = p});
      if (auto r = WriteOptions(
              e, decl_->coverpoints.Get(p).options, true, options,
              options_type);
          !r) {
        return r;
      }
    }
    for (const hir::CrossId c : decl_->crosses.Ids()) {
      const auto [options, options_type] = item_options(
          Members().group_type.cross_type_options,
          hir::CrossRef{.covergroup = Self(), .cross = c});
      if (auto r = WriteOptions(
              e, decl_->crosses.Get(c).options, true, options, options_type);
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
    UnitLowerer& unit_lowerer, const hir::ReplicatedCovergroup& covergroup,
    const StructuralScopeLowerer* declaring_scope)
    : unit_lowerer_(&unit_lowerer),
      covergroup_(covergroup.id),
      decl_(&covergroup.code),
      declaring_scope_(declaring_scope),
      class_id_(unit_lowerer.CovergroupClassIdentity(covergroup.id)) {
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
                  : mir::ClassRef{mir::ClassObjectRootRef{}},
      .implements = {},
      .self_pointer_type = unit_lowerer.SelfPointerTo(class_id_),
      .declaring_instance = std::nullopt,
      .scope_declarations = std::nullopt,
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
      .is_interface = false};
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
      mir::LibraryTypeOf(
          unit.types, support::LibraryDecl::kCancellationTarget));
  if (!unit_lowerer.HasClassShape(class_id_)) {
    unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  }
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
                ScopeThroughMember{.member = *shape.declaring_instance->member})
          : owner.WithStructuralBase(NoScope{});

  // The event kept for the type names a synchronization object of its own from
  // before any process runs, as a declared event does (LRM 6.17), brought up
  // with the rest of what replicates the declaration (LRM 10.5).
  if (type_event_.has_value()) {
    mir::Builder install = bring_up.install.Builder();
    const mir::TypeId cell_type =
        mir::ObservableCellOf(unit.types, unit.builtins.event);
    const mir::ExprId cell = install.Add(BuildStaticStorageAccess(
        unit, bring_up.install, *type_event_, cell_type, mir::EnclosingHops{}));
    const mir::ExprId fresh_event = install.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kEventNew}, {},
            unit.builtins.event));
    install.AppendStmt(
        mir::ExprStmt{
            .expr = install.Add(
                mir::MakeCapabilityInstallCallExpr(
                    cell, fresh_event, support::BuiltinFn::kInitialize,
                    unit.builtins.void_type))});
  }

  ClassWriter writer(
      unit_lowerer, covergroup_, *decl_, declaring_scope_, scopes_, cls,
      class_id_, owner, type_event_);
  if (auto r = writer.Write(); !r) return r;
  // A covergroup a design element's unit declares keeps its code in the body
  // realizing the scope its object's instance is, and its class is what every
  // body of the unit states alike.
  if (hir::RootsAnObject(unit_lowerer.Hir().definition)) {
    MoveCodeIntoBody(
        unit_lowerer, class_id_, cls, declaring_frame,
        declaring_scope_->PublishedClassId());
    if (!unit.classes.IsDefined(class_id_)) {
      unit.DefineClass(
          class_id_,
          StateCovergroupClassFromDeclaration(unit_lowerer, covergroup_));
    }
    return {};
  }
  if (!unit.classes.IsDefined(class_id_)) {
    unit.DefineClass(class_id_, std::move(cls));
  }
  return {};
}

auto CovergroupOperations() -> std::array<CovergroupOperation, 4> {
  return {
      CovergroupOperation{
          .callable = kSample,
          .name = kCovergroupSample,
          .form = CallableForm::kInstanceMember},
      CovergroupOperation{
          .callable = kMakeRecord,
          .name = kCovergroupMakeRecord,
          .form = CallableForm::kInstanceMember},
      CovergroupOperation{
          .callable = kTypeRecord,
          .name = kCovergroupTypeRecord,
          .form = CallableForm::kTypeAssociated},
      CovergroupOperation{
          .callable = kDeclareType,
          .name = kCovergroupDeclareType,
          .form = CallableForm::kTypeAssociated}};
}

auto CovergroupOperationPrototype(
    UnitLowerer& unit_lowerer, hir::CovergroupId covergroup,
    const ClassCodePoint& point) -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const CovergroupClass own = CovergroupClassOf(unit_lowerer, covergroup);
  const hir::PublishedCovergroup& published = *own.published;
  const mir::TypeId object = unit_lowerer.SelfPointerTo(
      unit_lowerer.CovergroupClassIdentity(covergroup));
  std::vector<mir::TypeId> params;
  // The instance a type-associated body is handed, where the class belongs to
  // one (LRM 6.22).
  const auto instance = [&] {
    if (TakesDeclaringInstance(unit_lowerer, covergroup)) {
      params.push_back(unit.builtins.scope_ptr);
    }
  };
  const auto formals =
      [&](const std::vector<hir::PublishedCovergroupFormal>& stated) {
        for (const hir::PublishedCovergroupFormal& formal : stated) {
          if (const std::optional<mir::TypeId> type = ParamTypeOf(
                  unit, unit_lowerer.TranslateType(formal.type),
                  formal.direction)) {
            params.push_back(*type);
          }
        }
      };
  // A synthesized function is handed where to report what it reads after
  // its formals, and answers its completion (LRM 9.4.2).
  const auto function = [&](std::optional<mir::TypeId> result) {
    params.push_back(unit.builtins.read_report_ptr);
    return OperationPrototype(
        params,
        SubroutineCallType(
            unit, hir::SubroutineKind::kFunction,
            CompletionPayloadType(
                unit, result.has_value() ? std::vector<mir::TypeId>{*result}
                                         : std::vector<mir::TypeId>{})));
  };
  const auto on_object = [](mir::CallableCode prototype) {
    prototype.receiver = prototype.params.front();
    return prototype;
  };
  const CoverageLibrary library(unit_lowerer.Hir());
  return std::visit(
      Overloaded{
          // `new(embedding, formals...)`, after the object and its instance,
          // and then where to report what it reads.
          [&](const ClassConstructorCode&) {
            params.push_back(object);
            instance();
            if (own.layout.embedding.has_value()) {
              params.push_back(CovergroupPropertyType(
                  unit_lowerer, own, *own.layout.embedding));
            }
            formals(published.formals);
            params.push_back(unit.builtins.read_report_ptr);
            return on_object(
                OperationPrototype(params, unit.builtins.void_type));
          },
          [&](const ClassCallableCode& code) {
            if (code.callable == kSample) {
              params.push_back(object);
              formals(published.sample_formals);
              return on_object(function(std::nullopt));
            }
            if (code.callable == kMakeRecord) {
              const CovergroupClass first = CovergroupClassOf(
                  unit_lowerer, FirstOf(unit_lowerer, covergroup));
              params.push_back(object);
              return on_object(function(CovergroupPropertyType(
                  unit_lowerer, first, *first.layout.record)));
            }
            if (code.callable == kTypeRecord) {
              instance();
              return function(TypeRecordType(unit_lowerer, library));
            }
            if (code.callable == kDeclareType) {
              instance();
              params.push_back(TypeRecordType(unit_lowerer, library));
              return function(std::nullopt);
            }
            throw InternalError(
                "CovergroupOperationPrototype: a covergroup's class has four "
                "operations besides its constructor -- please report this as "
                "a bug");
          }},
      point);
}

auto StateCovergroupClassFromDeclaration(
    UnitLowerer& unit_lowerer, hir::CovergroupId covergroup) -> mir::Class {
  const CovergroupClass own = CovergroupClassOf(unit_lowerer, covergroup);
  const hir::PublishedCovergroup& published = *own.published;
  const mir::ClassId id = unit_lowerer.CovergroupClassIdentity(covergroup);
  const mir::ClassId declaring =
      unit_lowerer.PublishedClassOf(*unit_lowerer.InstanceScopeOf(covergroup));
  mir::Class stated{};
  stated.identity = published.path;
  stated.self_pointer_type = unit_lowerer.SelfPointerTo(id);
  StateCovergroupLayout(unit_lowerer, own, stated);
  // Then the instance the class belongs to (LRM 6.22).
  const mir::TypeId instance_type = unit_lowerer.Unit().builtins.scope_ptr;
  const DeclaringInstance instance{
      .type = instance_type,
      .member = stated.fields.Add(mir::FieldDecl{.type = instance_type})};
  // `sample` and the record of the object's type take part in dispatch along
  // the lineage (LRM 19.4.1); the two methods of the class itself do not.
  const std::array<std::optional<mir::VirtualDispatchRole>, 4> roles{
      RoleIn(unit_lowerer, covergroup, kSampleBehavior),
      RoleIn(unit_lowerer, covergroup, kMakeRecordBehavior), std::nullopt,
      std::nullopt};
  for (const auto& [callable, name, form] : CovergroupOperations()) {
    stated.named_callables.push_back(
        mir::NamedCallable{.name = std::string{name}, .body = callable});
    const mir::CallableId added = stated.callables.Add(
        mir::CallableDecl{
            .code = EntryOf(
                unit_lowerer, id, instance,
                ClassCallableCode{.callable = callable}, form, declaring),
            .foreign = std::nullopt,
            .virtual_dispatch = roles.at(callable.value),
            .defining_unit = std::nullopt});
    if (added != callable) {
      throw InternalError(
          "StateCovergroupClassFromDeclaration: a covergroup's class holds its "
          "operations in the order it reserves them -- please report this as "
          "a bug");
    }
  }
  stated.constructor = mir::ConstructorDecl{
      .code = EntryOf(
          unit_lowerer, id, instance, ClassConstructorCode{},
          CallableForm::kConstructor, declaring)};
  return stated;
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
  mir::Builder block = frame.Builder();
  return block.Add(
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
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId handle)
    -> mir::ExprId {
  const mir::TypeId type = block.Get(handle).type;
  return ReduceToCondition(
      unit, block,
      BuildEquality(
          unit, block, support::ValueOperator::kInequality, handle,
          block.Add(mir::Expr{.data = mir::NullLiteral{}, .type = type}),
          unit.builtins.bit1));
}

}  // namespace

auto LowerCovergroupFactory(
    UnitLowerer& unit_lowerer, const WalkFrame& owner, const ClassShape& shape,
    hir::CovergroupId covergroup, const hir::CovergroupDecl& decl)
    -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::CovergroupId self = covergroup;
  const CovergroupClass own = CovergroupClassOf(unit_lowerer, self);
  SynthesizedFunction function(
      unit_lowerer, owner, shape, CallableForm::kInstanceMember);
  const WalkFrame& frame = function.Frame();
  mir::Builder block = function.Body();

  // What `new` of the covergroup is handed: the object and the arguments.
  std::vector<mir::ExprId> arguments;
  if (TakesDeclaringInstance(unit_lowerer, self)) {
    arguments.push_back(DeclaringInstanceOf(unit, frame));
  }
  const mir::ExprId this_handle = block.Add(BuildThisHandle(frame));
  const mir::TypeId embedding_handle =
      HandleTo(unit_lowerer, *own.published->embedding);
  arguments.push_back(
      ConvertToType(unit, block, this_handle, embedding_handle));
  for (const hir::CovergroupFormal& formal : decl.formals) {
    const mir::TypeId type = *ParamTypeOf(
        unit,
        unit_lowerer.TranslateType(
            decl.body.procedural_vars.Get(formal.var).type),
        formal.direction);
    arguments.push_back(block.ReadLocal(function.AddFormal(type), type));
  }
  arguments.push_back(BuildReportHandedOn(unit, frame));
  const mir::TypeId made_type = CovergroupHandle(unit_lowerer, self);
  const mir::LocalId made = function.Bindings().DeclareAnonymous(made_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = made,
          .init = block.Add(
              mir::MakeNewExpr(
                  block.Add(
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
    const hir::StructuralScope& replicating, BlockEventTriggers& triggers) {
  const hir::ClassDeclaration& holder =
      unit_lowerer.Hir().classes.Declaration(embedding);
  for (const hir::CovergroupId covergroup :
       hir::EmbeddedCovergroupsOf(holder.methods)) {
    const auto* at_blocks = std::get_if<hir::SampledAtBlocks>(
        &hir::CodeOf(replicating, covergroup).sampling);
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
  mir::Builder block = frame.Builder();
  for (const BlockEventTrigger& trigger : triggers) {
    std::visit(
        Overloaded{
            [&](const TypeEventCell& cell) {
              const mir::ExprId held = block.Add(BuildStaticStorageAccess(
                  unit, frame, cell.home,
                  mir::ObservableCellOf(unit.types, unit.builtins.event),
                  mir::EnclosingHops{}));
              block.AppendStmt(
                  mir::ExprStmt{
                      .expr = block.Add(BuildTriggerCallExpr(
                          unit_lowerer, block,
                          block.Add(block.WrapperContents(
                              unit.types, held, unit.builtins.event))))});
            },
            [&](const HeldCovergroupEvent& held) {
              // The holder's handle, read in `at`.
              const auto holder = [&](mir::Builder at) {
                return at.AsValue(
                    unit.types,
                    at.Add(PropertyStorage(
                        unit, at,
                        at.Add(MakeSelfRefExpr(frame.WithBlock(&at.Body()))),
                        held.holder, held.handle_type)));
              };
              const mir::ExprId holds = HoldsObject(unit, block, holder(block));
              mir::Block told;
              mir::Builder told_builder = block.Into(told);
              const mir::ExprId handle = holder(told_builder);
              const auto [first, record] =
                  RecordOf(unit_lowerer, held.covergroup);
              const mir::ClassFieldTarget record_property =
                  CovergroupPropertyTarget(unit_lowerer, first, record);
              const mir::TypeId record_type =
                  CovergroupPropertyType(unit_lowerer, first, record);
              const mir::ExprId record_handle = told_builder.AsValue(
                  unit.types, told_builder.Add(PropertyStorage(
                                  unit, told_builder, handle, record_property,
                                  record_type)));
              const CoverageLibrary library(unit_lowerer.Hir());
              const CoverageLibrary::Property event = library.PropertyOf(
                  library.Members().group_instance.block_event);
              const mir::ClassFieldTarget event_property =
                  LibraryField(unit_lowerer, event);
              const mir::TypeId event_type =
                  unit_lowerer.TranslateType(event.type);
              told_builder.AppendStmt(
                  mir::ExprStmt{
                      .expr = told_builder.Add(BuildTriggerCallExpr(
                          unit_lowerer, told_builder,
                          told_builder.Add(PropertyStorage(
                              unit, told_builder, record_handle, event_property,
                              event_type))))});
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
    const hir::Expr& written = lowerer.HirExprs().Get(actuals[i]);
    const mir::TypeId type = lowerer.Owner().TranslateType(formals[i].type);
    auto actual = EmitBoundActual(
        lowerer, frame, written, *ParamPassingOf(formals[i].direction), type);
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
  const mir::DirectTarget target =
      TypeMethod(unit_lowerer, covergroup, kTypeRecord);
  const mir::TypeId record = TypeRecordType(unit_lowerer, library);
  return BuildFunctionCall(
      unit_lowerer.Unit(), frame, mir::Direct{.target = target},
      std::move(arguments), record);
}

// The instance `handle` holds, as an instance of `covergroup`. A use names
// the covergroup the source wrote, which a variable a base class declares
// holds as the first of its lineage, since the derived covergroup's instance
// replaces the base's (LRM 19.4.1); the object is one of that covergroup, so
// it is seen as one.
auto InstanceAs(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId handle,
    hir::CovergroupId covergroup) -> mir::ExprId {
  const mir::TypeId type = CovergroupHandle(unit_lowerer, covergroup);
  if (block.Get(handle).type == type) return handle;
  return block.Add(
      mir::Expr{.data = mir::CastExpr{.operand = handle}, .type = type});
}

// The options of the item `item` names, or of the instance's record where it
// names none, as the library holds them (LRM 19.7).
auto OptionsOfItem(
    const hir::CoverageLibraryMembers& library,
    const std::optional<hir::CoverageItemRef>& item)
    -> const hir::ClassPropertyTarget& {
  if (!item.has_value()) return library.group_instance.option;
  return std::visit(
      Overloaded{
          [&](const hir::CoverpointRef&) -> const hir::ClassPropertyTarget& {
            return library.coverpoint.option;
          },
          [&](const hir::CrossRef&) -> const hir::ClassPropertyTarget& {
            return library.cross.option;
          }},
      *item);
}

// The library object a method or an option of an instance asks about: the
// item where one is named, and the instance's record otherwise.
auto KeeperOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    hir::CovergroupId covergroup, mir::ExprId instance,
    const std::optional<hir::CoverageItemRef>& item) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  const CoverageLibrary library(unit_lowerer.Hir());
  const auto read = [&](const CovergroupClass& owner, std::uint32_t property) {
    const mir::ExprId object = ReportedObject(unit, frame, instance);
    const mir::ClassFieldTarget target =
        CovergroupPropertyTarget(unit_lowerer, owner, property);
    const mir::TypeId type =
        CovergroupPropertyType(unit_lowerer, owner, property);
    return block.AsValue(
        unit.types,
        block.Add(PropertyStorage(unit, block, object, target, type)));
  };
  if (item.has_value()) {
    const HeldItem found = HeldItemOf(unit_lowerer, library, *item);
    return read(found.owner, found.property);
  }
  const auto [first, record] = RecordOf(unit_lowerer, covergroup);
  return read(first, record);
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
    const hir::Expr& written = lowerer.HirExprs().Get(actuals[i]);
    const mir::TypeId type = lowerer.Owner().TranslateType(formal.type);
    auto actual = EmitBoundActual(
        lowerer, frame, written, *ParamPassingOf(formal.direction), type);
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
  mir::Builder block = frame.Builder();
  auto lowered =
      lowerer.LowerExpr(lowerer.HirExprs().Get(through.instance), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return InstanceAs(
      unit_lowerer, block,
      block.AsValue(unit_lowerer.Unit().types, block.Add(*std::move(lowered))),
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
  mir::Builder block = frame.Builder();
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
        block.AsValue(unit.types, block.Add(*std::move(embedding)));
    auto actuals = BindCovergroupActuals(
        lowerer, frame, published.formals, created.arguments);
    if (!actuals) return std::unexpected(std::move(actuals.error()));
    const mir::VirtualSlot slot = FactorySlot(unit_lowerer, created.covergroup);
    return block.Get(BuildFunctionCall(
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
  // embedding it and the arguments (LRM 19.3).
  std::vector<mir::ExprId> arguments;
  if (created.declaring_instance.has_value()) {
    arguments.push_back(BuildImplicitInstanceArgument(
        lowerer, frame, *created.declaring_instance));
  }
  if (created.embedding.has_value()) {
    auto embedding =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*created.embedding), frame);
    if (!embedding) return std::unexpected(std::move(embedding.error()));
    const mir::ExprId handle =
        block.AsValue(unit.types, block.Add(*std::move(embedding)));
    const mir::TypeId embedding_handle =
        HandleTo(unit_lowerer, *published.embedding);
    arguments.push_back(ConvertToType(unit, block, handle, embedding_handle));
  }
  const hir::CovergroupId lineage_first =
      FirstOf(unit_lowerer, created.covergroup);
  const hir::PublishedCovergroup& first =
      PublishedOf(unit_lowerer, lineage_first);
  auto actuals =
      BindCovergroupActuals(lowerer, frame, first.formals, created.arguments);
  if (!actuals) return std::unexpected(std::move(actuals.error()));
  arguments.insert(arguments.end(), actuals->begin(), actuals->end());
  arguments.push_back(BuildReportHandedOn(unit, frame));
  return mir::MakeNewExpr(
      block.Add(
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
  mir::Builder block = frame.Builder();
  const CoverageLibrary library(unit_lowerer.Hir());
  const hir::CoverageLibraryMembers& members = library.Members();
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
      [&](mir::ExprId receiver, const hir::ClassMethodTarget& called,
          std::vector<mir::ExprId> leading) -> diag::Result<mir::Expr> {
    const CoverageLibrary::Method method = library.MethodOf(called);
    auto bound =
        BindLibraryActuals(lowerer, frame, method, given, leading.size());
    if (!bound) return std::unexpected(std::move(bound.error()));
    leading.insert(leading.end(), bound->begin(), bound->end());
    return block.Get(BuildMethodCall(
        unit_lowerer, frame, method.callee, receiver, leading,
        method.prototype.result_type));
  };
  // The library method of what keeps the instance's coverage, or its item's:
  // `of_instance` or `of_item`.
  const auto on_keeper =
      [&](const hir::ClassMethodTarget& of_instance,
          const hir::ClassMethodTarget& of_item) -> diag::Result<mir::Expr> {
    auto object = instance();
    if (!object) return std::unexpected(std::move(object.error()));
    return library_call(
        KeeperOf(unit_lowerer, frame, call.covergroup, *object, call.item),
        call.item.has_value() ? of_item : of_instance, {});
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
      return block.Get(BuildFunctionCall(
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
      if (!call.item.has_value()) {
        return library_call(
            type,
            counted ? members.group_type.get_coverage
                    : members.group_type.coverage,
            {});
      }
      return library_call(
          type,
          counted ? members.group_type.get_item_coverage
                  : members.group_type.item_coverage,
          {mir::BuildIntLiteral(
              unit, block, LineagePosition(unit_lowerer, *call.item))});
    }
    case hir::CovergroupMethod::kGetInstCoverage:
      return counted ? on_keeper(
                           members.group_instance.get_inst_coverage,
                           members.item.get_inst_coverage)
                     : on_keeper(
                           members.group_instance.inst_coverage,
                           members.item.inst_coverage);
    case hir::CovergroupMethod::kSetInstName:
      return on_keeper(
          members.group_instance.set_inst_name,
          members.group_instance.set_inst_name);
    case hir::CovergroupMethod::kStart:
      return on_keeper(members.group_instance.start, members.item.start);
    case hir::CovergroupMethod::kStop:
      return on_keeper(members.group_instance.stop, members.item.stop);
  }
  throw InternalError("unknown covergroup method");
}

template <ExprLowerer Lowerer>
auto CovergroupOptionPath(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::CovergroupOptionExpr& option) -> diag::Result<AccessPath> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  const CoverageLibrary library(unit_lowerer.Hir());
  // The member of the options structure `path` names, of `type`.
  const auto member = [&](AccessPath path, hir::TypeId type) -> AccessPath {
    return OptionMember(unit_lowerer, std::move(path), type, option.member);
  };
  // The library property `property` of the object `object` holds, and the
  // type it holds in the library.
  const auto property = [&](mir::ExprId object,
                            const hir::ClassPropertyTarget& property) {
    const CoverageLibrary::Property found = library.PropertyOf(property);
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
            if (!option.item.has_value()) {
              auto [path, held] =
                  property(type, library.Members().group_type.type_option);
              return member(std::move(path), held);
            }
            // An item's are the element of the type's table its place in the
            // lineage keys.
            auto [table, held] = property(
                type,
                std::holds_alternative<hir::CrossRef>(*option.item)
                    ? library.Members().group_type.cross_type_options
                    : library.Members().group_type.coverpoint_type_options);
            auto [options, options_type] = ItemTypeOptions(
                unit_lowerer, std::move(table), held,
                mir::BuildIntLiteral(
                    unit, block, LineagePosition(unit_lowerer, *option.item)));
            return member(std::move(options), options_type);
          },
          // An instance's (LRM 19.7).
          [&](const hir::CovergroupInstanceReceiver& through)
              -> diag::Result<AccessPath> {
            auto object = BuildReceivedInstance(
                lowerer, frame, through, option.covergroup);
            if (!object) return std::unexpected(std::move(object.error()));
            auto [path, held] = property(
                KeeperOf(
                    unit_lowerer, frame, option.covergroup, *object,
                    option.item),
                OptionsOfItem(library.Members(), option.item));
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
  const hir::CoverageRegistryMembers& registry = library.Members().registry;
  const hir::ClassMethodTarget& called =
      [&]() -> const hir::ClassMethodTarget& {
    switch (database) {
      case support::CoverageDatabaseCall::kGetCoverage:
        return registry.get_coverage;
      case support::CoverageDatabaseCall::kSetCoverageDbName:
        return registry.set_coverage_db_name;
      case support::CoverageDatabaseCall::kLoadCoverageDb:
        return registry.load_coverage_db;
    }
    throw InternalError("unknown coverage database function");
  }();
  const CoverageLibrary::Method method = library.MethodOf(called);
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
