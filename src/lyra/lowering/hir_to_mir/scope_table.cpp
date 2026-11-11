#include "lyra/lowering/hir_to_mir/scope_table.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/foreign_import_id.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The kind of declaration a procedural scope is, where LRM 37.12 makes it a
// scope; none for a scope only the language's own bookkeeping opens, nor for
// a block the front end made no scope, which nothing names.
auto LexicalScopeKindOf(const hir::ProceduralScopeDecl& scope)
    -> std::optional<support::LexicalScopeKind> {
  switch (scope.kind) {
    case hir::ProceduralScopeKind::kTask:
      return support::LexicalScopeKind::kTask;
    case hir::ProceduralScopeKind::kFunction:
      return support::LexicalScopeKind::kFunction;
    case hir::ProceduralScopeKind::kFor:
      return support::LexicalScopeKind::kFor;
    case hir::ProceduralScopeKind::kForeach:
      return support::LexicalScopeKind::kForeach;
    case hir::ProceduralScopeKind::kBegin:
      return support::LexicalScopeKind::kBegin;
    case hir::ProceduralScopeKind::kForkJoin:
      return support::LexicalScopeKind::kForkJoin;
    case hir::ProceduralScopeKind::kForkJoinAny:
      return support::LexicalScopeKind::kForkJoinAny;
    case hir::ProceduralScopeKind::kForkJoinNone:
      return support::LexicalScopeKind::kForkJoinNone;
    case hir::ProceduralScopeKind::kProcessRoot:
    case hir::ProceduralScopeKind::kConstraintBlock:
    case hir::ProceduralScopeKind::kCovergroup:
    case hir::ProceduralScopeKind::kImplicit:
      return std::nullopt;
  }
  throw InternalError("LexicalScopeKindOf: unknown procedural scope kind");
}

// What an instance member declares: one instance, or an array of them, of the
// kind of design element its instantiation instantiates.
auto InstanceDeclarationKind(const hir::InstanceMemberDecl& member)
    -> support::ElaboratedScopeKind {
  const bool array = !member.array_dims.empty();
  switch (member.instantiated) {
    case hir::DesignElementKind::kModule:
      return array ? support::ElaboratedScopeKind::kModuleArray
                   : support::ElaboratedScopeKind::kModule;
    case hir::DesignElementKind::kInterface:
      return array ? support::ElaboratedScopeKind::kInterfaceArray
                   : support::ElaboratedScopeKind::kInterface;
    case hir::DesignElementKind::kProgram:
      return array ? support::ElaboratedScopeKind::kProgramArray
                   : support::ElaboratedScopeKind::kProgram;
  }
  throw InternalError("InstanceDeclarationKind: a design element of no kind");
}

// Builds the table of the scopes one scope declares, each record a constant
// of the unit, in the order they are added.
class TableBuilder {
 public:
  TableBuilder(
      const mir::CompilationUnit& unit, const hir::CompilationUnit& hir)
      : unit_(&unit), hir_(&hir) {
  }

  void AddInstance(
      hir::InstanceMemberId id, const hir::InstanceMemberDecl& member) {
    table_.instances.emplace(
        id, AddElaborated(
                InstanceDeclarationKind(member), member.instance_name, false));
  }

  // A generate construct's blocks, under the name the source wrote or the one
  // LRM 27.6 gives each: the one block a conditional or a standalone block may
  // build, the blocks a loop counts out, or each a case may build.
  void AddGenerate(hir::GenerateId id, const hir::Generate& generate) {
    std::vector<mir::ConstantId> records;
    std::visit(
        Overloaded{
            [&](const hir::SingleBlock& single) {
              records.push_back(AddBlock(
                  support::ElaboratedScopeKind::kGenerateBlock,
                  single.declared));
            },
            [&](const hir::BlocksRepeat& repeat) {
              records.push_back(AddBlock(
                  support::ElaboratedScopeKind::kGenerateLoop,
                  repeat.declared));
            },
            [&](const hir::BlocksChoose& chosen) {
              for (const hir::GenerateBlockName& alternative :
                   chosen.alternatives) {
                records.push_back(AddBlock(
                    support::ElaboratedScopeKind::kGenerateBlock, alternative));
              }
            }},
        generate.counting);
    table_.generates.emplace(id, std::move(records));
  }

  // Each procedural scope of `scopes` LRM 37.12 makes a scope, under the
  // nearest one around it that is one, from `id` down.
  void AddProceduralScope(
      const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
          scopes,
      hir::ProceduralScopeId id, std::optional<mir::ConstantId> enclosing) {
    const hir::ProceduralScopeDecl& decl = scopes.Get(id);
    if (const std::optional<support::LexicalScopeKind> kind =
            LexicalScopeKindOf(decl)) {
      enclosing = AddLexical(*kind, decl.source_name, enclosing);
      table_.procedural_scopes.emplace(id, *enclosing);
    }
    for (const hir::ProceduralScopeId child : decl.child_scopes) {
      AddProceduralScope(scopes, child, enclosing);
    }
  }

  // A subroutine the scope imports from C, a task or a function of it as much
  // as one with a body (LRM 37.41 detail 6).
  void AddImport(hir::ForeignImportId id) {
    const hir::ForeignImportDecl& import = hir_->foreign_imports.Get(id);
    AddLexical(
        import.is_task ? support::LexicalScopeKind::kTask
                       : support::LexicalScopeKind::kFunction,
        import.name, std::nullopt);
  }

  void AddLexicalAtTop(
      support::LexicalScopeKind kind, const std::string& name) {
    AddLexical(kind, name, std::nullopt);
  }

  auto Take() && -> ScopeTable {
    return std::move(table_);
  }

 private:
  auto AddBlock(
      support::ElaboratedScopeKind kind, const hir::GenerateBlockName& declared)
      -> mir::ConstantId {
    return AddElaborated(kind, declared.name, declared.implicitly_named);
  }

  auto AddElaborated(
      support::ElaboratedScopeKind kind, const std::string& name,
      bool implicitly_named) -> mir::ConstantId {
    const mir::ConstantId record =
        ElaboratedScopeRecordConstant(*unit_, kind, name, implicitly_named);
    table_.elaborated.push_back(record);
    return record;
  }

  auto AddLexical(
      support::LexicalScopeKind kind, const std::optional<std::string>& name,
      std::optional<mir::ConstantId> enclosing) -> mir::ConstantId {
    const mir::ConstantId record = LexicalScopeRecordConstant(
        *unit_, kind, name, enclosing,
        static_cast<std::uint32_t>(table_.lexical.size()));
    table_.lexical.push_back(record);
    return record;
  }

  const mir::CompilationUnit* unit_;
  const hir::CompilationUnit* hir_;
  ScopeTable table_;
};

}  // namespace

auto StateScopeTable(
    const mir::CompilationUnit& unit, const hir::CompilationUnit& hir,
    const hir::StructuralScope& scope) -> ScopeTable {
  TableBuilder builder(unit, hir);
  for (const hir::InstanceMemberId id : scope.instance_members.Ids()) {
    builder.AddInstance(id, scope.instance_members.Get(id));
  }
  for (const hir::GenerateId id : scope.generates.Ids()) {
    builder.AddGenerate(id, scope.generates.Get(id));
  }
  for (const hir::ProceduralScopeId id :
       hir::RootProceduralScopes(scope.procedural_scopes)) {
    builder.AddProceduralScope(scope.procedural_scopes, id, std::nullopt);
  }
  for (const hir::ForeignImportId id : scope.declared_imports) {
    builder.AddImport(id);
  }
  for (const hir::ClockingBlockDecl& clocking : scope.clocking_blocks) {
    builder.AddLexicalAtTop(
        support::LexicalScopeKind::kClockingBlock, clocking.name);
  }
  for (const std::string& name : scope.declared_classes) {
    builder.AddLexicalAtTop(support::LexicalScopeKind::kClass, name);
  }
  return std::move(builder).Take();
}

}  // namespace lyra::lowering::hir_to_mir
