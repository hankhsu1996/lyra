#include "lyra/lowering/hir_to_mir/object_record.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The object as nothing in particular, which is what every entry the record
// holds takes and answers with. Nothing about the object's class travels in the
// type, because the entry is reached through the record of the class it belongs
// to and that is what says which class it is.
auto OpaquePointer(mir::CompilationUnit& unit) -> mir::TypeId {
  return mir::ErasedPointer(unit.types);
}

auto BorrowedPointerTo(mir::CompilationUnit& unit, mir::TypeId pointee)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = pointee,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
}

// Gives `code` the object as its first parameter and reads it back as a value
// of the class the entry belongs to. Every entry a class supplies opens this
// way: what a call reaching one through the record can say about the object is
// nothing at all, so reading it back is this side's own step, and the one the
// target's rules decide.
auto TakeSelf(
    mir::CompilationUnit& unit, mir::CallableCode& code, mir::TypeId self_type)
    -> mir::ExprId {
  const mir::TypeId opaque = OpaquePointer(unit);
  const mir::LocalId self = code.AddLocal(opaque);
  code.params.push_back(self);
  const mir::ExprId read =
      code.Body().exprs.Add(mir::MakeLocalRefExpr(self, opaque));
  return code.Body().exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = read}, .type = self_type});
}

// A body taking the object and answering with a pointer, with the object
// already read back as the class this record belongs to. Both entries a target
// supplies about reaching storage have this shape.
struct Entry {
  mir::CallableCode code;
  mir::ExprId typed_self;
};

auto BeginEntry(mir::CompilationUnit& unit, mir::TypeId self_type) -> Entry {
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::ExprId typed = TakeSelf(unit, code, self_type);
  code.result_type = OpaquePointer(unit);
  return Entry{.code = std::move(code), .typed_self = typed};
}

void EndEntry(mir::CompilationUnit& unit, Entry& entry, mir::ExprId pointer) {
  const mir::ExprId erased = entry.code.Body().exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = pointer},
          .type = OpaquePointer(unit)});
  entry.code.Body().AppendStmt(mir::ReturnStmt{.value = erased});
}

auto AddEntry(mir::Class& cls, Entry entry) -> mir::AbiAdapterId {
  return cls.abi_adapters.Add(
      mir::AbiAdapter{
          .code = std::move(entry.code), .published = mir::UnpublishedEntry{}});
}

// A body of this class, reached through an entry of one shape whatever the
// body's own signature is (LRM 8.20). The call site restores that signature,
// which is the same erasure a scope's exports use and for the same reason: one
// table holds entries of every prototype.
auto BehaviorEntry(
    mir::CompilationUnit& unit, mir::Class& cls, mir::ClassId id,
    mir::CallableId method) -> mir::AbiAdapterId {
  const mir::CallableCode& target = cls.callables.Get(method).code;
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::ExprId typed = TakeSelf(unit, code, cls.self_pointer_type);
  const std::span<const mir::LocalId> formals =
      std::span{target.params}.subspan(
          target.HasReceiver(cls.self_pointer_type) ? 1 : 0);
  std::vector<mir::ExprId> arguments;
  arguments.reserve(formals.size());
  for (const mir::LocalId formal : formals) {
    const mir::LocalDecl& decl = target.locals.Get(formal);
    const mir::LocalId param = code.AddLocal(decl.type);
    code.params.push_back(param);
    arguments.push_back(
        code.Body().exprs.Add(mir::MakeLocalRefExpr(param, decl.type)));
  }
  code.result_type = target.result_type;

  const mir::ExprId call = code.Body().exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{.owner = id, .slot = method},
                          .receiver = typed},
                  .arguments = std::move(arguments)},
          .type = target.result_type});
  // A task suspends its caller until it completes (LRM 13.3), so the entry
  // suspends too: it awaits the body and hands back the completion, which is
  // what whoever entered it awaits in turn. Anything else completes where it is
  // called and its result is the entry's.
  const mir::Type& result = unit.types.Get(target.result_type);
  if (const auto* coroutine = result.As<mir::CoroutineType>()) {
    const mir::LocalId completion = code.AddLocal(coroutine->payload);
    code.Body().AppendStmt(
        mir::LocalDeclStmt{
            .target = completion,
            .init = code.Body().exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call},
                    .type = coroutine->payload})});
    code.Body().AppendStmt(
        mir::ReturnStmt{
            .value = code.Body().exprs.Add(
                mir::MakeLocalRefExpr(completion, coroutine->payload))});
  } else if (result.Is<mir::VoidType>()) {
    code.Body().AppendStmt(mir::ExprStmt{.expr = call});
    code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    code.Body().AppendStmt(mir::ReturnStmt{.value = call});
  }
  return cls.abi_adapters.Add(
      mir::AbiAdapter{
          .code = std::move(code), .published = mir::UnpublishedEntry{}});
}

// Which of a class's own introductions `slot` is, counted the way every reader
// of a dispatch position counts it: over what that class introduces, in the
// order it introduces them.
auto OrdinalOf(const ClassShape& shape, mir::CallableId slot) -> std::uint32_t {
  std::uint32_t ordinal = 0;
  for (std::uint32_t at = 0; at < shape.callable_signatures.size(); ++at) {
    const mir::CallableId here{at};
    if (here == slot) {
      return ordinal;
    }
    if (mir::IntroducesSlot(
            shape.callable_signatures.Get(here).virtual_dispatch)) {
      ++ordinal;
    }
  }
  throw InternalError(
      "InstallObjectRecord: a class takes over a behavior the class it names "
      "introduces nowhere");
}

// The pointer an object of `ref` is read back as, for the one step a class
// takes toward its own base. A class of the runtime library is never the base
// of a class of the source language, so no object of one is reached this way.
auto ClassPointerType(mir::CompilationUnit& unit, const mir::ClassRef& ref)
    -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& i) -> mir::TypeId {
            return unit.GetClass(i.class_id).self_pointer_type;
          },
          [&](const mir::CrossUnitClassRef& e) -> mir::TypeId {
            return BorrowedPointerTo(
                unit, unit.types.Intern(
                          mir::Type{mir::CrossUnitClassType{
                              .unit_name = e.unit_name,
                              .class_name = e.class_name}}));
          },
          [](const mir::RuntimeClassRef&) -> mir::TypeId {
            throw InternalError(
                "InstallObjectRecord: a class of the source language extends a "
                "class of the runtime library");
          }},
      ref);
}

}  // namespace

void InstallObjectRecord(
    UnitLowerer& lowerer, mir::ClassId id, mir::Class& cls) {
  mir::CompilationUnit& unit = lowerer.Unit();
  const mir::TypeId opaque = OpaquePointer(unit);

  // Where each of this class's own storage slots sits on an object of it. Only
  // the side that laid the object out can answer, so this is the whole of what
  // the record asks a target for about reaching a property. There is one entry
  // per slot rather than per name, because a slot is what a coordinate carries
  // and what the name table answers with -- storage the source never named
  // simply has an entry nothing asks for.
  std::vector<mir::AbiAdapterId> slots;
  slots.reserve(cls.fields.size());
  for (const mir::FieldId slot : cls.fields.Ids()) {
    Entry entry = BeginEntry(unit, cls.self_pointer_type);
    const mir::TypeId field_type = cls.fields.Get(slot).type;
    const mir::ExprId field = entry.code.Body().exprs.Add(
        mir::MakeFieldAccessExpr(
            entry.typed_self, mir::ClassFieldTarget{.owner = id, .slot = slot},
            field_type));
    const mir::ExprId address = entry.code.Body().exprs.Add(
        mir::MakeAddressOfExpr(field, BorrowedPointerTo(unit, field_type)));
    EndEntry(unit, entry, address);
    slots.push_back(AddEntry(cls, std::move(entry)));
  }

  // The same object seen as the class this one extends. The step is the target
  // language's own conversion, which is why it is an entry rather than an
  // offset: where a base sits inside an object is that language's answer.
  std::optional<mir::AbiAdapterId> to_base;
  if (cls.base.has_value()) {
    Entry entry = BeginEntry(unit, cls.self_pointer_type);
    const mir::ExprId as_base = entry.code.Body().exprs.Add(
        mir::Expr{
            .data = mir::CastExpr{.operand = entry.typed_self},
            .type = ClassPointerType(unit, *cls.base)});
    EndEntry(unit, entry, as_base);
    to_base = AddEntry(cls, std::move(entry));
  }

  // What this class adds to the dispatch its lineage carries (LRM 8.20): the
  // behaviors it introduces, in the order it introduces them, and the ones it
  // takes over from whatever introduced them. A class states its own
  // contribution and nothing about the lineage, so which body answers a
  // position is found by walking from the class an object is.
  struct Takeover {
    mir::ClassRef introduced_by;
    std::uint32_t ordinal;
    mir::AbiAdapterId body;
  };
  std::vector<std::optional<mir::AbiAdapterId>> introductions;
  std::vector<Takeover> takeovers;
  std::vector<std::pair<std::string, std::uint32_t>> behavior_names;
  std::vector<std::pair<std::string, mir::AbiAdapterId>> body_names;
  for (const mir::CallableId method : cls.callables.Ids()) {
    if (!cls.callables.IsDefined(method)) {
      continue;
    }
    const std::optional<mir::VirtualDispatchRole>& role =
        cls.callables.Get(method).virtual_dispatch;
    // A callable answering no dispatch position leaves the object nothing to
    // decide (LRM 8.14), so the name reaches the body itself rather than a
    // position to look one up at. It has no place among the introductions for
    // the same reason: there is no position to count it as.
    if (!role.has_value()) {
      const std::optional<std::string_view> name =
          mir::NameOf(cls.named_callables, method);
      if (name.has_value() && cls.callables.Get(method).code.body.has_value()) {
        body_names.emplace_back(
            std::string{*name}, BehaviorEntry(unit, cls, id, method));
      }
      continue;
    }
    // A behavior declared with no body (LRM 8.21, and every behavior an
    // interface class states, LRM 8.26) is a position nothing answers. It still
    // takes its place among what the class introduces, because that is what
    // every reader counts over, and it holds nothing -- which is sound because
    // no object of such a class is ever built.
    const std::optional<mir::AbiAdapterId> entry =
        cls.callables.Get(method).code.body.has_value()
            ? std::optional{BehaviorEntry(unit, cls, id, method)}
            : std::nullopt;
    std::visit(
        Overloaded{
            [&](const mir::IntroducesVirtualSlot&) {
              const auto ordinal =
                  static_cast<std::uint32_t>(introductions.size());
              introductions.push_back(entry);
              const std::optional<std::string_view> name =
                  mir::NameOf(cls.named_callables, method);
              if (name.has_value()) {
                behavior_names.emplace_back(std::string{*name}, ordinal);
              }
            },
            [&](const mir::OverridesIntraUnitSlot& o) {
              if (!entry.has_value()) {
                return;
              }
              takeovers.push_back(
                  Takeover{
                      .introduced_by =
                          mir::IntraUnitClassRef{.class_id = o.slot_owner},
                      .ordinal = OrdinalOf(
                          lowerer.GetClassShape(o.slot_owner), o.slot_id),
                      .body = *entry});
            },
            [&](const mir::OverridesExternalSlot& o) {
              if (!entry.has_value()) {
                return;
              }
              takeovers.push_back(
                  Takeover{
                      .introduced_by =
                          mir::CrossUnitClassRef{
                              .unit_name = o.unit_name,
                              .class_name = o.class_name},
                      .ordinal = o.ordinal.value,
                      .body = *entry});
            }},
        *role);
  }

  // The names this class answers while a reference to it resolves, each paired
  // with where it lands. Only what this class declares is here: a name it does
  // not declare is found by asking what it extends.
  mir::StaticConstantDecl names_decl;
  const auto name_count = static_cast<std::uint32_t>(cls.named_fields.size());
  {
    mir::RuntimeRecordBuilder records(unit, names_decl.body.exprs);
    const mir::TypeId record_type =
        records.Type(mir::RuntimeLibraryKind::kObjectDefinition);
    std::vector<mir::ExprId> entries;
    entries.reserve(cls.named_fields.size());
    for (const mir::NamedField& named : cls.named_fields) {
      const mir::ExprId own = records.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target =
                          mir::ObjectRecordRef{
                              .of = mir::IntraUnitClassRef{.class_id = id}}},
              .type = record_type});
      const mir::ExprId at = records.Construct(
          mir::RuntimeLibraryKind::kPropertyCoordinate,
          {records.Add(
               mir::MakeAddressOfExpr(
                   own, BorrowedPointerTo(unit, record_type))),
           records.MachineInt(static_cast<std::int64_t>(named.slot.value))});
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kResolvedProperty,
          {records.StringRef(named.name), at}));
    }
    names_decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kResolvedProperty),
        std::move(entries));
    names_decl.type = records.TypeOf(names_decl.value);
  }
  const mir::TypeId names_type = names_decl.type;
  const mir::StaticConstantId names_id =
      cls.static_constants.Add(std::move(names_decl));

  // The entries themselves, gathered in the order the class gave positions.
  mir::StaticConstantDecl slots_decl;
  const auto slot_count = static_cast<std::uint32_t>(slots.size());
  {
    mir::RuntimeRecordBuilder records(unit, slots_decl.body.exprs);
    std::vector<mir::ExprId> entries;
    entries.reserve(slots.size());
    for (const mir::AbiAdapterId slot : slots) {
      entries.push_back(records.FunctionRef(cls, slot));
    }
    const mir::TypeId entry_type =
        entries.empty() ? unit.types.Intern(
                              mir::Type{mir::MachineFunctionType{
                                  .params = {opaque}, .result = opaque}})
                        : records.TypeOf(entries.front());
    slots_decl.value = records.MachineArray(entry_type, std::move(entries));
    slots_decl.type = records.TypeOf(slots_decl.value);
  }
  const mir::TypeId slots_type = slots_decl.type;
  const mir::StaticConstantId slots_id =
      cls.static_constants.Add(std::move(slots_decl));

  // The bodies this class introduces, in the order it introduces them. A
  // position nothing supplies a body for belongs to a class LRM 8.21 forbids
  // constructing, so it holds nothing and no object ever reaches it.
  mir::StaticConstantDecl introductions_decl;
  const auto introduction_count =
      static_cast<std::uint32_t>(introductions.size());
  {
    mir::RuntimeRecordBuilder records(unit, introductions_decl.body.exprs);
    const mir::TypeId entry_type = mir::ErasedFunction(unit.types);
    std::vector<mir::ExprId> entries;
    entries.reserve(introductions.size());
    for (const std::optional<mir::AbiAdapterId>& body : introductions) {
      entries.push_back(
          body.has_value()
              ? records.ErasedFunctionRef(cls, *body)
              : records.Add(
                    mir::Expr{.data = mir::NullLiteral{}, .type = entry_type}));
    }
    introductions_decl.value =
        records.MachineArray(entry_type, std::move(entries));
    introductions_decl.type = records.TypeOf(introductions_decl.value);
  }
  const mir::TypeId introductions_type = introductions_decl.type;
  const mir::StaticConstantId introductions_id =
      cls.static_constants.Add(std::move(introductions_decl));

  // The positions this class takes over, each naming what introduced it.
  mir::StaticConstantDecl takeovers_decl;
  const auto takeover_count = static_cast<std::uint32_t>(takeovers.size());
  {
    mir::RuntimeRecordBuilder records(unit, takeovers_decl.body.exprs);
    const mir::TypeId record_type =
        records.Type(mir::RuntimeLibraryKind::kObjectDefinition);
    std::vector<mir::ExprId> entries;
    entries.reserve(takeovers.size());
    for (const Takeover& taken : takeovers) {
      const mir::ExprId introducer = records.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target =
                          mir::ObjectRecordRef{.of = taken.introduced_by}},
              .type = record_type});
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kDispatchTakeover,
          {records.Add(
               mir::MakeAddressOfExpr(
                   introducer, BorrowedPointerTo(unit, record_type))),
           records.MachineInt(static_cast<std::int64_t>(taken.ordinal)),
           records.ErasedFunctionRef(cls, taken.body)}));
    }
    takeovers_decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kDispatchTakeover),
        std::move(entries));
    takeovers_decl.type = records.TypeOf(takeovers_decl.value);
  }
  const mir::TypeId takeovers_type = takeovers_decl.type;
  const mir::StaticConstantId takeovers_id =
      cls.static_constants.Add(std::move(takeovers_decl));

  // The names its own introductions answer to. A behavior taken over answers
  // under the name its introducer already gave it, so only an introduction
  // brings one.
  mir::StaticConstantDecl behavior_names_decl;
  const auto behavior_name_count =
      static_cast<std::uint32_t>(behavior_names.size());
  {
    mir::RuntimeRecordBuilder records(unit, behavior_names_decl.body.exprs);
    const mir::TypeId record_type =
        records.Type(mir::RuntimeLibraryKind::kObjectDefinition);
    std::vector<mir::ExprId> entries;
    entries.reserve(behavior_names.size());
    for (const auto& [name, ordinal] : behavior_names) {
      const mir::ExprId own = records.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target =
                          mir::ObjectRecordRef{
                              .of = mir::IntraUnitClassRef{.class_id = id}}},
              .type = record_type});
      const mir::ExprId at = records.Construct(
          mir::RuntimeLibraryKind::kBehaviorCoordinate,
          {records.Add(
               mir::MakeAddressOfExpr(
                   own, BorrowedPointerTo(unit, record_type))),
           records.MachineInt(static_cast<std::int64_t>(ordinal))});
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kResolvedBehavior,
          {records.StringRef(name), at}));
    }
    behavior_names_decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kResolvedBehavior),
        std::move(entries));
    behavior_names_decl.type = records.TypeOf(behavior_names_decl.value);
  }
  const mir::TypeId behavior_names_type = behavior_names_decl.type;
  const mir::StaticConstantId behavior_names_id =
      cls.static_constants.Add(std::move(behavior_names_decl));

  // The names it answers with a body outright. A name its lineage declares is
  // not here either: a call the object gets no say in still names a class, and
  // that class's own declaration is what the name means (LRM 8.14).
  mir::StaticConstantDecl body_names_decl;
  const auto body_name_count = static_cast<std::uint32_t>(body_names.size());
  {
    mir::RuntimeRecordBuilder records(unit, body_names_decl.body.exprs);
    std::vector<mir::ExprId> entries;
    entries.reserve(body_names.size());
    for (const auto& [name, body] : body_names) {
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kDeclaredBody,
          {records.StringRef(name), records.ErasedFunctionRef(cls, body)}));
    }
    body_names_decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kDeclaredBody),
        std::move(entries));
    body_names_decl.type = records.TypeOf(body_names_decl.value);
  }
  const mir::TypeId body_names_type = body_names_decl.type;
  const mir::StaticConstantId body_names_id =
      cls.static_constants.Add(std::move(body_names_decl));

  // The record itself.
  mir::StaticConstantDecl record_decl;
  {
    mir::RuntimeRecordBuilder record(unit, record_decl.body.exprs);
    const mir::TypeId record_type =
        record.Type(mir::RuntimeLibraryKind::kObjectDefinition);
    const mir::TypeId record_ptr = BorrowedPointerTo(unit, record_type);

    mir::ExprId base =
        record.Add(mir::Expr{.data = mir::NullLiteral{}, .type = record_ptr});
    if (cls.base.has_value()) {
      const mir::ExprId of = record.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target = mir::ObjectRecordRef{.of = *cls.base}},
              .type = record_type});
      base = record.Add(mir::MakeAddressOfExpr(of, record_ptr));
    }

    const auto run = [&](mir::StaticConstantId constant, mir::TypeId type,
                         mir::TypeId element,
                         mir::RuntimeLibraryKind table_kind,
                         std::uint32_t count) -> mir::ExprId {
      const mir::ExprId held = record.Add(
          mir::Expr{
              .data =
                  mir::ReferenceExpr{
                      .target = mir::StaticConstantRef{.constant = constant}},
              .type = type});
      const mir::ExprId data = record.Add(
          mir::Expr{
              .data = mir::MachineArrayDataExpr{.array = held},
              .type = unit.types.Intern(
                  mir::Type{mir::PointerType{
                      .pointee = element,
                      .ownership = mir::PointerOwnership::kBorrowed,
                      .mutability = mir::Mutability::kReadOnly}})});
      return record.Construct(table_kind, {data, record.MachineInt(count)});
    };

    const mir::ExprId property_slots =
        run(slots_id, slots_type,
            unit.types.Intern(
                mir::Type{mir::MachineFunctionType{
                    .params = {opaque}, .result = opaque}}),
            mir::RuntimeLibraryKind::kPropertySlotTable, slot_count);
    const mir::ExprId property_names =
        run(names_id, names_type,
            record.Type(mir::RuntimeLibraryKind::kResolvedProperty),
            mir::RuntimeLibraryKind::kResolvedPropertyTable, name_count);
    const mir::ExprId introduction_table = run(
        introductions_id, introductions_type, mir::ErasedFunction(unit.types),
        mir::RuntimeLibraryKind::kMethodDispatchTable, introduction_count);
    const mir::ExprId takeover_table =
        run(takeovers_id, takeovers_type,
            record.Type(mir::RuntimeLibraryKind::kDispatchTakeover),
            mir::RuntimeLibraryKind::kTakeoverTable, takeover_count);
    const mir::ExprId behavior_name_table = run(
        behavior_names_id, behavior_names_type,
        record.Type(mir::RuntimeLibraryKind::kResolvedBehavior),
        mir::RuntimeLibraryKind::kResolvedBehaviorTable, behavior_name_count);
    const mir::ExprId body_name_table =
        run(body_names_id, body_names_type,
            record.Type(mir::RuntimeLibraryKind::kDeclaredBody),
            mir::RuntimeLibraryKind::kDeclaredBodyTable, body_name_count);
    const mir::ExprId view =
        to_base.has_value()
            ? record.FunctionRef(cls, *to_base)
            : record.Add(mir::Expr{.data = mir::NullLiteral{}, .type = opaque});

    record_decl.value = record.Construct(
        mir::RuntimeLibraryKind::kObjectDefinition,
        {base, property_slots, introduction_table, takeover_table,
         property_names, behavior_name_table, body_name_table, view});
    record_decl.type = record.TypeOf(record_decl.value);
  }
  cls.object_record = std::move(record_decl);
}

}  // namespace lyra::lowering::hir_to_mir
