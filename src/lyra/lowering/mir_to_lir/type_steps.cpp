#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

constexpr std::array kEveryStep{
    lir::ValueStep::kCopy, lir::ValueStep::kMove, lir::ValueStep::kAssign,
    lir::ValueStep::kEnd};

// One function carrying out a step, built block by block: its parameters are
// where the values it takes lie, and what it builds, where it builds one, is
// its answer, made in its return place.
class StepBody {
 public:
  StepBody(
      std::string name, std::span<const lir::TypeId> params,
      lir::TypeId result_type, bool answers) {
    fn_.name = std::move(name);
    fn_.result_type = result_type;
    fn_.linkage = lir::Linkage::kInternal;
    for (const lir::TypeId param : params) {
      fn_.params.push_back(fn_.values.Add(
          lir::Local{
              .name = {}, .type = param, .kind = lir::LocalKind::kParam}));
    }
    if (answers) {
      fn_.return_place = fn_.values.Add(
          lir::Local{
              .name = {}, .type = result_type, .kind = lir::LocalKind::kPlace});
    }
    current_ = NewBlock();
  }

  [[nodiscard]] auto Param(std::size_t i) const -> lir::ValueId {
    return fn_.params.at(i);
  }

  // The whole value the parameter at `i` points at.
  [[nodiscard]] auto Pointed(std::size_t i) const -> lir::Place {
    return lir::Place{
        .base = lir::Use{.value = fn_.params.at(i)},
        .chain = {lir::DerefProjection{}}};
  }

  // The return place, where the answer is made.
  [[nodiscard]] auto Answer() const -> lir::Place {
    return lir::Place{
        .base = lir::Use{.value = *fn_.return_place}, .chain = {}};
  }

  // The part at `index` of the value lying at `whole`.
  [[nodiscard]] static auto Component(lir::Place whole, std::size_t index)
      -> lir::Place {
    whole.chain.emplace_back(
        lir::ComponentProjection{
            .index = base::ComponentIndex{static_cast<std::uint32_t>(index)}});
    return whole;
  }

  // The element at the position `position` of the array lying at `whole`.
  [[nodiscard]] static auto Element(
      lir::Place whole, const lir::Operand& position) -> lir::Place {
    whole.chain.emplace_back(lir::ElementProjection{.coordinates = {position}});
    return whole;
  }

  auto Emit(lir::TypeId type, lir::Rvalue value) -> lir::Operand {
    const lir::ValueId made = fn_.values.Add(
        lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
    Statement(
        lir::AssignInstr{
            .destination = lir::Transient{.value = made},
            .value = std::move(value)});
    return lir::Use{.value = made};
  }

  auto Load(lir::Place place, lir::TypeId type) -> lir::Operand {
    return Emit(type, lir::LoadInstr{.place = std::move(place)});
  }

  void Statement(lir::InstrData data) {
    blocks_[current_.value].instrs.push_back(
        lir::Instr{.data = std::move(data)});
  }

  void Into(lir::Place place, lir::Rvalue value) {
    Statement(
        lir::AssignInstr{
            .destination = std::move(place), .value = std::move(value)});
  }

  auto Slot(lir::TypeId type) -> lir::ValueId {
    const lir::ValueId slot = fn_.values.Add(
        lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kPlace});
    Statement(
        lir::StorageInstr{.op = lir::StorageInstr::Op::kLive, .of = slot});
    return slot;
  }

  auto NewBlock() -> lir::BlockId {
    const lir::BlockId id{static_cast<std::uint32_t>(blocks_.size())};
    blocks_.emplace_back();
    return id;
  }

  void SetCurrent(lir::BlockId block) {
    current_ = block;
  }

  void Terminate(lir::TerminatorData data) {
    blocks_[current_.value].terminator =
        lir::Terminator{.data = std::move(data)};
  }

  auto Finish() && -> lir::Function {
    Terminate(lir::ReturnTerm{});
    fn_.blocks.reserve(blocks_.size());
    for (OpenBlock& block : blocks_) {
      if (!block.terminator.has_value()) {
        throw InternalError(
            "mir_to_lir: a step's body leaves a block with no way out -- "
            "please report this as a bug");
      }
      fn_.blocks.push_back(
          lir::BasicBlock{
              .instrs = std::move(block.instrs),
              .terminator = *std::move(block.terminator)});
    }
    return std::move(fn_);
  }

 private:
  struct OpenBlock {
    std::vector<lir::Instr> instrs;
    std::optional<lir::Terminator> terminator;
  };

  lir::Function fn_;
  std::vector<OpenBlock> blocks_;
  lir::BlockId current_{};
};

// A part of `type` built at `to` out of the one lying at `from`: copied, or
// moved, the one at `from` being ended afterwards by whoever holds it. A part
// held as itself rather than in storage of its own is put there as it is.
void BuildPart(
    StepBody& body, lir::TypeId type, const lir::Type& stated,
    lir::CallTarget step, lir::Place from, lir::Place to) {
  const lir::Operand read = body.Load(std::move(from), type);
  if (!stated.IsHeldInStorage()) {
    body.Into(std::move(to), lir::UseInstr{.operand = read});
    return;
  }
  body.Into(
      std::move(to), lir::CallInstr{.target = std::move(step), .args = {read}});
}

}  // namespace

void UnitLowerer::StateTypeSteps() {
  // Every type the unit names that a step is carried out by a function for --
  // a value laid out by its type, which the library holding one asks every
  // step of, and a closure -- is given that function. Each is named before any
  // body is built, since a body steps the parts of its value as their own types
  // say; building them names no type that needs one of its own: borrowed
  // pointers, machine integers, void.
  std::vector<std::pair<lir::TypeId, lir::ValueStep>> stated;
  for (const lir::TypeId type : out_.types.Ids()) {
    for (const lir::ValueStep step : kEveryStep) {
      if (!NeedsStepFunction(type, step)) {
        continue;
      }
      out_.type_steps.emplace(std::pair{type, step}, out_.functions.Declare());
      stated.emplace_back(type, step);
    }
  }
  for (const auto& [type, step] : stated) {
    out_.functions.Define(
        out_.type_steps.at(std::pair{type, step}),
        BuildStepFunction(type, step));
  }
}

auto UnitLowerer::NeedsStepFunction(lir::TypeId type, lir::ValueStep step) const
    -> bool {
  const lir::StepRealization realized = lir::StepOf(out_, type, step);
  if (std::holds_alternative<lir::StepByFunction>(realized)) {
    return true;
  }
  // A value laid out by its type is held by the library through a table that
  // names a function for every step of it it has, its bytes or not.
  return lir::IsLaidOutByItsType(out_.types.Get(type)) &&
         !std::holds_alternative<lir::NoSuchStep>(realized);
}

auto UnitLowerer::BuildStepFunction(lir::TypeId type, lir::ValueStep step)
    -> lir::Function {
  const auto pointer = [&](lir::Mutability mutability) {
    return out_.types.Intern(
        lir::Type{lir::PointerType{
            .pointee = type,
            .ownership = lir::PointerOwnership::kBorrowed,
            .mutability = mutability}});
  };
  const lir::TypeId read = pointer(lir::Mutability::kReadOnly);
  const lir::TypeId written = pointer(lir::Mutability::kMutable);
  const lir::TypeId void_type = out_.types.Intern(lir::Type{lir::VoidType{}});
  // A copy and a move take where the value lies and answer the value they
  // build, as rustc's clone shim answers `Self`; a write over a value takes
  // where it is written and where the value written lies; an end takes where
  // the value lies.
  std::vector<lir::TypeId> params;
  bool answers = false;
  switch (step) {
    case lir::ValueStep::kCopy:
      params = {read};
      answers = true;
      break;
    case lir::ValueStep::kMove:
      params = {written};
      answers = true;
      break;
    case lir::ValueStep::kAssign:
      params = {written, read};
      break;
    case lir::ValueStep::kEnd:
      params = {written};
      break;
  }
  StepBody body(
      lir::ValueStepSymbol(out_.body_name, lir::SymbolPartOf(out_, type), step),
      params, answers ? type : void_type, answers);
  // A copy, since building the body interns types, which may move what the
  // pool holds.
  const lir::Type stated = out_.types.Get(type);
  // Where the value the step takes lies, and where what it builds or writes
  // over lies: its answer, or the value written over.
  const lir::Place taken =
      step == lir::ValueStep::kAssign ? body.Pointed(1) : body.Pointed(0);
  std::optional<lir::Place> made;
  switch (step) {
    case lir::ValueStep::kCopy:
    case lir::ValueStep::kMove:
      made = body.Answer();
      break;
    case lir::ValueStep::kAssign:
      made = body.Pointed(0);
      break;
    case lir::ValueStep::kEnd:
      break;
  }

  // One part's step: `from` names where the part the step takes lies, and
  // `to` where the part it builds or writes over lies.
  const auto part = [&](lir::TypeId part_type, const lir::Place& from,
                        const std::optional<lir::Place>& to) {
    const lir::Type part_stated = out_.types.Get(part_type);
    switch (step) {
      case lir::ValueStep::kCopy:
        BuildPart(
            body, part_type, part_stated, lir::CopyValueTarget{}, from, *to);
        return;
      case lir::ValueStep::kMove:
        BuildPart(
            body, part_type, part_stated, lir::MoveValueTarget{}, from, *to);
        return;
      case lir::ValueStep::kAssign:
        body.Statement(
            lir::StoreInstr{.place = *to, .value = body.Load(from, part_type)});
        return;
      case lir::ValueStep::kEnd:
        if (lir::NeedsEnd(out_, part_type)) {
          body.Emit(
              void_type, lir::CallInstr{
                             .target = lir::EndValueTarget{},
                             .args = {body.Load(from, part_type)}});
        }
        return;
    }
  };

  // A value that is its bytes is stepped as them, whatever it is laid out as:
  // built by putting a reading of it in place, written by a store, and ended
  // by nothing.
  if (std::holds_alternative<lir::StepIsBytes>(lir::StepOf(out_, type, step))) {
    switch (step) {
      case lir::ValueStep::kCopy:
      case lir::ValueStep::kMove:
        body.Into(*made, lir::UseInstr{.operand = body.Load(taken, type)});
        break;
      case lir::ValueStep::kAssign:
        body.Statement(
            lir::StoreInstr{.place = *made, .value = body.Load(taken, type)});
        break;
      case lir::ValueStep::kEnd:
        break;
    }
    return std::move(body).Finish();
  }

  // A closure ends each of its captures where it lies, the last placed first,
  // as a C++ class ends its members.
  if (const auto* closure = stated.As<lir::ClosureType>()) {
    const std::size_t captures =
        out_.closures.Get(closure->closure_id).captures.size();
    for (auto i = static_cast<std::uint32_t>(captures); i-- > 0;) {
      lir::Place capture = taken;
      capture.chain.emplace_back(
          lir::MemberProjection{
              .member = lir::StatedMemberRef{
                  .declared_by = type, .slot = lir::MemberSlot{i}}});
      body.Statement(lir::EndStorageInstr{.place = std::move(capture)});
    }
    return std::move(body).Finish();
  }

  // An array steps each element in turn, in position order, counted by a
  // machine integer, which a step into the array reads as the ordinal it is.
  if (const auto* array = stated.As<lir::UnpackedArrayType>()) {
    const lir::TypeId position = out_.types.Intern(
        lir::Type{lir::MachineIntType{
            .width = lir::MachineIntWidth::k64,
            .signedness = lir::Signedness::kUnsigned}});
    const auto constant = [&](std::uint64_t value) -> lir::Operand {
      return lir::IntConst{
          .value =
              lir::IntegralConstant{.value_words = {value}, .state_words = {}},
          .type = position};
    };
    const lir::Place counter{
        .base = lir::Use{.value = body.Slot(position)}, .chain = {}};
    body.Into(counter, lir::UseInstr{.operand = constant(0)});
    const lir::BlockId test = body.NewBlock();
    const lir::BlockId each = body.NewBlock();
    const lir::BlockId done = body.NewBlock();
    body.Terminate(lir::BranchTerm{.target = test});
    body.SetCurrent(test);
    const lir::Operand index = body.Load(counter, position);
    const lir::Operand within = body.Emit(
        MachineBoolType(), lir::BinaryInstr{
                               .op = lir::BinaryOp::kLessThan,
                               .lhs = index,
                               .rhs = constant(array->size)});
    body.Terminate(
        lir::CondBranchTerm{
            .condition = within, .if_true = each, .if_false = done});
    body.SetCurrent(each);
    part(
        array->element_type, StepBody::Element(taken, index),
        made.transform([&](lir::Place to) {
          return StepBody::Element(std::move(to), index);
        }));
    body.Statement(
        lir::StoreInstr{
            .place = counter,
            .value = body.Emit(
                position, lir::BinaryInstr{
                              .op = lir::BinaryOp::kAdd,
                              .lhs = index,
                              .rhs = constant(1)})});
    body.Terminate(lir::BranchTerm{.target = test});
    body.SetCurrent(done);
    return std::move(body).Finish();
  }

  // A tagged union steps the member its tag names (LRM 7.3.2). A copy builds
  // the union holding a copy of that member, and so does a move, which leaves
  // the one moved from to be ended; a write over one ends what was there and
  // builds a copy of the other in its place, unless the two are one.
  if (lir::IsTaggedUnion(out_, type)) {
    const std::span<const lir::TypeId> members =
        *lir::OverlaidMembers(out_, type);
    if (step == lir::ValueStep::kAssign) {
      const lir::BlockId replace = body.NewBlock();
      const lir::BlockId done = body.NewBlock();
      const lir::Operand same = body.Emit(
          MachineBoolType(), lir::BinaryInstr{
                                 .op = lir::BinaryOp::kEquality,
                                 .lhs = lir::Use{.value = body.Param(0)},
                                 .rhs = lir::Use{.value = body.Param(1)}});
      body.Terminate(
          lir::CondBranchTerm{
              .condition = same, .if_true = done, .if_false = replace});
      body.SetCurrent(replace);
      const lir::Operand written = body.Load(taken, type);
      body.Statement(
          lir::EndAndReplaceInstr{
              .place = *made,
              .value = lir::CallInstr{
                  .target = lir::CopyValueTarget{}, .args = {written}}});
      body.Terminate(lir::BranchTerm{.target = done});
      body.SetCurrent(done);
      return std::move(body).Finish();
    }
    const lir::Operand tag = body.Emit(
        out_.types.Intern(
            lir::Type{lir::MachineIntType{
                .width = lir::MachineIntWidth::k64,
                .signedness = lir::Signedness::kUnsigned}}),
        lir::DiscriminantInstr{.place = taken});
    const lir::BlockId done = body.NewBlock();
    const lir::BlockId none = body.NewBlock();
    std::vector<lir::SwitchTarget> arms;
    std::vector<lir::BlockId> blocks;
    for (std::size_t k = 0; k < members.size(); ++k) {
      blocks.push_back(body.NewBlock());
      arms.push_back(
          lir::SwitchTarget{
              .value = static_cast<std::int64_t>(k), .target = blocks.back()});
    }
    body.Terminate(
        lir::SwitchTerm{
            .discriminant = tag,
            .targets = std::move(arms),
            .otherwise = none});
    body.SetCurrent(none);
    body.Terminate(lir::UnreachableTerm{});
    for (std::size_t k = 0; k < members.size(); ++k) {
      body.SetCurrent(blocks[k]);
      const lir::Place member = StepBody::Component(taken, k);
      if (step == lir::ValueStep::kEnd) {
        part(members[k], member, std::nullopt);
      } else {
        body.Into(
            *made,
            lir::UnionInstr{
                .member = base::ComponentIndex{static_cast<std::uint32_t>(k)},
                .value = body.Load(member, members[k])});
      }
      body.Terminate(lir::BranchTerm{.target = done});
    }
    body.SetCurrent(done);
    return std::move(body).Finish();
  }

  // A product steps each component in position order.
  const std::optional<std::span<const lir::TypeId>> components =
      lir::ProductElements(out_, type);
  if (!components.has_value()) {
    throw InternalError(
        "mir_to_lir: a step is stated for a type that has none of its own -- "
        "please report this as a bug");
  }
  for (std::size_t i = 0; i < components->size(); ++i) {
    part(
        (*components)[i], StepBody::Component(taken, i),
        made.transform([&](lir::Place to) {
          return StepBody::Component(std::move(to), i);
        }));
  }
  return std::move(body).Finish();
}

void UnitLowerer::StateClassSteps() {
  // Every class this unit defines that a value is built of begins and ends
  // its own parts and is created and ended whole, as a C++ compiler gives a
  // class its base object constructor and destructor, its allocating
  // creation and its deleting destructor. Each is a function under the symbol
  // a class extending it, or an object of it, reaches it by, in this unit or
  // another; an interface class holds no storage and no value is built of one.
  const lir::TypeId void_type = out_.types.Intern(lir::Type{lir::VoidType{}});
  for (const lir::ClassId id : out_.classes.Ids()) {
    const lir::Class& cls = out_.classes.Get(id);
    if (cls.defining_unit.has_value() || !cls.base.has_value()) continue;
    const lir::TypeId type =
        out_.types.Intern(lir::Type{lir::ObjectType{.class_id = id}});
    const std::string definition = *lir::DefinitionSymbol(out_, type);
    const auto pointer_to = [&](lir::TypeId pointee) {
      return out_.types.Intern(
          lir::Type{lir::PointerType{
              .pointee = pointee,
              .ownership = lir::PointerOwnership::kBorrowed,
              .mutability = lir::Mutability::kMutable}});
    };
    const lir::TypeId object = pointer_to(type);
    const auto stated = [&](std::string name, lir::Function fn) {
      fn.name = std::move(name);
      fn.linkage = cls.linkage;
      out_.functions.Add(std::move(fn));
    };
    // The parts the class places in the object at `o`: what it extends, and
    // then its own members.
    const auto parts = [&](StepBody& body) {
      const lir::Operand as_base = body.Emit(
          pointer_to(*cls.base),
          lir::CastInstr{.operand = lir::Use{.value = body.Param(0)}});
      std::vector<lir::Place> held{
          lir::Place{.base = as_base, .chain = {lir::DerefProjection{}}}};
      for (std::uint32_t i = 0; i < cls.members.size(); ++i) {
        lir::Place member = body.Pointed(0);
        member.chain.emplace_back(
            lir::MemberProjection{
                .member = lir::StatedMemberRef{
                    .declared_by = type, .slot = lir::MemberSlot{i}}});
        held.push_back(std::move(member));
      }
      return held;
    };

    {
      StepBody body({}, std::array{object}, void_type, false);
      for (lir::Place& part : parts(body)) {
        body.Statement(lir::BeginStorageInstr{.place = std::move(part)});
      }
      stated(
          lir::CreationSymbol(definition, lir::Creation::kBaseObject),
          std::move(body).Finish());
    }
    // Ending them ends the last begun first, as a C++ class ends its members.
    {
      StepBody body({}, std::array{object}, void_type, false);
      std::vector<lir::Place> held = parts(body);
      for (lir::Place& part : std::views::reverse(held)) {
        body.Statement(lir::EndStorageInstr{.place = std::move(part)});
      }
      stated(
          lir::DestructorSymbol(definition, lir::Destructor::kBaseObject),
          std::move(body).Finish());
    }
    // An object is created whole in storage of its own and is of its class
    // from then on, before any constructor runs (LRM 8.17 NOTE 2), as the
    // JVM's `new` gives an object all its fields and its class.
    {
      StepBody body({}, {}, object, true);
      const lir::Operand made = body.Emit(
          object, lir::CallInstr{
                      .target =
                          lir::DynamicStorageTarget{
                              .op = lir::DynamicStorageTarget::Op::kAllocate},
                      .args = {}});
      body.Statement(
          lir::BeginStorageInstr{
              .place =
                  lir::Place{.base = made, .chain = {lir::DerefProjection{}}}});
      body.Statement(lir::SetObjectClassInstr{.object = made, .cls = type});
      body.Into(body.Answer(), lir::UseInstr{.operand = made});
      stated(
          lir::CreationSymbol(definition, lir::Creation::kAllocating),
          std::move(body).Finish());
    }
    // Ending one whole ends its parts and gives its storage back, which is
    // what the class's definition holds as how its objects end.
    {
      StepBody body({}, std::array{object}, void_type, false);
      body.Statement(lir::EndStorageInstr{.place = body.Pointed(0)});
      body.Emit(
          void_type,
          lir::CallInstr{
              .target =
                  lir::DynamicStorageTarget{
                      .op = lir::DynamicStorageTarget::Op::kDeallocate},
              .args = {lir::Use{.value = body.Param(0)}}});
      stated(
          lir::DestructorSymbol(definition, lir::Destructor::kDeleting),
          std::move(body).Finish());
    }
  }
}

}  // namespace lyra::lowering::mir_to_lir
