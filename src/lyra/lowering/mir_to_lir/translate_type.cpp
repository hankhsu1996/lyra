#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto TranslateStateKind(mir::IntegralStateKind s) -> lir::IntegralStateKind {
  switch (s) {
    case mir::IntegralStateKind::kTwoState:
      return lir::IntegralStateKind::kTwoState;
    case mir::IntegralStateKind::kFourState:
      return lir::IntegralStateKind::kFourState;
  }
  throw InternalError("TranslateStateKind: unknown IntegralStateKind");
}

auto TranslateSignedness(mir::Signedness s) -> lir::Signedness {
  return s == mir::Signedness::kSigned ? lir::Signedness::kSigned
                                       : lir::Signedness::kUnsigned;
}

auto TranslateMachineIntWidth(mir::MachineIntWidth w) -> lir::MachineIntWidth {
  switch (w) {
    case mir::MachineIntWidth::k8:
      return lir::MachineIntWidth::k8;
    case mir::MachineIntWidth::k16:
      return lir::MachineIntWidth::k16;
    case mir::MachineIntWidth::k32:
      return lir::MachineIntWidth::k32;
    case mir::MachineIntWidth::k64:
      return lir::MachineIntWidth::k64;
  }
  throw InternalError("TranslateMachineIntWidth: unknown MachineIntWidth");
}

auto TranslatePointerOwnership(mir::PointerOwnership o)
    -> lir::PointerOwnership {
  switch (o) {
    case mir::PointerOwnership::kUnique:
      return lir::PointerOwnership::kUnique;
    case mir::PointerOwnership::kShared:
      return lir::PointerOwnership::kShared;
    case mir::PointerOwnership::kBorrowed:
      return lir::PointerOwnership::kBorrowed;
  }
  throw InternalError("TranslatePointerOwnership: unknown PointerOwnership");
}

auto TranslateMutability(mir::Mutability m) -> lir::Mutability {
  return m == mir::Mutability::kReadOnly ? lir::Mutability::kReadOnly
                                         : lir::Mutability::kMutable;
}

auto TranslateIntegral(const mir::IntegralType& integral) -> lir::IntegralType {
  return lir::IntegralType{
      .bit_width = integral.bit_width,
      .signedness = TranslateSignedness(integral.signedness),
      .state_kind = TranslateStateKind(integral.state_kind)};
}

}  // namespace

auto UnitLowerer::TranslateType(mir::TypeId id) -> lir::TypeId {
  if (const auto it = type_memo_.find(id); it != type_memo_.end()) {
    return it->second;
  }
  const lir::TypeId lir_id =
      out_.types.Intern(TranslateType(mir_->types.Get(id)));
  type_memo_.emplace(id, lir_id);
  return lir_id;
}

auto UnitLowerer::TranslateTypes(std::span<const mir::TypeId> source)
    -> std::vector<lir::TypeId> {
  std::vector<lir::TypeId> types;
  types.reserve(source.size());
  for (const mir::TypeId type : source) {
    types.push_back(TranslateType(type));
  }
  return types;
}

auto UnitLowerer::TranslateType(const mir::Type& ty) -> lir::Type {
  return ty.Visit(
      Overloaded{
          [&](const mir::IntegralType& integral) -> lir::Type {
            return lir::Type{TranslateIntegral(integral)};
          },
          // What an enumeration answers about a value is asked of the member
          // list the unit states for it, so below MIR nothing distinguishes an
          // enumeration from its base.
          [&](const mir::EnumType& e) -> lir::Type {
            return lir::Type{TranslateIntegral(e.base)};
          },
          [&](const mir::UnpackedArrayType& ua) -> lir::Type {
            return lir::Type{lir::UnpackedArrayType{
                .element_type = TranslateType(ua.element_type),
                .size = ua.Size()}};
          },
          // Reached only through a reference, which is translated whole.
          [](const mir::ErasedValueType&) -> lir::Type {
            throw InternalError(
                "mir to lir: a value of no known type is translated apart from "
                "the reference holding it -- please report this as a bug");
          },
          [](const mir::MachineCStringType&) -> lir::Type {
            return lir::Type{lir::MachineCStringType{}};
          },
          [](const mir::MachineBoolType&) -> lir::Type {
            return lir::Type{lir::MachineBoolType{}};
          },
          [](const mir::MachineIntType& mi) -> lir::Type {
            return lir::Type{lir::MachineIntType{
                .width = TranslateMachineIntWidth(mi.width),
                .signedness = TranslateSignedness(mi.signedness)}};
          },
          [](const mir::MachineFloatType& mf) -> lir::Type {
            return lir::Type{lir::MachineFloatType{.width = mf.width}};
          },
          [&](const mir::MachineArrayType& ma) -> lir::Type {
            return lir::Type{lir::MachineArrayType{
                .element = TranslateType(ma.element), .size = ma.size}};
          },
          [&](const mir::MachineSpanType& ms) -> lir::Type {
            return lir::Type{
                lir::MachineSpanType{.element = TranslateType(ms.element)}};
          },
          [&](const mir::MachineFunctionType& mf) -> lir::Type {
            return lir::Type{lir::MachineFunctionType{
                .params = TranslateTypes(mf.params),
                .result = TranslateType(mf.result)}};
          },
          [](const mir::RealType& r) -> lir::Type {
            return lir::Type{lir::RealType{.width = r.width}};
          },
          [](const mir::VoidType&) -> lir::Type {
            return lir::Type{lir::VoidType{}};
          },
          [&](const mir::ObjectType& ob) -> lir::Type {
            return Types().Get(ClassValueType(ob.of));
          },
          [&](const mir::LibraryType& l) -> lir::Type {
            std::vector<lir::LibraryType::Arg> args;
            args.reserve(l.args.size());
            for (const mir::LibraryType::Arg& arg : l.args) {
              args.push_back(
                  std::visit(
                      Overloaded{
                          [&](mir::TypeId type) -> lir::LibraryType::Arg {
                            return TranslateType(type);
                          },
                          [](const support::QueueBound& bound)
                              -> lir::LibraryType::Arg { return bound; }},
                      arg));
            }
            return lir::Type{
                lir::LibraryType{.decl = l.decl, .args = std::move(args)}};
          },
          [&](const mir::CoroutineType& co) -> lir::Type {
            return lir::Type{
                lir::CoroutineType{.payload = TranslateType(co.payload)}};
          },
          // A reference names the storage its referent lives in, and that
          // storage may be a subscribable variable or one nothing subscribes
          // to (LRM 13.5.2). A body taking a reference is lowered once for
          // every caller, so the type cannot say which of the two it was
          // handed: what it states is the values that storage holds.
          [&](const mir::VariableRefType& r) -> lir::Type {
            if (mir_->types.Get(r.pointee).Is<mir::ErasedValueType>()) {
              return lir::Type{lir::AnyReferenceType{}};
            }
            return lir::Type{lir::VariableRefType{
                .pointee = TranslateType(r.pointee),
                .mutability = TranslateMutability(r.mutability)}};
          },
          // Bound later than a reference created bound, and named through
          // alike, so LIR carries either as one reference.
          [&](const mir::ReferenceWrapperType& r) -> lir::Type {
            return lir::Type{lir::VariableRefType{
                .pointee = TranslateType(r.pointee),
                .mutability = TranslateMutability(r.mutability)}};
          },
          [&](const mir::PointerType& pt) -> lir::Type {
            return lir::Type{lir::PointerType{
                .pointee = TranslateType(pt.pointee),
                .ownership = TranslatePointerOwnership(pt.ownership),
                .mutability = TranslateMutability(pt.mutability)}};
          },
          // Executed, a reference is the address of its referent, as a C++
          // reference is to clang's code generation: what sets it apart from a
          // borrowed pointer -- bound once, never null -- is a rule its holder
          // keeps, not something the address carries.
          [&](const mir::ReferenceType& r) -> lir::Type {
            return lir::Type{lir::PointerType{
                .pointee = TranslateType(r.referent),
                .ownership = lir::PointerOwnership::kBorrowed,
                .mutability = TranslateMutability(r.mutability)}};
          },
          [&](const mir::ManagedRefType& mr) -> lir::Type {
            return lir::Type{
                lir::ManagedRefType{.pointee = TranslateType(mr.pointee)}};
          },
          [&](const mir::UnmanagedRefType& u) -> lir::Type {
            return lir::Type{
                lir::UnmanagedRefType{.pointee = TranslateType(u.pointee)}};
          },
          [&](const mir::VectorType& v) -> lir::Type {
            return lir::Type{
                lir::VectorType{.element = TranslateType(v.element)}};
          },
          [&](const mir::TupleType& t) -> lir::Type {
            return lir::Type{
                lir::TupleType{.elements = TranslateTypes(t.elements)}};
          },
          [&](const mir::ResolvedType& r) -> lir::Type {
            return lir::Type{
                lir::ResolvedType{.value = TranslateType(r.value)}};
          },
          [&](const mir::DriverType& d) -> lir::Type {
            return lir::Type{lir::DriverType{.value = TranslateType(d.value)}};
          },
          [&](const mir::OpenWriteType& w) -> lir::Type {
            return lir::Type{
                lir::OpenWriteType{.value = TranslateType(w.value)}};
          },
          [&](const mir::DesignationType& d) -> lir::Type {
            return lir::Type{
                lir::DesignationType{.value = TranslateType(d.value)}};
          },
          // A reference to bits is written through where it is taken, so the
          // write it is taken for is what lowers, and no value of it does.
          [](const mir::BitsReferenceType&) -> lir::Type {
            throw InternalError(
                "mir_to_lir: a reference to bits is written through where it "
                "is taken and is never a value of its own -- please report "
                "this as a bug");
          },
          // Which class the write answers its object as is a fact of the
          // writer's own accesses, each of which states its member's class, so
          // below MIR the write is the one write every class shares.
          [](const mir::ObjectWriteType&) -> lir::Type {
            return lir::Type{lir::ObjectWriteType{}};
          },
          [&](const mir::ObservableType& ob) -> lir::Type {
            return lir::Type{
                lir::ObservableType{.value = TranslateType(ob.value)}};
          },
          [&](const mir::RecordType& r) -> lir::Type {
            return lir::Type{lir::RecordType{
                .declaration = RecordDeclaration(r.declaration)}};
          },
          [&](const mir::ClosureType& c) -> lir::Type {
            return lir::Type{lir::ClosureType{
                .closure_id = ClosureDeclaration(c.closure_id)}};
          },
          [&](const mir::ErasedCallableType& e) -> lir::Type {
            return lir::Type{lir::ErasedCallableType{
                .params = TranslateTypes(e.params),
                .result = TranslateType(e.result)}};
          }});
}

auto UnitLowerer::ControlEffectType() const -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::LibraryType{
          .decl = support::LibraryDecl::kControlEffect, .args = {}}});
}

}  // namespace lyra::lowering::mir_to_lir
