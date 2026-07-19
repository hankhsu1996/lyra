#include "lyra/hir/type.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A class the runtime library defines and every unit imports by reference is a
// direct member of the built-in `std` package (LRM 9.7 `process` is the first
// Lyra supports). Keying on the declaring package plus the name -- rather than
// a bare name match anywhere -- is what makes this the library declaration's
// identity, not a user class that happens to be named `process`.
auto DetectImportedRuntimeClass(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass> {
  const slang::ast::Scope* scope = cls.getParentScope();
  if (scope == nullptr) {
    return std::nullopt;
  }
  const auto& std_package = scope->getCompilation().getStdPackage();
  if (scope != static_cast<const slang::ast::Scope*>(&std_package)) {
    return std::nullopt;
  }
  if (cls.name == "process") {
    return support::ImportedRuntimeClass::kProcess;
  }
  return std::nullopt;
}

// An enum nested in an imported runtime class (LRM 9.7 `process::state`) is a
// value the runtime returns and the program compares, with no unit-emitted enum
// declaration behind it. It lowers to its underlying integral type.
auto EnumBelongsToImportedRuntimeClass(const slang::ast::EnumType& enum_type)
    -> bool {
  const slang::ast::Scope* scope = enum_type.getParentScope();
  if (scope == nullptr) {
    return false;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::ClassType) {
    return false;
  }
  return DetectImportedRuntimeClass(owner.as<slang::ast::ClassType>())
      .has_value();
}

auto LowerScalarAtom(slang::ast::ScalarType::Kind k) -> hir::BitAtom {
  switch (k) {
    case slang::ast::ScalarType::Bit:
      return hir::BitAtom::kBit;
    case slang::ast::ScalarType::Logic:
      return hir::BitAtom::kLogic;
    case slang::ast::ScalarType::Reg:
      return hir::BitAtom::kReg;
  }
  throw InternalError("LowerScalarAtom: unknown scalar kind");
}

auto LowerRange(const slang::ConstantRange& r) -> hir::PackedRange {
  return hir::PackedRange{
      .left = static_cast<std::int64_t>(r.left),
      .right = static_cast<std::int64_t>(r.right),
  };
}

// A predefined-width integer is a single-dimension packed array over a scalar
// bit (LRM 7.4.1); `form` records the syntactic origin (`int` vs the equivalent
// `bit signed [31:0]`).
auto LowerPredefinedInteger(
    const UnitLowerer& unit_lowerer, slang::ast::PredefinedIntegerType::Kind k)
    -> hir::PackedArrayType {
  using SK = slang::ast::PredefinedIntegerType::Kind;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const auto make = [&](hir::TypeId element, std::int64_t msb,
                        hir::Signedness signedness,
                        hir::PackedArrayForm form) -> hir::PackedArrayType {
    return hir::PackedArrayType{
        .dim = hir::PackedRange{.left = msb, .right = 0},
        .element_type = element,
        .signedness = signedness,
        .form = form,
    };
  };
  switch (k) {
    case SK::Byte:
      return make(
          builtins.scalar_bit, 7, hir::Signedness::kSigned,
          hir::PackedArrayForm::kByte);
    case SK::ShortInt:
      return make(
          builtins.scalar_bit, 15, hir::Signedness::kSigned,
          hir::PackedArrayForm::kShortInt);
    case SK::Int:
      return make(
          builtins.scalar_bit, 31, hir::Signedness::kSigned,
          hir::PackedArrayForm::kInt);
    case SK::LongInt:
      return make(
          builtins.scalar_bit, 63, hir::Signedness::kSigned,
          hir::PackedArrayForm::kLongInt);
    case SK::Integer:
      return make(
          builtins.scalar_logic, 31, hir::Signedness::kSigned,
          hir::PackedArrayForm::kInteger);
    case SK::Time:
      return make(
          builtins.scalar_logic, 63, hir::Signedness::kUnsigned,
          hir::PackedArrayForm::kTime);
  }
  throw InternalError("LowerPredefinedInteger: unknown integer kind");
}

// One HIR node per declared dimension (LRM 7.4.1: a packed array is recursively
// other packed arrays / structures). The element is interned and named by its
// `TypeId`, so the nest -- and an aggregate element's identity -- is carried by
// recursion, not flattened here.
auto LowerExplicitPackedArray(
    UnitLowerer& unit_lowerer, const slang::ast::PackedArrayType& array,
    bool outer_signed, diag::SourceSpan span)
    -> diag::Result<hir::PackedArrayType> {
  auto element = unit_lowerer.InternType(array.elementType, span);
  if (!element) return std::unexpected(std::move(element.error()));
  return hir::PackedArrayType{
      .dim = LowerRange(array.range),
      .element_type = *element,
      .signedness =
          outer_signed ? hir::Signedness::kSigned : hir::Signedness::kUnsigned,
      .form = hir::PackedArrayForm::kExplicit,
  };
}

auto LowerPackedStruct(
    const slang::ast::PackedStructType& struct_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::PackedStructType> {
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       struct_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = unit_lowerer.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    const auto field_width =
        static_cast<std::uint64_t>(field.getType().getBitWidth());
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
            .bit_offset = field.bitOffset,
            .bit_width = field_width,
        });
  }
  return hir::PackedStructType{
      .fields = std::move(fields),
      .signedness = struct_type.isSigned ? hir::Signedness::kSigned
                                         : hir::Signedness::kUnsigned,
      .four_state = struct_type.isFourState,
  };
}

auto LowerPackedUnion(
    const slang::ast::PackedUnionType& union_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::PackedUnionType> {
  if (union_type.isTagged) {
    return diag::Fail(
        decl_span, diag::DiagCode::kUnsupportedTaggedPackedUnion,
        "tagged packed unions are not yet supported");
  }
  std::vector<hir::PackedAggregateField> fields;
  for (const auto& field :
       union_type.membersOfType<slang::ast::FieldSymbol>()) {
    auto field_type_or = unit_lowerer.InternType(field.getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    const auto field_width =
        static_cast<std::uint64_t>(field.getType().getBitWidth());
    fields.push_back(
        hir::PackedAggregateField{
            .name = std::string(field.name),
            .type = *field_type_or,
            .bit_offset = 0U,
            .bit_width = field_width,
        });
  }
  return hir::PackedUnionType{
      .fields = std::move(fields),
      .signedness = union_type.isSigned ? hir::Signedness::kSigned
                                        : hir::Signedness::kUnsigned,
      .four_state = union_type.isFourState,
  };
}

// LRM 7.2.2: a member declaration may carry a constant default value, used in
// place of the member type's Table 7-1 default when the enclosing struct is
// default-constructed. Slang has bound and constant-checked it; evaluate it and
// fold it into the member's value-form metadata, the same shape an enum member
// carries its value -- the default is a value the type holds, not an
// expression.
auto LowerMemberDefault(
    const slang::ast::FieldSymbol& field, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::ConstantValue>> {
  const auto* init = field.getInitializer();
  if (init == nullptr) {
    return std::optional<hir::ConstantValue>{};
  }
  slang::ast::EvalContext eval_context(field);
  const slang::ConstantValue constant = init->eval(eval_context);
  auto value_or = MakeConstantValue(constant, span);
  if (!value_or) {
    return std::unexpected(std::move(value_or.error()));
  }
  return std::optional<hir::ConstantValue>{*std::move(value_or)};
}

auto LowerUnpackedAggregateFields(
    std::span<const slang::ast::FieldSymbol* const> fields,
    diag::SourceSpan decl_span, UnitLowerer& unit_lowerer)
    -> diag::Result<std::vector<hir::UnpackedAggregateField>> {
  std::vector<hir::UnpackedAggregateField> out;
  out.reserve(fields.size());
  for (const auto* field : fields) {
    auto field_type_or = unit_lowerer.InternType(field->getType(), decl_span);
    if (!field_type_or) {
      return std::unexpected(std::move(field_type_or.error()));
    }
    auto default_or = LowerMemberDefault(*field, decl_span);
    if (!default_or) {
      return std::unexpected(std::move(default_or.error()));
    }
    out.push_back(
        hir::UnpackedAggregateField{
            .name = std::string(field->name),
            .type = *field_type_or,
            .default_init = *std::move(default_or),
        });
  }
  return out;
}

auto LowerUnpackedStruct(
    const slang::ast::UnpackedStructType& struct_type,
    diag::SourceSpan decl_span, UnitLowerer& unit_lowerer)
    -> diag::Result<hir::UnpackedStructType> {
  auto fields_or =
      LowerUnpackedAggregateFields(struct_type.fields, decl_span, unit_lowerer);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedStructType{.fields = *std::move(fields_or)};
}

auto LowerUnpackedUnion(
    const slang::ast::UnpackedUnionType& union_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::UnpackedUnionType> {
  // A tagged union (LRM 7.3.2) is a type-checked sum type, a separate concept
  // from the untagged overlapping-storage form; reject it here where the
  // declaration span is available, so MIR translation only ever sees untagged
  // unions.
  if (union_type.isTagged) {
    return diag::Fail(
        decl_span, diag::DiagCode::kUnsupportedUnpackedUnionType,
        "tagged unions are not yet supported");
  }
  auto fields_or =
      LowerUnpackedAggregateFields(union_type.fields, decl_span, unit_lowerer);
  if (!fields_or) return std::unexpected(std::move(fields_or.error()));
  return hir::UnpackedUnionType{
      .fields = *std::move(fields_or),
      .tagged = union_type.isTagged,
  };
}

auto LowerEnum(
    const slang::ast::EnumType& enum_type, diag::SourceSpan decl_span,
    UnitLowerer& unit_lowerer) -> diag::Result<hir::EnumType> {
  auto base_id_or = unit_lowerer.InternType(enum_type.baseType, decl_span);
  if (!base_id_or) return std::unexpected(std::move(base_id_or.error()));
  std::vector<hir::EnumMember> members;
  for (const auto& value_sym : enum_type.values()) {
    const auto& cv = value_sym.getValue();
    if (!cv.isInteger()) {
      throw InternalError("LowerEnum: enum value is not integral");
    }
    members.push_back(
        hir::EnumMember{
            .name = std::string(value_sym.name),
            .value = LowerSVIntToIntegralConstant(cv.integer()),
        });
  }
  return hir::EnumType{
      .base_type = *base_id_or,
      .members = std::move(members),
  };
}

auto TranslateTypeData(
    UnitLowerer& unit_lowerer, const slang::ast::Type& type,
    diag::SourceSpan decl_span) -> diag::Result<hir::TypeData> {
  const auto& canonical = type.getCanonicalType();

  switch (canonical.kind) {
    case slang::ast::SymbolKind::ScalarType: {
      const auto& scalar = canonical.as<slang::ast::ScalarType>();
      return hir::TypeData{
          hir::ScalarBitType{.atom = LowerScalarAtom(scalar.scalarKind)}};
    }
    case slang::ast::SymbolKind::PredefinedIntegerType: {
      const auto& pi = canonical.as<slang::ast::PredefinedIntegerType>();
      return hir::TypeData{
          LowerPredefinedInteger(unit_lowerer, pi.integerKind)};
    }
    case slang::ast::SymbolKind::PackedArrayType: {
      auto pa = LowerExplicitPackedArray(
          unit_lowerer, canonical.as<slang::ast::PackedArrayType>(),
          canonical.isSigned(), decl_span);
      if (!pa.has_value()) {
        return std::unexpected(std::move(pa.error()));
      }
      return hir::TypeData{*std::move(pa)};
    }
    case slang::ast::SymbolKind::EnumType: {
      const auto& enum_type = canonical.as<slang::ast::EnumType>();
      if (EnumBelongsToImportedRuntimeClass(enum_type)) {
        return TranslateTypeData(unit_lowerer, enum_type.baseType, decl_span);
      }
      auto e = LowerEnum(enum_type, decl_span, unit_lowerer);
      if (!e.has_value()) {
        return std::unexpected(std::move(e.error()));
      }
      return hir::TypeData{*std::move(e)};
    }
    case slang::ast::SymbolKind::FloatingType: {
      const auto& f = canonical.as<slang::ast::FloatingType>();
      switch (f.floatKind) {
        case slang::ast::FloatingType::Real:
          return hir::TypeData{hir::RealType{}};
        case slang::ast::FloatingType::ShortReal:
          return hir::TypeData{hir::ShortRealType{}};
        case slang::ast::FloatingType::RealTime:
          return hir::TypeData{hir::RealTimeType{}};
      }
      throw InternalError("TranslateTypeData: unknown FloatingType kind");
    }
    case slang::ast::SymbolKind::StringType:
      return hir::TypeData{hir::StringType{}};
    case slang::ast::SymbolKind::EventType:
      return hir::TypeData{hir::EventType{}};
    case slang::ast::SymbolKind::CHandleType:
      return hir::TypeData{hir::ChandleType{}};
    case slang::ast::SymbolKind::NullType:
      return hir::TypeData{hir::NullType{}};
    case slang::ast::SymbolKind::ClassType: {
      const auto& class_type = canonical.as<slang::ast::ClassType>();
      if (const auto imported = DetectImportedRuntimeClass(class_type)) {
        return hir::TypeData{hir::ImportedClassHandleType{.klass = *imported}};
      }
      auto class_ref_or = unit_lowerer.ResolveClassRef(class_type, decl_span);
      if (!class_ref_or) {
        return std::unexpected(std::move(class_ref_or.error()));
      }
      return hir::TypeData{
          hir::ClassHandleType{.class_ref = *std::move(class_ref_or)}};
    }
    case slang::ast::SymbolKind::VoidType:
      return hir::TypeData{hir::VoidType{}};
    case slang::ast::SymbolKind::PackedStructType: {
      auto s = LowerPackedStruct(
          canonical.as<slang::ast::PackedStructType>(), decl_span,
          unit_lowerer);
      if (!s.has_value()) {
        return std::unexpected(std::move(s.error()));
      }
      return hir::TypeData{*std::move(s)};
    }
    case slang::ast::SymbolKind::PackedUnionType: {
      auto u = LowerPackedUnion(
          canonical.as<slang::ast::PackedUnionType>(), decl_span, unit_lowerer);
      if (!u.has_value()) {
        return std::unexpected(std::move(u.error()));
      }
      return hir::TypeData{*std::move(u)};
    }
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& fa = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      auto elem_id_or = unit_lowerer.InternType(fa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::TypeData{hir::UnpackedArrayType{
          .element_type = *elem_id_or,
          .dim =
              hir::UnpackedRange{
                  .left = static_cast<std::int64_t>(fa.range.left),
                  .right = static_cast<std::int64_t>(fa.range.right)},
      }};
    }
    case slang::ast::SymbolKind::DynamicArrayType: {
      const auto& da = canonical.as<slang::ast::DynamicArrayType>();
      auto elem_id_or = unit_lowerer.InternType(da.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      return hir::TypeData{hir::DynamicArrayType{.element_type = *elem_id_or}};
    }
    case slang::ast::SymbolKind::QueueType: {
      const auto& q = canonical.as<slang::ast::QueueType>();
      auto elem_id_or = unit_lowerer.InternType(q.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // slang encodes the unbounded queue (`[$]`) as maxBound == 0; a bounded
      // queue (`[$:N]`) carries the bound (LRM 7.10.4).
      return hir::TypeData{hir::QueueType{
          .element_type = *elem_id_or,
          .max_bound = q.maxBound == 0
                           ? std::nullopt
                           : std::optional<std::uint64_t>{q.maxBound},
      }};
    }
    case slang::ast::SymbolKind::AssociativeArrayType: {
      const auto& aa = canonical.as<slang::ast::AssociativeArrayType>();
      auto elem_id_or = unit_lowerer.InternType(aa.elementType, decl_span);
      if (!elem_id_or) {
        return std::unexpected(std::move(elem_id_or.error()));
      }
      // LRM 7.8.1 wildcard index (`[*]`) declares no index type: the key is any
      // integral value identified by its magnitude, carried by the dedicated
      // wildcard-index key type.
      if (aa.indexType == nullptr) {
        return hir::TypeData{hir::AssociativeArrayType{
            .element_type = *elem_id_or,
            .key_type = unit_lowerer.Unit().builtins.wildcard_index,
        }};
      }
      // A declared index covers string (LRM 7.8.2), integral, which includes a
      // packed struct / enum (LRM 7.8.4 / 7.8.5), and chandle, whose entries
      // may order arbitrarily (LRM 6.14). A class index (LRM 7.8.3) and a real
      // index are rejected.
      if (!(aa.indexType->isIntegral() || aa.indexType->isString() ||
            aa.indexType->isCHandle())) {
        return diag::Fail(
            decl_span, diag::DiagCode::kUnsupportedAssociativeArrayType,
            "associative arrays are only supported with a string, integral, "
            "chandle, or wildcard index type");
      }
      auto key_id_or = unit_lowerer.InternType(*aa.indexType, decl_span);
      if (!key_id_or) {
        return std::unexpected(std::move(key_id_or.error()));
      }
      return hir::TypeData{hir::AssociativeArrayType{
          .element_type = *elem_id_or,
          .key_type = *key_id_or,
      }};
    }
    case slang::ast::SymbolKind::UnpackedStructType: {
      auto s = LowerUnpackedStruct(
          canonical.as<slang::ast::UnpackedStructType>(), decl_span,
          unit_lowerer);
      if (!s) return std::unexpected(std::move(s.error()));
      return hir::TypeData{*std::move(s)};
    }
    case slang::ast::SymbolKind::UnpackedUnionType: {
      auto u = LowerUnpackedUnion(
          canonical.as<slang::ast::UnpackedUnionType>(), decl_span,
          unit_lowerer);
      if (!u) return std::unexpected(std::move(u.error()));
      return hir::TypeData{*std::move(u)};
    }
    default:
      return diag::Fail(
          decl_span, diag::DiagCode::kUnsupportedTypeKind,
          "unsupported type kind");
  }
}

// LRM 8.7: a class the source declares without a `function new` still has a
// constructor -- the implicit `new`, whose only effect is the property
// initialization every class performs. It is modeled as a constructor with no
// formals and an empty body; the initialization itself is composed onto the
// body separately, the same way it is for a user-written constructor.
// Looks up the given method name in `derived_cls`'s base chain for a pure
// virtual prototype (LRM 8.21) an implementation on `derived_cls` should
// override. Slang's `checkForOverride` establishes a
// `SubroutineSymbol::getOverride()` link only when the base method is itself
// a `SubroutineSymbol`; when the base declares the method as `pure virtual`
// (a `MethodPrototypeSymbol`), slang leaves the derived method's
// `overrides` unset, so this lookup re-establishes the link. Slang's own
// `Scope::find` already flattens the ancestor chain by unwrapping the
// `TransparentMember` wrappers each inheriting scope inserts, so one query
// on the immediate base scope reaches the actual declaration wherever it
// sits in the chain.
auto FindOverriddenPureInBaseChain(
    const slang::ast::ClassType& derived_cls, std::string_view method_name)
    -> const slang::ast::MethodPrototypeSymbol* {
  const auto* base_type = derived_cls.getBaseClass();
  if (base_type == nullptr) return nullptr;
  const auto& base_cls =
      base_type->getCanonicalType().as<slang::ast::ClassType>();
  const auto* found = base_cls.find(method_name);
  if (found == nullptr ||
      found->kind != slang::ast::SymbolKind::MethodPrototype) {
    return nullptr;
  }
  const auto& proto = found->as<slang::ast::MethodPrototypeSymbol>();
  if (!proto.flags.has(slang::ast::MethodFlags::Pure)) return nullptr;
  return &proto;
}

auto SynthesizeDefaultConstructor(hir::TypeId void_type, diag::SourceSpan span)
    -> hir::SubroutineDecl {
  hir::ProceduralBody body;
  body.root_stmt = body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockStmt{.statements = {}, .scope = std::nullopt},
          .span = span});
  return hir::SubroutineDecl{
      .name = "new",
      .kind = hir::SubroutineKind::kFunction,
      .result_type = void_type,
      .params = {},
      .result_var = std::nullopt,
      .body = std::move(body),
      .is_virtual = false,
      .overrides = std::nullopt};
}

// Walks a class's parent-scope chain to find the compilation-unit-level
// symbol that owns its declaration -- a package (LRM 26), a module body
// (LRM 23.2), or an interface body (LRM 25). A class nested in another
// class or in a generate block still hits the enclosing module or package
// body first, which is the ownership boundary. Returns null when the class
// has no such ancestor (only expected for imported runtime-library classes,
// which never reach this helper).
auto DeclaringCompilationUnit(const slang::ast::ClassType& cls)
    -> const slang::ast::Symbol* {
  const slang::ast::Scope* scope = cls.getParentScope();
  while (scope != nullptr) {
    const slang::ast::Symbol& owner = scope->asSymbol();
    if (owner.kind == slang::ast::SymbolKind::Package ||
        owner.kind == slang::ast::SymbolKind::InstanceBody) {
      return &owner;
    }
    scope = owner.getParentScope();
  }
  return nullptr;
}

// The name a compilation unit publishes for itself: a package's declared
// name, or a module body's specialization name (the same name that unit
// registers its own artifact under). A consumer reaching a class of this
// unit by name uses this name for the unit component of the reference; the
// class component is `SpecializationName(cls)`, computed on both sides
// deterministically without a shared table.
auto CompilationUnitName(const slang::ast::Symbol& unit) -> std::string {
  if (unit.kind == slang::ast::SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == slang::ast::SymbolKind::InstanceBody) {
    return SpecializationName(unit.as<slang::ast::InstanceBodySymbol>());
  }
  throw InternalError(
      "CompilationUnitName: symbol is not a package or module body");
}

}  // namespace

auto UnitLowerer::MakeClassMethodTarget(
    const hir::ClassRef& class_ref,
    const slang::ast::SubroutineSymbol& method) const
    -> hir::ClassMethodTarget {
  if (const auto* local = std::get_if<hir::LocalClassRef>(&class_ref)) {
    return hir::LocalClassMethodTarget{
        .owner = local->class_id, .method = LookupMethodId(method)};
  }
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  return hir::ExternalClassMethodTarget{
      .unit_name = ext.unit_name,
      .class_name = ext.class_name,
      .method_name = std::string(method.name)};
}

auto UnitLowerer::MakeClassPropertyTarget(
    const hir::ClassRef& class_ref,
    const slang::ast::ClassPropertySymbol& prop) const
    -> hir::ClassPropertyTarget {
  if (const auto* local = std::get_if<hir::LocalClassRef>(&class_ref)) {
    return hir::LocalClassPropertyTarget{
        .owner = local->class_id, .field = LookupClassPropertyFieldId(prop)};
  }
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  return hir::ExternalClassPropertyTarget{
      .unit_name = ext.unit_name,
      .class_name = ext.class_name,
      .property_name = std::string(prop.name)};
}

auto UnitLowerer::MakeStaticPropertyTarget(
    const hir::ClassRef& class_ref,
    const slang::ast::ClassPropertySymbol& prop) const
    -> hir::StaticPropertyTarget {
  if (const auto* local = std::get_if<hir::LocalClassRef>(&class_ref)) {
    return hir::LocalStaticPropertyTarget{
        .owner = local->class_id, .prop = LookupClassPropertyStaticId(prop)};
  }
  const auto& ext = std::get<hir::ExternalClassRef>(class_ref);
  return hir::ExternalStaticPropertyTarget{
      .unit_name = ext.unit_name,
      .class_name = ext.class_name,
      .property_name = std::string(prop.name)};
}

auto UnitLowerer::ResolveClassRef(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassRef> {
  // A class this unit has already classified reads its `ClassRef` straight
  // from the cache. Only the first reference to a class runs the boundary
  // walk that answers "which CU declares it?" and stores the result.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return it->second;
  }
  const slang::ast::Symbol* decl_unit = DeclaringCompilationUnit(cls);
  if (decl_unit == nullptr) {
    throw InternalError(
        "UnitLowerer::ResolveClassRef: class has no compilation-unit-level "
        "declaring scope");
  }
  if (decl_unit != &scope_->asSymbol()) {
    const auto [it, _] = class_cache_.emplace(
        &cls, hir::ClassRef{hir::ExternalClassRef{
                  .unit_name = CompilationUnitName(*decl_unit),
                  .class_name = SpecializationName(cls)}});
    return it->second;
  }
  // A local class not yet minted (e.g. a class nested inside a generate
  // block, which the top-level scope walk does not reach): mint it lazily
  // now, so a reference and the pre-pass both converge on the same identity
  // regardless of which route saw the class first.
  auto id_or = InternLocalClass(cls, span);
  if (!id_or) return std::unexpected(std::move(id_or.error()));
  return hir::ClassRef{hir::LocalClassRef{.class_id = *id_or}};
}

auto UnitLowerer::InternLocalClass(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<hir::ClassId> {
  // Idempotent: a repeat call for the same class returns the id the first
  // mint installed. The cache entry is populated before body population, so a
  // cyclic reference during that population -- a class that names itself in
  // one of its member types, or a mutually-referential pair -- resolves
  // against a stable id already visible to peer lookups.
  if (const auto it = class_cache_.find(&cls); it != class_cache_.end()) {
    return std::get<hir::LocalClassRef>(it->second).class_id;
  }
  const hir::ClassId id = unit_.classes.Declare();
  class_cache_.emplace(&cls, hir::ClassRef{hir::LocalClassRef{.class_id = id}});

  hir::ClassDecl decl;
  decl.name = SpecializationName(cls);

  // Base class (LRM 8.13). Slang exposes the base as a `ClassType*` on the
  // derived class and flattens inherited members into the derived's member
  // list; the base is reached through the class-reference translator because
  // it may live in another compilation unit, and each member iteration below
  // filters by parent scope so only members declared on this class enter
  // its arena.
  if (const auto* base_type = cls.getBaseClass()) {
    const auto& base_class =
        base_type->getCanonicalType().as<slang::ast::ClassType>();
    if (base_class.isInterface) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "interface class conformance is not yet supported");
    }
    auto base_ref = ResolveClassRef(base_class, span);
    if (!base_ref) return std::unexpected(std::move(base_ref.error()));
    decl.base = *std::move(base_ref);
  }

  for (const auto& prop :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (prop.getParentScope() != &cls) {
      continue;
    }
    auto prop_type = InternType(prop.getType(), span);
    if (!prop_type) return std::unexpected(std::move(prop_type.error()));
    // LRM 8.9 / 8.10 keyword position: `static <T> x` marks a type-associated
    // property, whose storage is one cell owned by the class rather than a
    // per-instance member. Instance properties and static properties live in
    // disjoint arenas because their identity spaces do not overlap and each
    // downstream reference form names one or the other.
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      const hir::StaticPropertyId static_id = decl.static_properties.Add(
          hir::ClassStaticProperty{
              .name = std::string(prop.name), .type = *prop_type});
      RegisterClassPropertyStaticId(prop, static_id);
      continue;
    }
    const hir::FieldId field_id = decl.fields.Add(
        hir::ClassField{.name = std::string(prop.name), .type = *prop_type});
    RegisterClassPropertyFieldId(prop, field_id);
  }
  std::optional<hir::SubroutineDecl> user_constructor;
  for (const auto& method : cls.membersOfType<slang::ast::SubroutineSymbol>()) {
    if (method.getParentScope() != &cls) {
      continue;
    }
    // Every class carries compiler-generated built-ins (the randomize family,
    // LRM 18.6); they are provided by the runtime, not lowered from source.
    if (method.flags.has(slang::ast::MethodFlags::BuiltIn)) {
      continue;
    }
    if (method.flags.has(slang::ast::MethodFlags::Constructor)) {
      for (const auto* formal : method.getArguments()) {
        if (formal->direction != slang::ast::ArgumentDirection::In) {
          return diag::Fail(
              span, diag::DiagCode::kUnsupportedClassFeature,
              "a constructor with an output / inout / ref argument is not yet "
              "supported");
        }
      }
      // `getBaseConstructorCall` returns the `super.new(...)` slang lifted out
      // of the ctor body: null when the source did not write one (LRM 8.7
      // implicit forwarding, materialized as an empty stated base_init in
      // HIR-to-MIR) or when this class has no base.
      const slang::ast::Expression* base_call_ast =
          cls.getBaseConstructorCall();
      auto ctor_or =
          LowerConstructorDecl(*this, method, WalkFrame{}, base_call_ast);
      if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
      user_constructor = std::move(ctor_or->constructor);
      decl.base_call = std::move(ctor_or->base_call);
      continue;
    }
    if (method.subroutineKind == slang::ast::SubroutineKind::Task) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "class task methods are not yet supported");
    }
    auto method_decl = LowerSubroutineDecl(*this, method, WalkFrame{});
    if (!method_decl) return std::unexpected(std::move(method_decl.error()));
    // Source-level facts on a class method: `static` (LRM 8.10) makes the
    // signature receiver-less; `virtual` (LRM 8.20) puts the method in the
    // class's dispatch table. Slang enforces at declaration that static and
    // virtual are mutually exclusive, so the override / dispatch wiring
    // below runs only for non-static methods.
    method_decl->is_static = method.flags.has(slang::ast::MethodFlags::Static);
    method_decl->is_virtual =
        method.flags.has(slang::ast::MethodFlags::Virtual);
    if (!method_decl->is_static) {
      // The frontend has already matched signature, direction, return type,
      // and any other LRM 8.20 override compatibility rule when it resolved
      // this reference; the HIR consumes it as an already-resolved identity.
      // The base link the class shape carries has resolved a `LocalClassRef`
      // for a same-unit base or an `ExternalClassRef` for a cross-unit base;
      // the override target follows the same axis, owner-qualified in either
      // case.
      if (const auto* overridden = method.getOverride();
          overridden != nullptr) {
        const auto& base_class = overridden->getParentScope()
                                     ->asSymbol()
                                     .as<slang::ast::ClassType>();
        auto base_ref = ResolveClassRef(base_class, span);
        if (!base_ref) return std::unexpected(std::move(base_ref.error()));
        method_decl->overrides = MakeClassMethodTarget(*base_ref, *overridden);
      } else if (const auto* pure_base =
                     FindOverriddenPureInBaseChain(cls, method.name);
                 pure_base != nullptr) {
        // Slang leaves the override link unset when the base method is a
        // pure virtual prototype (LRM 8.21); wire it here so downstream
        // dispatch canonicalizes to the base's slot rather than treating
        // this method as a fresh introducer.
        const auto& base_class =
            pure_base->getParentScope()->asSymbol().as<slang::ast::ClassType>();
        auto base_ref = ResolveClassRef(base_class, span);
        if (!base_ref) return std::unexpected(std::move(base_ref.error()));
        const auto* proto_stub = pure_base->getSubroutine();
        if (proto_stub == nullptr) {
          throw InternalError(
              "UnitLowerer::InternLocalClass: pure virtual prototype has no "
              "stub subroutine slang normally materializes");
        }
        method_decl->overrides = MakeClassMethodTarget(*base_ref, *proto_stub);
        // An override of a base virtual method is itself virtual (LRM 8.20),
        // even when the derived declaration omits the `virtual` keyword.
        method_decl->is_virtual = true;
      }
    }
    const hir::MethodId method_id = decl.methods.Add(*std::move(method_decl));
    RegisterMethodId(method, method_id);
  }

  // A pure virtual method (LRM 8.21 `pure virtual function ...;`) lands in
  // slang as a `MethodPrototypeSymbol`, not a `SubroutineSymbol`, because it
  // has no body. Its signature still occupies a class-owned method slot the
  // dispatch table introduces and every extended concrete class must fill,
  // so it enters the same `methods` arena that carries ordinary instance
  // methods; the record is marked `is_prototype` so the backend renders it
  // as a C++ pure declaration (`= 0;`) instead of a body.
  for (const auto& proto :
       cls.membersOfType<slang::ast::MethodPrototypeSymbol>()) {
    if (proto.getParentScope() != &cls) {
      continue;
    }
    if (!proto.flags.has(slang::ast::MethodFlags::Pure)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "extern / out-of-block class method prototypes are not yet "
          "supported");
    }
    if (proto.subroutineKind == slang::ast::SubroutineKind::Task) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedClassFeature,
          "pure virtual class task prototypes are not yet supported");
    }
    auto proto_decl = LowerMethodPrototypeDecl(*this, proto);
    if (!proto_decl) return std::unexpected(std::move(proto_decl.error()));
    // A middle abstract class re-declaring an ancestor's pure virtual method
    // as another `pure virtual` produces a prototype whose overrides link
    // slang exposes as a `Symbol*` (either the ancestor prototype's stub or
    // another prototype). Translate the reachable stub form to the HIR
    // identity registered when that ancestor was interned.
    if (const auto* overridden = proto.getOverride(); overridden != nullptr) {
      const slang::ast::SubroutineSymbol* overridden_sub = nullptr;
      if (overridden->kind == slang::ast::SymbolKind::Subroutine) {
        overridden_sub = &overridden->as<slang::ast::SubroutineSymbol>();
      } else if (overridden->kind == slang::ast::SymbolKind::MethodPrototype) {
        overridden_sub =
            overridden->as<slang::ast::MethodPrototypeSymbol>().getSubroutine();
      }
      if (overridden_sub != nullptr) {
        const auto& base_class = overridden_sub->getParentScope()
                                     ->asSymbol()
                                     .as<slang::ast::ClassType>();
        auto base_ref = ResolveClassRef(base_class, span);
        if (!base_ref) return std::unexpected(std::move(base_ref.error()));
        proto_decl->overrides =
            MakeClassMethodTarget(*base_ref, *overridden_sub);
      }
    }
    const hir::MethodId method_id = decl.methods.Add(*std::move(proto_decl));
    // Register the prototype's stub subroutine so a downstream override
    // walker or the `FindOverriddenPureInBaseChain` fallback can translate
    // the prototype to its HIR method identity through the same
    // `SubroutineSymbol`-keyed registry every other override lookup uses.
    if (const auto* proto_stub = proto.getSubroutine(); proto_stub != nullptr) {
      RegisterMethodId(*proto_stub, method_id);
    }
  }

  hir::SubroutineDecl constructor =
      user_constructor.has_value()
          ? *std::move(user_constructor)
          : SynthesizeDefaultConstructor(unit_.builtins.void_type, span);

  // A property initializer (LRM 8.7) runs during construction and may read
  // another property through the receiver, so it lowers on the procedural path
  // -- the one that resolves a property name to a receiver-relative reference
  // -- into the constructor body's expression arena that holds it. The class is
  // the containing symbol so any structural name in the initializer routes from
  // the class's position.
  ProcessLowerer init_lowerer(*this, cls);
  const WalkFrame init_frame = WalkFrame{}.WithProceduralBody(
      &constructor.body, &constructor.body.exprs);
  // A static property initializer (LRM 8.9 / 10.5) runs once at design init,
  // not per instance, so its expression lands in the class's `static_init`
  // arena rather than the constructor body's. It cannot read a per-instance
  // formal or `self`, and the lowering routes that no such receiver is in
  // scope; a downstream initializer that names another static property of
  // the same class reads it as `Cls::other_prop` (a `StaticPropertyRef`),
  // never through the constructor's receiver.
  const WalkFrame static_init_frame = WalkFrame{}.WithProceduralBody(
      &decl.static_init, &decl.static_init.exprs);
  for (const auto& prop :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (prop.getParentScope() != &cls) continue;
    const auto* init = prop.getInitializer();
    if (init == nullptr) continue;
    if (prop.lifetime == slang::ast::VariableLifetime::Static) {
      auto init_expr = init_lowerer.LowerExpr(*init, static_init_frame);
      if (!init_expr) return std::unexpected(std::move(init_expr.error()));
      decl.static_property_inits.push_back(
          hir::StaticPropertyInit{
              .target = LookupClassPropertyStaticId(prop),
              .value = decl.static_init.exprs.Add(*std::move(init_expr))});
      continue;
    }
    auto init_expr = init_lowerer.LowerExpr(*init, init_frame);
    if (!init_expr) return std::unexpected(std::move(init_expr.error()));
    decl.field_inits.push_back(
        hir::FieldInit{
            .target = LookupClassPropertyFieldId(prop),
            .value = constructor.body.exprs.Add(*std::move(init_expr))});
  }
  decl.constructor = std::move(constructor);

  unit_.classes.Define(id, std::move(decl));
  return id;
}

auto UnitLowerer::AddComposedType(hir::TypeData data) -> hir::TypeId {
  return unit_.types.Add(hir::Type{.data = std::move(data)});
}

auto UnitLowerer::InternType(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::TypeId> {
  const auto* canonical = &type.getCanonicalType();
  if (const auto it = type_cache_.find(canonical); it != type_cache_.end()) {
    return it->second;
  }
  auto data_or = TranslateTypeData(*this, type, span);
  if (!data_or) return std::unexpected(std::move(data_or.error()));
  const hir::TypeId id =
      unit_.types.Add(hir::Type{.data = *std::move(data_or)});
  type_cache_.emplace(canonical, id);
  return id;
}

}  // namespace lyra::lowering::ast_to_hir
