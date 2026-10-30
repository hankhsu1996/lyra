#include "lyra/hir/structure_bytes.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/published_clocking.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::hir {

namespace {

// Writes one structure. Every part ends where it can be seen to end -- a text
// behind its length, a list behind its count, each alternative behind a mark
// of its own -- so two structures that differ anywhere differ here.
class Writer {
 public:
  Writer(const UnitSignature& signature, const ReferenceBytes& reference)
      : signature_(&signature), reference_(&reference) {
  }

  void Text(std::string_view text) {
    out_ += std::format("{}:{}", text.size(), text);
  }

  void Number(std::int64_t number) {
    out_ += std::format("{};", number);
  }

  void Mark(char mark) {
    out_ += mark;
  }

  template <typename Enum>
  void Kind(Enum value) {
    Number(static_cast<std::int64_t>(value));
  }

  void Reference(const std::string& unit_name, const support::DefPath& path) {
    Text((*reference_)(unit_name, path));
  }

  // A path inside the unit the structure is of.
  void OwnPath(const support::DefPath& path) {
    Reference(signature_->unit_name, path);
  }

  template <typename T, typename Write>
  void List(const std::vector<T>& items, Write write) {
    Number(static_cast<std::int64_t>(items.size()));
    for (const T& item : items) write(item);
  }

  template <typename T, typename Id, typename Write>
  void Arena(const base::Arena<T, Id>& items, Write write) {
    Number(static_cast<std::int64_t>(items.size()));
    for (const Id id : items.Ids()) write(items.Get(id));
  }

  template <typename T, typename Write>
  void Optional(const std::optional<T>& item, Write write) {
    if (!item.has_value()) {
      Mark('-');
      return;
    }
    Mark('+');
    write(*item);
  }

  void Class(ClassId id) {
    const ClassKey& key = signature_->ClassKeyOf(id);
    Reference(key.unit_name, key.class_path);
  }

  void Covergroup(CovergroupId id) {
    const CovergroupKey& key = signature_->CovergroupKeyOf(id);
    Reference(key.unit_name, key.path);
  }

  void ScopeClass(ScopeClassId id) {
    const ScopeClassKey& key = signature_->ScopeClassKeyOf(id);
    Reference(key.unit_name, key.class_path);
  }

  void Integral(const IntegralConstant& value) {
    Number(value.width);
    Kind(value.signedness);
    Kind(value.state_kind);
    List(value.value_words, [&](std::uint64_t word) {
      Number(static_cast<std::int64_t>(word));
    });
    List(value.state_words, [&](std::uint64_t word) {
      Number(static_cast<std::int64_t>(word));
    });
  }

  void Constant(const ConstantValue& value) {
    std::visit(
        Overloaded{
            [&](const IntegralConstant& integral) {
              Mark('i');
              Integral(integral);
            },
            [&](const RealBits& real) {
              Mark('r');
              Number(static_cast<std::int64_t>(real.bits));
            },
            [&](const std::string& text) {
              Mark('t');
              Text(text);
            },
            [&](const std::vector<ConstantValue>& elements) {
              Mark('a');
              List(elements, [&](const ConstantValue& e) { Constant(e); });
            }},
        value.data);
  }

  // A type by what it is, through every type it holds, so where a pool
  // happened to place it says nothing.
  void Type(TypeId id) {
    const hir::Type& type = signature_->types.Get(id);
    type.Visit(
        Overloaded{
            [&](const ScalarBitType& t) {
              Mark('b');
              Kind(t.atom);
            },
            [&](const PackedArrayType& t) {
              Mark('p');
              Number(t.dim.left);
              Number(t.dim.right);
              Kind(t.signedness);
              Type(t.element_type);
            },
            [&](const PackedStructType& t) {
              Mark('s');
              Kind(t.signedness);
              List(t.fields, [&](const PackedAggregateField& f) {
                Text(f.name);
                Type(f.type);
              });
            },
            [&](const PackedUnionType& t) {
              Mark('u');
              Kind(t.signedness);
              Number(t.tagged ? 1 : 0);
              List(t.fields, [&](const PackedAggregateField& f) {
                Text(f.name);
                Type(f.type);
              });
            },
            [&](const EnumType& t) {
              Mark('e');
              Reference(t.declaration.unit_name, t.declaration.path);
            },
            [&](const UnpackedStructType& t) {
              Mark('S');
              Reference(t.declaration.unit_name, t.declaration.path);
            },
            [&](const UnpackedUnionType& t) {
              Mark('U');
              Reference(t.declaration.unit_name, t.declaration.path);
            },
            [&](const UnpackedArrayType& t) {
              Mark('[');
              Number(t.dim.left);
              Number(t.dim.right);
              Type(t.element_type);
            },
            [&](const DynamicArrayType& t) {
              Mark('d');
              Type(t.element_type);
            },
            [&](const QueueType& t) {
              Mark('q');
              Optional(t.max_bound, [&](std::uint64_t bound) {
                Number(static_cast<std::int64_t>(bound));
              });
              Type(t.element_type);
            },
            [&](const AssociativeArrayType& t) {
              Mark('m');
              Type(t.key_type);
              Type(t.element_type);
            },
            [&](const WildcardIndexType&) { Mark('*'); },
            [&](const StringType&) { Mark('t'); },
            [&](const EventType&) { Mark('v'); },
            [&](const RealType&) { Mark('r'); },
            [&](const ShortRealType&) { Mark('h'); },
            [&](const RealTimeType&) { Mark('R'); },
            [&](const ChandleType&) { Mark('c'); },
            [&](const ClassHandleType& t) {
              Mark('C');
              Class(t.class_id);
            },
            [&](const ImportedClassHandleType& t) {
              Mark('I');
              Text(support::ImportedRuntimeClassName(t.klass));
            },
            [&](const WeakHandleType& t) {
              Mark('w');
              Type(t.handle);
            },
            [&](const CovergroupHandleType& t) {
              Mark('g');
              Covergroup(t.covergroup);
            },
            [&](const UnitObjectType& t) {
              Mark('o');
              ScopeClass(t.scope_class);
            },
            [&](const UnitObjectsType& t) {
              Mark('O');
              List(t.ranges, [&](const UnpackedRange& r) {
                Number(r.left);
                Number(r.right);
              });
              List(t.kinds, [&](const UnitObjectType& k) {
                ScopeClass(k.scope_class);
              });
              List(t.taken, [&](std::uint32_t at) { Number(at); });
            },
            [&](const VirtualInterfaceType& t) {
              Mark('V');
              Reference(t.unit_name, support::DefPath{});
            },
            [&](const NullType&) { Mark('n'); },
            [&](const DynamicType&) { Mark('y'); },
            [&](const VoidType&) { Mark('0'); }});
  }

  void Interface(const CalleeInterface& callee) {
    Kind(callee.kind);
    List(callee.params, [&](const CalleeParam& p) {
      Kind(p.direction);
      Type(p.type);
      Optional(p.default_evaluator, [&](const std::string& e) { Text(e); });
    });
  }

  void Prototype(const SubroutinePrototype& prototype) {
    Text(prototype.name);
    Interface(prototype.interface);
    Type(prototype.result_type);
  }

  void Projection(const MemberProjection& projection) {
    Number(projection.member.value);
    List(projection.path, [&](const PublishedSelector& selector) {
      std::visit(
          Overloaded{
              [&](const PublishedElementSelector& s) {
                Mark('e');
                Number(s.index);
                Type(s.projected_type);
              },
              [&](const PublishedSliceSelector& s) {
                Mark('s');
                std::visit(
                    Overloaded{
                        [&](const PublishedConstantRange& r) {
                          Mark('c');
                          Number(r.left);
                          Number(r.right);
                        },
                        [&](const PublishedIndexedUpRange& r) {
                          Mark('u');
                          Number(r.base);
                          Number(r.width);
                        },
                        [&](const PublishedIndexedDownRange& r) {
                          Mark('d');
                          Number(r.base);
                          Number(r.width);
                        }},
                    s.range);
                Type(s.projected_type);
              }},
          selector);
    });
    Optional(projection.positions, [&](const PublishedPositions& p) {
      Number(p.position);
      Number(p.width);
    });
  }

  void Scope(const ScopeClassSignature& scope) {
    Arena(scope.members, [&](const PublishedMember& m) {
      Text(m.name);
      OwnPath(m.holder);
      Type(m.type);
      std::visit(
          Overloaded{
              [&](const VariableStorage&) { Mark('v'); },
              [&](const NetStorage&) { Mark('n'); },
              [&](const ReferenceStorage& s) {
                Mark('r');
                Kind(s.binding);
              },
              [&](const BorrowedObjectStorage&) { Mark('b'); }},
          m.storage);
    });
    Arena(scope.callables, [&](const SubroutinePrototype& p) { Prototype(p); });
    Arena(scope.loops, [&](const PublishedLoop& loop) { Text(loop.name); });
    Arena(scope.choices, [&](const PublishedChoice& choice) {
      Number(choice.construct);
    });
    Arena(scope.disable_targets, [&](const PublishedDisableTarget& target) {
      OwnPath(target.path);
    });
    List(scope.modports, [&](const PublishedModport& modport) {
      Text(modport.name);
      List(modport.ports, [&](const PublishedModportPort& port) {
        Text(port.name);
        std::visit(
            Overloaded{
                [&](const ViewDefinedPlace& place) {
                  Mark('p');
                  List(place.parts, [&](const MemberProjection& part) {
                    Projection(part);
                  });
                  Type(place.type);
                },
                [&](const ViewComputedValue& computed) {
                  Mark('c');
                  Number(computed.evaluate.value);
                  List(computed.observes, [&](PublishedMemberId member) {
                    Number(member.value);
                  });
                }},
            port.meaning);
      });
    });
    List(scope.clocking_blocks, [&](const PublishedClockingBlock& block) {
      Text(block.name);
      List(block.events, [&](const PublishedClockingEntry& entry) {
        Number(entry.evaluate.value);
        std::visit(
            Overloaded{
                [&](const PublishedClockingChange& change) {
                  Mark('c');
                  Kind(change.edge);
                  List(change.observes, [&](PublishedMemberId member) {
                    Number(member.value);
                  });
                },
                [&](const PublishedClockingTrigger&) { Mark('t'); }},
            entry.occurrence);
        Optional(entry.condition, [&](PublishedCallableId condition) {
          Number(condition.value);
        });
      });
    });
  }

  // What the unit declares in the scope `declaring`: its types, its classes
  // and its covergroups. Each is part of the structure of the scope it is
  // declared in and of no other, so which blocks a construction built never
  // reaches a scope holding them.
  void DeclaredIn(const support::DefPath& declaring) {
    Definitions(declaring);
    ClassDeclarations(declaring);
    Covergroups(declaring);
  }

  void Definitions(const support::DefPath& declaring) {
    std::vector<std::pair<std::string, const TypeDefinition*>> definitions;
    for (const auto& [declaration, definition] : signature_->type_definitions) {
      if (support::DeclaringScopeClassOf(declaration.path) != declaring) {
        continue;
      }
      definitions.emplace_back(
          (*reference_)(declaration.unit_name, declaration.path), &definition);
    }
    std::ranges::sort(
        definitions, {}, &decltype(definitions)::value_type::first);
    Number(static_cast<std::int64_t>(definitions.size()));
    const auto fields = [&](const std::vector<UnpackedAggregateField>& of) {
      List(of, [&](const UnpackedAggregateField& f) {
        Text(f.name);
        Type(f.type);
        Optional(f.default_init, [&](const ConstantValue& v) { Constant(v); });
        Kind(f.random);
      });
    };
    for (const auto& [declaration, definition] : definitions) {
      Text(declaration);
      std::visit(
          Overloaded{
              [&](const EnumDefinition& e) {
                Mark('e');
                Type(e.base_type);
                List(e.members, [&](const EnumMember& m) {
                  Text(m.name);
                  Integral(m.value);
                });
              },
              [&](const UnpackedStructDefinition& s) {
                Mark('s');
                fields(s.fields);
              },
              [&](const UnpackedUnionDefinition& u) {
                Mark('u');
                Number(u.tagged ? 1 : 0);
                fields(u.fields);
              }},
          *definition);
    }
  }

  void ClassDeclarations(const support::DefPath& declaring) {
    const auto properties = [&](const auto& of) {
      Arena(of, [&](const DeclaredProperty& p) {
        Text(p.name);
        Type(p.type);
        Kind(p.random);
      });
    };
    std::vector<const ClassDeclaration*> declared;
    for (const ClassDeclaration& cls : signature_->classes) {
      if (support::DeclaringScopeClassOf(cls.class_path) == declaring) {
        declared.push_back(&cls);
      }
    }
    List(declared, [&](const ClassDeclaration* of) {
      const ClassDeclaration& cls = *of;
      const auto named = [&](const NamedBehavior& behavior) {
        Class(behavior.introducer);
        Text(behavior.name);
      };
      OwnPath(cls.class_path);
      Optional(cls.base, [&](ClassId base) { Class(base); });
      Number(cls.is_interface_class ? 1 : 0);
      List(cls.implements, [&](ClassId id) { Class(id); });
      properties(cls.properties);
      properties(cls.static_properties);
      Optional(
          cls.constructor, [&](const CalleeInterface& c) { Interface(c); });
      Arena(cls.methods, [&](const DeclaredMethod& method) {
        Prototype(method.prototype);
        std::visit(
            Overloaded{
                [&](const TypeAssociated&) { Mark('t'); },
                [&](const NotVirtual&) { Mark('n'); },
                [&](const IntroducesVirtual& v) {
                  Mark('i');
                  Number(v.is_pure ? 1 : 0);
                },
                [&](const OverridesVirtual& v) {
                  Mark('o');
                  Number(v.is_pure ? 1 : 0);
                  named(v.overridden);
                }},
            method.dispatch);
        Optional(method.replaces, [&](RandomizationHook hook) { Kind(hook); });
        Optional(method.makes, [&](CovergroupId id) { Covergroup(id); });
      });
      Optional(cls.instance_scope, [&](const support::DefPath& scope) {
        OwnPath(scope);
      });
      List(cls.conforming, [&](const ConformingBehavior& answered) {
        named(answered.interface_behavior);
        Optional(answered.answered_by, named);
      });
    });
  }

  void Covergroups(const support::DefPath& declaring) {
    const auto formals = [&](const std::vector<PublishedCovergroupFormal>& of) {
      List(of, [&](const PublishedCovergroupFormal& f) {
        Text(f.name);
        Type(f.type);
        Kind(f.direction);
      });
    };
    std::vector<const PublishedCovergroup*> declared;
    for (const PublishedCovergroup& covergroup : signature_->covergroups) {
      if (support::DeclaringScopeClassOf(covergroup.path) == declaring) {
        declared.push_back(&covergroup);
      }
    }
    List(declared, [&](const PublishedCovergroup* of) {
      const PublishedCovergroup& covergroup = *of;
      OwnPath(covergroup.path);
      formals(covergroup.formals);
      formals(covergroup.sample_formals);
      Optional(covergroup.embedding, [&](ClassId id) { Class(id); });
      List(covergroup.coverpoints, [&](const PublishedCoverpoint& point) {
        Text(point.name);
        Type(point.value_type);
        Class(point.library_class);
      });
      List(covergroup.crosses, [&](const std::string& cross) { Text(cross); });
      Optional(covergroup.base, [&](CovergroupId id) { Covergroup(id); });
    });
  }

  void Unit() {
    DeclaredIn(support::DefPath{});
    std::visit(
        Overloaded{
            [&](const PublishedDesignElement& element) {
              Mark('d');
              List(element.ports, [&](const PortDecl& port) {
                Text(port.name);
                List(port.parts, [&](const PortPart& part) {
                  std::visit(
                      Overloaded{
                          [&](const DataPortPart& data) {
                            Mark('d');
                            Kind(data.direction);
                            Type(data.type);
                            std::visit(
                                Overloaded{
                                    [&](const MemberProjection& projection) {
                                      Mark('m');
                                      Projection(projection);
                                    },
                                    [&](const NoInternalTarget&) {
                                      Mark('-');
                                    }},
                                data.target);
                          },
                          [&](const InterfacePortPart& iface) {
                            Mark('i');
                            Number(iface.member.value);
                          }},
                      part);
                });
                Optional(port.default_value, [&](PublishedCallableId id) {
                  Number(id.value);
                });
              });
              List(element.handed, [&](TypeId type) { Type(type); });
              Scope(element.instance_class);
            },
            [&](const PublishedNamespace& space) {
              Mark('n');
              List(space.subroutines, [&](const SubroutinePrototype& p) {
                Prototype(p);
              });
            }},
        signature_->unit);
  }

  auto Take() -> std::string {
    return std::move(out_);
  }

 private:
  const UnitSignature* signature_;
  const ReferenceBytes* reference_;
  std::string out_;
};

}  // namespace

auto BlockClassBytes(
    const UnitSignature& signature, const ScopeClassSignature& scope,
    const ReferenceBytes& reference) -> std::string {
  Writer writer(signature, reference);
  writer.DeclaredIn(scope.class_path);
  writer.Scope(scope);
  return writer.Take();
}

auto StructureBytes(
    const UnitSignature& signature, const ReferenceBytes& reference)
    -> std::string {
  Writer writer(signature, reference);
  writer.Unit();
  return writer.Take();
}

}  // namespace lyra::hir
