#pragma once

// What one instance's body reaches outside itself. A body is a definition's
// text, and the same text means different things in different instances only
// through what its names resolve to outside it -- a name searched upward (LRM
// 23.8), a class an enclosing module declares and hands down as a type (LRM
// 6.20.3), a parameter another instance holds, the instances an interface port
// is bound to (LRM 25.3) -- and through which of its parameters its parent
// hands it. So an instance is its body's text and this record, as a closure is
// its code and its captures.
//
// The record is worked out once, from the instance, before any unit is
// lowered; what identifies the instance is rendered from it, and the lowering
// of the body takes every such fact from it and from nowhere else. A fact the
// lowering asks for and the record does not hold is a fact the identity never
// held either, which is this compiler's defect and is reported as one.

#include <optional>
#include <span>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/numeric/ConstantValue.h>

#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/instance_context.hpp"

namespace slang::ast {
class HierarchicalReference;
class ParameterSymbol;
class Scope;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a value parameter's value comes from, for the objects built from one
// compiled scope. Settled at elaboration: the code reads it as the constant
// this specialization settled it to. Supplied at construction: whoever builds
// the object passes it -- an instance's parameter its code reads (LRM 23.10.2),
// or the index a loop's block was built at (LRM 27.4). Computed at
// construction: its own declaration writes it from a supplied value, directly
// or through another such, so each object computes it when built.
enum class ParameterValueSource {
  kSettledAtElaboration,
  kSuppliedAtConstruction,
  kComputedAtConstruction,
};

// Where each value parameter of an instance's body takes its value from, and
// those its objects are handed, in the order the body declares them.
struct ParametersOfInstance {
  std::vector<const slang::ast::ParameterSymbol*> supplied;
  std::unordered_map<const slang::ast::ParameterSymbol*, ParameterValueSource>
      sources;
};

// The scope declaring a class or covergroup the body uses that stands in
// another instance, and where the body starts reaching it, or nothing where it
// cannot.
struct ClassHome {
  const slang::ast::Scope* scope = nullptr;
  std::optional<ReachStart> start;
};

// A parameter of another instance the body reads: the constant it settled to,
// which the body's code is compiled with, or nothing where the objects of that
// instance each hold it and a read of it reads their cell.
struct ForeignParameter {
  const slang::ast::ParameterSymbol* parameter = nullptr;
  std::optional<slang::ConstantValue> settled;
};

// An interface port of the body and what the instance's parent bound to it.
struct PortBinding {
  const slang::ast::Symbol* port = nullptr;
  ConnectedInterface connected;
};

class Environment {
 public:
  // The environment of a unit no instance is an application of: a namespace,
  // or a specialization of a generic class that is a unit of its own. Its body
  // reaches nothing outside itself through an instance.
  Environment() = default;

  Environment(
      std::vector<NameUse> names, ParametersOfInstance parameters,
      std::vector<ClassHome> class_homes,
      std::vector<ForeignParameter> foreign_parameters,
      std::vector<PortBinding> ports)
      : names_(std::move(names)),
        parameters_(std::move(parameters)),
        class_homes_(std::move(class_homes)),
        foreign_parameters_(std::move(foreign_parameters)),
        ports_(std::move(ports)) {
  }

  // Where the name `found`, reached by the path `written`, lands once it
  // leaves the instance, or nothing where it stays inside.
  [[nodiscard]] auto LandingOf(
      const slang::ast::Symbol& found,
      const slang::ast::HierarchicalReference& written) const
      -> const std::optional<ClimbAnchor>&;

  // Where the body starts reaching `scope`, the scope of another instance
  // declaring a class or covergroup it uses, or nothing where it cannot.
  [[nodiscard]] auto ReachOf(const slang::ast::Scope& scope) const
      -> const std::optional<ReachStart>&;

  // The constant `parameter`, another instance's, settled to, or nothing where
  // the objects of that instance each hold its value.
  [[nodiscard]] auto SettledValueOf(
      const slang::ast::ParameterSymbol& parameter) const
      -> const std::optional<slang::ConstantValue>&;

  // What the parent bound to the interface port `port`.
  [[nodiscard]] auto BindingOf(const slang::ast::Symbol& port) const
      -> const ConnectedInterface&;

  [[nodiscard]] auto Parameters() const -> const ParametersOfInstance& {
    return parameters_;
  }

  [[nodiscard]] auto ClassHomes() const -> std::span<const ClassHome> {
    return class_homes_;
  }
  [[nodiscard]] auto ForeignParameters() const
      -> std::span<const ForeignParameter> {
    return foreign_parameters_;
  }
  [[nodiscard]] auto Ports() const -> std::span<const PortBinding> {
    return ports_;
  }

 private:
  std::vector<NameUse> names_;
  ParametersOfInstance parameters_;
  std::vector<ClassHome> class_homes_;
  std::vector<ForeignParameter> foreign_parameters_;
  std::vector<PortBinding> ports_;
};

}  // namespace lyra::lowering::ast_to_hir
