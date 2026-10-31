#include "lyra/lowering/ast_to_hir/environment.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

auto Specializations::EnvironmentOf(
    const slang::ast::InstanceSymbol& inst) const -> const Environment& {
  if (const auto kept = environments_.find(&inst);
      kept != environments_.end()) {
    return kept->second;
  }
  const slang::ast::InstanceBodySymbol& body =
      HasBodyOfItsOwn(inst) ? inst.body : *inst.getCanonicalBody();
  const InstanceContext& context = ContextOf(inst);
  const std::vector<ClimbAnchor> climbs = context.Climbs();

  std::vector<ClassHome> homes;
  homes.reserve(context.class_scopes.size());
  for (const slang::ast::Scope* scope : context.class_scopes) {
    homes.push_back(
        ClassHome{.scope = scope, .start = StartOfReach(*scope, body, climbs)});
  }

  std::vector<ForeignParameter> foreign;
  foreign.reserve(context.foreign_parameters.size());
  for (const slang::ast::ParameterSymbol* param : context.foreign_parameters) {
    const bool held_by_each_object =
        ValueSourceOf(InstanceHolding(*param->getParentScope()), *param) !=
        ParameterValueSource::kSettledAtElaboration;
    foreign.push_back(
        ForeignParameter{
            .parameter = param,
            .settled =
                held_by_each_object
                    ? std::nullopt
                    : std::optional<slang::ConstantValue>{param->getValue()}});
  }

  std::vector<PortBinding> ports;
  for (const slang::ast::PortConnection* connection :
       inst.getPortConnections()) {
    if (connection->port.kind != slang::ast::SymbolKind::InterfacePort) {
      continue;
    }
    ports.push_back(
        PortBinding{
            .port = &connection->port,
            .connected = ConnectedInterfaceOf(connection->getIfaceConn())});
  }

  return environments_
      .emplace(
          &inst, Environment(
                     context.uses, ParametersOf(inst), std::move(homes),
                     std::move(foreign), std::move(ports)))
      .first->second;
}

auto Environment::LandingOf(
    const slang::ast::Symbol& found,
    const slang::ast::HierarchicalReference& written) const
    -> const std::optional<ClimbAnchor>& {
  const auto same_path = [&](const NameUse& use) {
    if (use.path.size() != written.path.size()) return false;
    for (std::size_t at = 0; at < use.path.size(); ++at) {
      if (use.path[at] != written.path[at].symbol) return false;
    }
    return true;
  };
  for (const NameUse& use : names_) {
    if (use.found == &found && same_path(use)) return use.landing;
  }
  throw InternalError(
      std::format(
          "Environment: the name '{}' was lowered but never recorded where it "
          "lands, so what identifies its unit did not hold it -- please report "
          "this as a bug",
          found.name));
}

auto Environment::ReachOf(const slang::ast::Scope& scope) const
    -> const std::optional<ReachStart>& {
  for (const ClassHome& home : class_homes_) {
    if (home.scope == &scope) return home.start;
  }
  throw InternalError(
      std::format(
          "Environment: a class of '{}' was lowered but never recorded as one "
          "the body reaches, so what identifies its unit did not hold it -- "
          "please report this as a bug",
          scope.asSymbol().name));
}

auto Environment::SettledValueOf(const slang::ast::ParameterSymbol& parameter)
    const -> const std::optional<slang::ConstantValue>& {
  for (const ForeignParameter& read : foreign_parameters_) {
    if (read.parameter == &parameter) return read.settled;
  }
  throw InternalError(
      std::format(
          "Environment: the parameter '{}' of another instance was lowered but "
          "never recorded as read, so what identifies its unit did not hold "
          "it -- please report this as a bug",
          parameter.name));
}

auto Environment::BindingOf(const slang::ast::Symbol& port) const
    -> const ConnectedInterface& {
  for (const PortBinding& binding : ports_) {
    if (binding.port == &port) return binding.connected;
  }
  throw InternalError(
      std::format(
          "Environment: the interface port '{}' was lowered with no binding "
          "recorded -- please report this as a bug",
          port.name));
}

}  // namespace lyra::lowering::ast_to_hir
