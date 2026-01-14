#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::mir {

/// An enum member (name and value pair).
struct EnumMember {
  std::string name;
  int64_t value;
};

/// A type declaration within a package (typedef or enum).
struct TypeDeclaration {
  std::string name;
  common::Type type;                // The underlying type
  std::vector<EnumMember> members;  // For enums: the enum members
};

/// A parameter declaration within a package.
/// Both `parameter` and `localparam` in packages have identical semantics
/// (non-overridable compile-time constants).
struct PackageParameter {
  common::Variable variable;
  std::unique_ptr<Expression>
      initializer;  // Always non-null (evaluated by Slang)
};

/// A variable declaration within a package.
struct PackageVariable {
  common::Variable variable;
  std::unique_ptr<Expression> initializer;  // nullptr if no initializer
};

/// A SystemVerilog package containing shared declarations.
///
/// Packages provide a namespace for sharing type declarations across modules.
class Package {
 public:
  std::string name;
  std::vector<TypeDeclaration> types;
  std::vector<PackageParameter> parameters;
  std::vector<PackageVariable> variables;
  std::vector<FunctionDefinition> functions;
};

}  // namespace lyra::mir
