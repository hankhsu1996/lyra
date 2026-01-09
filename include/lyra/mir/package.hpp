// Copyright 2024 Lyra Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef LYRA_MIR_PACKAGE_HPP
#define LYRA_MIR_PACKAGE_HPP

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/expression.hpp"

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
  std::vector<PackageVariable> variables;
};

}  // namespace lyra::mir

#endif  // LYRA_MIR_PACKAGE_HPP
