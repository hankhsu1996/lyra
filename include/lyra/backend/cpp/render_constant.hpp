#pragma once

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/linkage.hpp"

namespace lyra::backend::cpp {

// The constant `constant`, as a text defined under `linkage` names it.
void WriteConstantReference(
    const mir::CompilationUnit& unit, support::Linkage linkage,
    UnitRenderReport& report, mir::ConstantId constant, TargetText& out);

// `value`, a value of `type`, as the C++ expression that is it, each constant
// it points at named as a text defined under `linkage` names one.
void WriteConstantValue(
    const mir::CompilationUnit& unit, support::Linkage linkage,
    UnitRenderReport& report, mir::TypeId type, const mir::ConstantValue& value,
    TargetText& out);

}  // namespace lyra::backend::cpp
