#pragma once

// Forwarding header: RuntimeValue has moved to lyra::semantic.
// This header re-exports all symbols into lyra::mir::interp for backward
// compatibility. New code should include "lyra/semantic/value.hpp" directly.

#include "lyra/semantic/value.hpp"

namespace lyra::mir::interp {

// Re-export types
using RuntimeIntegral = semantic::RuntimeIntegral;
using RuntimeString = semantic::RuntimeString;
using RuntimeReal = semantic::RuntimeReal;
using RuntimeShortReal = semantic::RuntimeShortReal;
using RuntimeStruct = semantic::RuntimeStruct;
using RuntimeArray = semantic::RuntimeArray;
using RuntimeUnion = semantic::RuntimeUnion;
using RuntimeValue = semantic::RuntimeValue;

// Re-export factory functions
using semantic::Clone;
using semantic::MakeArray;
using semantic::MakeIntegral;
using semantic::MakeIntegralFromConstant;
using semantic::MakeIntegralSigned;
using semantic::MakeIntegralWide;
using semantic::MakeIntegralX;
using semantic::MakeReal;
using semantic::MakeShortReal;
using semantic::MakeString;
using semantic::MakeStruct;
using semantic::MakeUnion;

// Re-export type checks
using semantic::IsArray;
using semantic::IsIntegral;
using semantic::IsReal;
using semantic::IsShortReal;
using semantic::IsString;
using semantic::IsStruct;
using semantic::IsUnion;

// Re-export accessors
using semantic::AsArray;
using semantic::AsIntegral;
using semantic::AsReal;
using semantic::AsShortReal;
using semantic::AsString;
using semantic::AsStruct;
using semantic::AsUnion;

// Re-export conversion functions
using semantic::ToBinaryString;
using semantic::ToDecimalString;
using semantic::ToHexString;
using semantic::ToOctalString;
using semantic::ToString;

}  // namespace lyra::mir::interp
