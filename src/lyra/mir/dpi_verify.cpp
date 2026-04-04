#include "lyra/mir/dpi_verify.hpp"

#include <format>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/parameter_direction.hpp"

namespace lyra::mir {

void ValidateDpiSignatureContract(const DpiSignature& sig, const char* caller) {
  // Return descriptor: kind must match abi_type classification.
  if (sig.result.abi_type == DpiAbiTypeClass::kVoid) {
    if (sig.result.kind != DpiReturnKind::kVoid) {
      throw common::InternalError(
          caller, "return: abi_type is void but kind is not kVoid");
    }
  } else if (IsPackedVecDpiType(sig.result.abi_type)) {
    if (sig.result.kind != DpiReturnKind::kIndirect) {
      throw common::InternalError(
          caller, "return: packed-vec abi_type but kind is not kIndirect");
    }
  } else {
    if (sig.result.kind != DpiReturnKind::kDirectValue) {
      throw common::InternalError(
          caller, "return: scalar abi_type but kind is not kDirectValue");
    }
  }
  if (!IsValidDpiReturnType(sig.result.abi_type)) {
    throw common::InternalError(
        caller, "invalid DPI return type in frozen signature");
  }

  // Per-parameter structural contract.
  for (size_t i = 0; i < sig.params.size(); ++i) {
    const auto& p = sig.params[i];
    if (!IsValidDpiParamType(p.abi_type)) {
      throw common::InternalError(
          caller,
          std::format("param {}: invalid DPI ABI type in frozen signature", i));
    }
    if (p.direction == ParameterDirection::kRef) {
      throw common::InternalError(
          caller,
          std::format("param {}: ref direction in frozen DPI signature", i));
    }
    auto expected_passing = GetDpiPassingMode(p.direction, p.abi_type);
    if (p.passing != expected_passing) {
      throw common::InternalError(
          caller,
          std::format(
              "param {}: passing mode {} does not match expected {} for "
              "direction/abi_type",
              i, static_cast<int>(p.passing),
              static_cast<int>(expected_passing)));
    }
  }
}

void ValidateDpiCallContract(
    const DpiSignature& sig, std::span<const DpiArgBinding> args,
    const char* caller) {
  if (args.size() != sig.params.size()) {
    throw common::InternalError(
        caller, std::format(
                    "binding count {} != param count {}", args.size(),
                    sig.params.size()));
  }
  for (size_t i = 0; i < args.size(); ++i) {
    const auto& binding = args[i];
    const auto& param = sig.params[i];
    bool has_input = binding.input_value.has_value();
    bool has_dest = binding.writeback_dest.has_value();

    switch (param.direction) {
      case ParameterDirection::kInput:
        if (!has_input || has_dest) {
          throw common::InternalError(
              caller,
              std::format(
                  "param {}: input binding must have input_value only", i));
        }
        break;
      case ParameterDirection::kOutput:
        if (has_input || !has_dest) {
          throw common::InternalError(
              caller,
              std::format(
                  "param {}: output binding must have writeback_dest only", i));
        }
        break;
      case ParameterDirection::kInOut:
        if (!has_input || !has_dest) {
          throw common::InternalError(
              caller,
              std::format(
                  "param {}: inout binding must have both input_value and "
                  "writeback_dest",
                  i));
        }
        break;
      case ParameterDirection::kRef:
        throw common::InternalError(
            caller, std::format("param {}: ref not supported in DPI", i));
    }
  }
}

}  // namespace lyra::mir
