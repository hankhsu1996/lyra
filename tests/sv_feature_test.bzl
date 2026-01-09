"""Macro for creating SystemVerilog feature test targets."""

def sv_feature_test(name, yaml, size = "large"):
    """Creates a test target that runs SV feature tests from a YAML file.

    This creates a thin sh_test wrapper around the unified sv_feature_tests
    binary, filtering to run only tests from the specified YAML file.

    Args:
        name: Test target name (e.g., "operators_binary_tests")
        yaml: Path to YAML file (e.g., "sv_features/operators/binary.yaml")
        size: Test size (default "large")
    """

    # Extract category from yaml path for gtest filter
    # e.g., "sv_features/operators/binary.yaml" -> "operators_binary"
    parts = yaml.replace("sv_features/", "").replace(".yaml", "").split("/")
    filter_pattern = "_".join(parts) + "_*"

    native.sh_test(
        name = name,
        srcs = ["framework/run_sv_test.sh"],
        args = [
            "$(rootpath :sv_feature_tests_bin)",
            "*" + filter_pattern,
        ],
        data = [
            ":sv_feature_tests_bin",
            yaml,
            "//:embedded_headers",
        ],
        size = size,
    )
