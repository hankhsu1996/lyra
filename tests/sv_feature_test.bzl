"""Custom test rule for SystemVerilog feature tests."""

def _sv_feature_test_impl(ctx):
    """Implementation of sv_feature_test rule."""

    # Generate wrapper script at build time
    script = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(
        output = script,
        content = """#!/bin/bash
exec "{binary}" --gtest_filter="{filter}"
""".format(
            binary = ctx.executable.binary.short_path,
            filter = ctx.attr.filter,
        ),
        is_executable = True,
    )

    # Collect runfiles from the binary
    runfiles = ctx.runfiles(files = [ctx.executable.binary])
    runfiles = runfiles.merge(ctx.attr.binary[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        executable = script,
        runfiles = runfiles,
    )]

_sv_feature_test = rule(
    implementation = _sv_feature_test_impl,
    test = True,
    attrs = {
        "binary": attr.label(
            executable = True,
            cfg = "target",
            mandatory = True,
        ),
        "filter": attr.string(mandatory = True),
    },
)

def sv_feature_test(name, yaml, size = "large"):
    """Creates a test target that runs SV feature tests from a YAML file.

    This creates a thin wrapper around the unified sv_feature_tests binary,
    filtering to run only tests from the specified YAML file.

    Args:
        name: Test target name (e.g., "operators_binary_tests")
        yaml: Path to YAML file (e.g., "sv_features/operators/binary.yaml")
        size: Test size (default "large")
    """

    # Extract category from yaml path for gtest filter
    # e.g., "sv_features/operators/binary.yaml" -> "operators_binary"
    parts = yaml.replace("sv_features/", "").replace(".yaml", "").split("/")
    filter_pattern = "*" + "_".join(parts) + "_*"

    _sv_feature_test(
        name = name,
        binary = ":sv_feature_tests_bin",
        filter = filter_pattern,
        size = size,
    )
