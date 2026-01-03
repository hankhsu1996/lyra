"""Macro for defining SystemVerilog feature tests."""

load("@rules_cc//cc:cc_test.bzl", "cc_test")

def sv_feature_test(name, yaml, size = "large", deps = []):
    """Creates a test target that runs SV feature tests from a YAML file.

    Args:
        name: Name of the test target
        yaml: Path to the YAML test file (relative to tests/)
        size: Test size (small, medium, large)
        deps: Additional dependencies
    """
    cc_test(
        name = name,
        size = size,
        srcs = ["framework/sv_feature_tests.cpp"],
        data = [
            yaml,
            "//:sdk_headers",
        ],
        env = {
            "SV_TEST_YAML": "$(rootpath " + yaml + ")",
        },
        deps = [
            "//:core",
            "//tests:test_framework",
            "@googletest//:gtest",
        ] + deps,
    )
