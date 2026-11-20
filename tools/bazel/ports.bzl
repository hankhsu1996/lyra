"""Prepare the ports of supported third-party libraries that Lyra ships.

A port is an upstream release as Lyra prepares it: the release fetched and
patched by `MODULE.bazel`, and the library manifest Lyra adds. Its identity is
the directory holding its `BUILD.bazel`, `ports/<name>/<version>/<port_version>`,
so a port version's recipe sits apart from every other's and editing one cannot
reach another. A published port version is never changed: the digest its
content was published with is stated beside it, and content with another
digest fails the test that reads every shipped port back.
"""

PortInfo = provider(
    doc = "A port's identity, its prepared tree's files and its published digest.",
    fields = {
        "name": "the library's name",
        "version": "the upstream release",
        "port_version": "the revision of Lyra's preparation of that release",
        "files": "list of (path within the tree, File)",
        "digest": "the digest the content was published with",
    },
)

def _port_impl(ctx):
    parts = ctx.label.package.split("/")
    if len(parts) != 4 or parts[0] != "ports":
        fail("a port lives at ports/<name>/<version>/<port_version>, not " + ctx.label.package)
    files = []
    for f in ctx.files.tree:
        root = f.owner.workspace_root
        if not root:
            fail("%s is not part of a fetched release" % f.path)
        files.append((f.path[len(root) + 1:], f))
    files.append(("lyra.toml", ctx.file.manifest))
    return [PortInfo(
        name = parts[1],
        version = parts[2],
        port_version = parts[3],
        files = files,
        digest = ctx.attr.digest,
    )]

port = rule(
    implementation = _port_impl,
    doc = "A port of the release `tree` holds, compiled as `manifest` says.",
    attrs = {
        "digest": attr.string(
            doc = "The digest the prepared content was published with.",
            mandatory = True,
        ),
        "manifest": attr.label(
            allow_single_file = True,
            doc = "The prepared library's `lyra.toml`.",
            mandatory = True,
        ),
        "tree": attr.label(
            doc = "The fetched and patched release's files the port ships.",
            mandatory = True,
        ),
    },
)

def _quoted(text):
    return '"%s"' % text.replace("\\", "\\\\").replace('"', '\\"')

def _port_store_impl(ctx):
    staged = []
    for target in ctx.attr.ports:
        info = target[PortInfo]
        base = "%s/%s/%s/%s/" % (ctx.attr.root, info.name, info.version, info.port_version)
        for relative, f in info.files:
            out = ctx.actions.declare_file(base + relative)
            ctx.actions.symlink(output = out, target_file = f)
            staged.append(out)
        metadata = ctx.actions.declare_file(base + "port.toml")
        ctx.actions.write(
            output = metadata,
            content = "name = %s\nversion = %s\nport_version = %s\ndigest = %s\n" % (
                _quoted(info.name),
                _quoted(info.version),
                info.port_version,
                _quoted(info.digest),
            ),
        )
        staged.append(metadata)
    return [DefaultInfo(files = depset(staged), runfiles = ctx.runfiles(files = staged))]

port_store = rule(
    implementation = _port_store_impl,
    doc = """Stages `ports` as a store, each at `<root>/<name>/<version>/<port_version>/`
with its metadata at its root, in a directory holding nothing else. The metadata
states the digest each port was published with; a test reads every port back
against it, so content that has drifted from it fails before it ships.""",
    attrs = {
        "ports": attr.label_list(
            doc = "Every port version Lyra ships; one published is never removed.",
            providers = [PortInfo],
        ),
        "root": attr.string(
            doc = "Where the store is staged.",
            mandatory = True,
        ),
    },
)
