# Repository Guidelines

## Purpose and Sources of Truth

LispBSD is a personal, live, inspectable Common Lisp operating environment
built above the NetBSD kernel and drivers: a modern Lisp machine. This file
contains its enduring architectural and repository policy. The tracked
`lisp-machine-spec.org` is the normative design specification and outranks
this file wherever they conflict. `docs/architecture.org` maps runtime and
source boundaries and `docs/guide.org` documents user-visible behavior once
they appear; tracked source plus behavioral tests are executable truth. Do
not create another omnibus product specification.

Decisions already fixed above the specification's open choices:

- SBCL is the Common Lisp implementation of this project. The specification
  permits alternative runtime substrates; this project fixes SBCL and does
  not carry an abstraction burden for hypothetical alternatives except where
  the specification demands narrow interfaces around implementation-specific
  behavior such as image saving, thread control, stack inspection, FFI, and
  debugger integration.
- The NetBSD kernel and drivers provide hardware enablement, virtual memory,
  scheduling primitives, filesystems, networking, and device drivers. The
  project does not write a kernel from scratch and does not port Unix
  ontologies into the Lisp world; conventional concepts stop at explicit
  compatibility facilities exactly as the specification requires.
- Development happens on Linux x86-64 hosts. The operating-system side is
  built from a NetBSD source tree with its own `build.sh`, and every runnable
  artifact is exercised under QEMU virtual machines driven by checked-in
  scripts.
- The target platform is NetBSD/amd64.
- The canonical user interface is the black-and-white bitmap desktop inspired
  by Xerox PARC Interlisp-D and Medley, described by the specification. It is
  hosted on a 1-bit logical framebuffer with a preferred native NetBSD display
  and input backend, optionally presented through Wayland surfaces.
- The default branch is `master`.

Core principles:

- Treat the live Lisp world as the primary runtime and primary ontology while
  keeping source sufficient for a clean rebuild.
- Prefer Common Lisp and focused CLOS protocols over generated scripts,
  parallel type dispatch, duplicated data structures, or overlapping public
  interfaces.
- Keep durable world mutations explicit, auditable, reconstructible, and
  confined to a small operation set.
- Preserve saved worlds, checkpoint generations, and a separate recovery path
  that can boot without the active world.
- Use process boundaries for reliability and accidental-damage containment,
  never as a hostile-code security claim.
- Keep platform-specific and implementation-specific behavior behind narrow
  adapters.
- Treat migrations and compatibility readers as temporary release machinery.
  Give them an explicit removal boundary instead of accumulating them forever.

Do not leave TODOs, FIXMEs, stubs, placeholders, or knowingly partial
implementations. If a requirement is genuinely too broad or conflicts with
another requirement, stop and ask.

## Upstream References

When a change depends on established behavior in another codebase, inspect a
current upstream checkout outside this Git worktree. Relevant upstreams
include the NetBSD source tree, the Autolith repository, and Interlisp-D or
Medley materials. Keep reference checkouts read-only, record the repository
URL and exact inspected commit in review notes, and refresh the checkout
before making claims about current behavior. References are research inputs,
not dependencies; do not edit them or copy their architecture wholesale.

## Architectural Guardrails

- Keep the codebase small and prefer Common Lisp, ASDF, and UIOP for host-side
  filesystem, process, networking, and build work.
- Shell scripts, makefiles, and C sources appear where the NetBSD substrate
  genuinely requires them: `build.sh` integration, kernel configuration,
  userland glue, and boot media construction. Do not generate ad hoc Python
  or shell files for work that Common Lisp performs adequately.
- Keep the NetBSD kernel and driver substrate, the SBCL runtime substrate, the
  live Lisp world, the window system and UI backend, and any resident agent as
  distinct components with explicitly documented interfaces.
- All SBCL-specific operations run behind the runtime protocol described by
  the specification. World semantics must not depend on SBCL internals leaking
  across that boundary.
- Treat process separation as an accidental-damage and reliability boundary,
  never as a security sandbox.
- Keep credentials out of saved worlds, heap images, conversation records,
  disk images, logs, and Git.
- Source is authoritative for clean rebuilds. Saved worlds and checkpoint
  generations preserve exact working live states, but never replace tracked
  source.
- Operate on Lisp forms for durable source edits. Do not use blind regular
  expression replacement of source code.
- Generated build artifacts, disk images, kernel snapshots, and virtual machine
  state stay out of Git. Check in only the scripts, configurations, and
  documentation that reproduce them.

For a durable world mutation, preserve the specified order:

1. Journal the intended mutation.
2. Compile and install it in the live world.
3. Run relevant checks.
4. Publish the complete reconstructible state as an immutable generation,
   probe its replay procedure, and retain it in durable history.
5. Atomically select the generation and mark the journal entry durable.

Durable world changes are recorded as reconstructible definition changes, not
as opaque heap edits. Ordinary workspace tools may develop tracked source,
including boot, substrate, and recovery sources, exactly like any other
project.

Conversation records and mutation journals are append-only sequences of
top-level readable forms. Bind `*read-eval*` to `nil` when reading persisted
record data, keep that data portable, and tolerate an incomplete final form
after a crash. Publish checkpoints and manifests atomically. A separate
recovery world MUST be able to boot without the active world, network access,
an LLM provider, or any resident agent, and must remain able to restore or
reconstruct a known-good world from generations, source, and mutation history.

## Package Policy

Use one project package, `#:lispbsd`. Do not create scoped, subsystem,
feature, file-local, or test packages unless the user explicitly changes this
policy. Split the implementation into focused files while keeping those files
in the single project package. Runtime component boundaries are not package
boundaries.

- Define the package once and use `(in-package #:lispbsd)` in project source.
- `:use` only `#:cl`.
- Import individual third-party symbols with `:import-from`; do not wholesale
  `:use` third-party packages.
- Do not introduce packages merely to express internal architecture. Express
  those boundaries with files, functions, CLOS protocols, and clear naming.
- This policy applies to Common Lisp code. C sources follow NetBSD conventions
  for their layer instead.

## Code Organization

- Keep boot files limited to startup, shutdown, configuration, and component
  wiring. Put substantive behavior in focused implementation files.
- Split code by coherent responsibility. Do not create `misc`, `helpers`, or
  a growing multi-purpose `util` dumping ground.
- Keep utility files single-purpose.
- Preserve public entry points when splitting code, and move one coherent
  concern at a time.
- Group definitions by functionality, not alphabetically.
- Within a file, prefer this order where applicable: types and classes,
  generic functions, methods, public functions, private functions, and
  conditions.

Prefer simple, established, readable solutions. Keep business logic above
low-level mechanics. Prefer small, documented functions even when a helper is
used only once. Use CLOS when it provides a useful semantic protocol instead
of repeating type or state dispatch in `cond` trees.

## Common Lisp Style

### Naming

- Use kebab-case symbols without abbreviations.
- Do not use `defconstant` or `define-constant`.
- Use `defparameter` for reloadable policy and defaults.
- Use `defvar` only for state or identity intended to survive reload.
- Special variables use surrounding asterisks, for example `*active-world*`.
- Functions use clear, entity-prefixed names where an entity exists, for
  example `generation-find` and `world-append-journal-record`.
- Internal functions use a double hyphen after the entity or subsystem name,
  for example `generation--validate-manifest`.
- Predicates use a `-p` suffix.
- Conversion functions use `->`, for example `presentation->object`.
- Classes are singular lowercase names. Accessors are prefixed with the
  entity name.
- Functions and methods with four or more parameters use keyword arguments.
  Do not introduce new positional lambda lists with four or more parameters.
- Prefer `first` and `rest` over `car` and `cdr` in application code.
- Quote keywords used as data. Whenever a keyword value directly follows a
  keyword-argument name, in a call, an evaluated plist, or a `defclass`
  `:initform`, quote the value, for example `:status ':durable`. Never quote
  keywords in unevaluated syntax positions: `defclass` `:initarg` names,
  `case` clause keys, `member` type specifiers, quoted configuration data,
  and macro metadata the macro quotes itself.

### Types and Documentation

- Declare function types with the project's `->` notation, compatible with
  Serapeum's:

  ```lisp
  (-> generation-find (string) (option generation))
  ```

- Keep reusable custom types in one coherent location.
- Do not define weak aliases that merely expand to `list` or another generic
  type. Validate structured types with `(satisfies predicate)` or a stronger
  type expression.
- Give functions and macros documentation strings. Give classes, slots,
  generic functions, and conditions documentation in their supported
  documentation locations.
- Use this comment hierarchy:

  ```lisp
  ;;;; -- Major Section --
  ;;; Minor section
  ;; Regular comment
  ; Inline comment, rarely
  ```

### Formatting

Use two-space indentation and vertical alignment where related forms expose a
repeated structure. In particular, align class slot options, `let` bindings,
and keyword arguments in multi-line calls.

```lisp
(let* ((generation-path (generation-path generation))
       (manifest-path   (generation-manifest-path generation))
       (commit          (generation-commit generation)))
  (generation-validate :generation-path generation-path
                       :manifest-path   manifest-path
                       :commit          commit))
```

- Put one blank line between definitions and two blank lines between major
  sections.
- Leave no trailing whitespace.
- Put a conditional clause body on the line after its test, even for `nil`.
- In `labels`, leave a blank line between local function definitions.
- A literal percent sign in a `format` control string is `%`, not `%%`.
  Common Lisp `format` directives begin with `~`.

### Conditions, Restarts, and Returns

- Define domain-specific conditions with structured data, documentation, and
  helpful report functions. Do not use raw error strings where callers need
  to distinguish or recover from failures.
- Use `handler-case` for expected failures and establish useful restarts where
  practical.
- Keep the condition and restart model explicit at component boundaries,
  especially in journaling, provider, checkpoint, and recovery code.
- Guard agent-assisted condition handling against recursion. A serious failure
  while that path is already handling a condition is fatal.
- Use `(block nil ... (return ...))` for clear early returns.
- Boolean functions return exactly `t` or `nil`.
- Use explicit `values` forms for intentional multiple-value returns and
  document those values.

### Macros and Dependencies

- Prefer functions to macros when functions suffice.
- Name context and resource macros with a `with-` prefix.
- Prevent variable capture with gensyms or deliberate block names, and
  document expansion and evaluation behavior.
- Anaphoric forms may be used when they materially improve readability. Do
  not add an anaphora dependency without asking if the project does not
  already use one.

Pin third-party dependencies so builds are reproducible. Record new Quicklisp
or vendored dependencies in the build documentation when introduced.

## Quality and Testing

- Implement complete behavior, including the difficult failure paths. Do not
  substitute a cheap approximation for a specified invariant.
- Add tests for behavior, state transitions, persistence, recovery,
  serialization, and external boundaries. Avoid tests whose main value is
  pinning formatting, incidental source shape, or a private implementation
  detail.
- Prefer table-driven cases for families of literal inputs and expected
  outputs.
- Test expected failures as well as successful paths.
- Run focused checks while developing, then every repository-wide check that
  exists after every change, including documentation and configuration
  changes, and before committing.
- Do not invent a test command. When the project gains test, lint, build, or
  virtual-machine entry points, document the exact commands here and keep
  them current.
- Never commit with known failing relevant checks. If an environmental failure
  prevents a check, report the exact failure rather than claiming success.
- Operating-system changes are verified by building the affected NetBSD
  artifact and booting it under QEMU before the change is called done.

Run the hosted Lisp test suite from a REPL that can see this repository:

```lisp
(asdf:load-asd (merge-pathnames "lispbsd.asd" (uiop:getcwd)))
(asdf:test-system :lispbsd)
```

Cross-build the NetBSD/amd64 tools and GENERIC kernel:

```sh
script/netbsd-build tools kernel=GENERIC
```

Build the LispBSD serial/Multiboot kernel:

```sh
script/netbsd-build kernel=LISPBSD
```

Boot a raw disk image under QEMU with the serial console on stdin/stdout:

```sh
script/vm-run path/to/disk.img
```

Boot that image with the self-built LISPBSD kernel:

```sh
KERNEL=/root/common-lisp/refs/netbsd-obj/sys/arch/amd64/compile/LISPBSD/netbsd \
  script/vm-run path/to/disk.img
```

Drive a command or Lisp form over the guest serial console:

```sh
script/guest uname -a
script/guest '(lisp-implementation-version)'
script/guest --send netbsd/console/install.lisp
```

`script/guest` logs in as root. After login the root profile execs SBCL,
so a form is evaluated in the live listener. Attach a FAT image of pkgsrc
packages with `PKG_DISK=path/to/fat.img` when installing software into the
guest.

Export a host directory into the guest over virtio-9p:

```sh
VIRTFS=/root VIRTFS_TAG=host MEMORY=4096 script/guest '(lispbsd:world-phase lispbsd:*world*)'
```

The guest kernel exposes that export as `vio9p0`. Create `/dev/vio9p0` with
`mknod /dev/vio9p0 c 356 0` and mount it with `mount_9p -cu /dev/vio9p0 /host`.

For a fast delimiter check before loading edited Lisp, use the built-in
`lisp.paren-check` operation on each relevant source tree:

```lisp
(lisp.paren-check :path "src")
(lisp.paren-check :path "tests")
```

## Commit Policy

- Use primitive-style, imperative commit messages with a title line only,
  shorter than 72 characters.
- Do not add `Co-Authored-By` or other attribution trailers.
- Keep commits tiny, granular, single-purpose, and independently reviewable.
- Prefer one commit per regression fix, feature slice, or coherent structural
  change. Never put more than one issue in a commit.
- For broad work, make a sequence of small vertical commits instead of one
  final catch-all commit.
- Do not bundle unrelated cleanup, refactoring, or formatting with a behavior
  change.
- Rebase instead of merging and avoid merge commits.
- Run the relevant checks before each commit.
- Commit each completed change after its checks pass, and push to `origin`
  `master` after every commit without exception.
- Do not rewrite, discard, or include unrelated user changes. Inspect the
  worktree and index before committing, and stage only files belonging to
  the task.

Never use em dashes in source comments, documentation, commit messages, or
user-facing text.
