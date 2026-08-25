# LispBSD

A personal, live, inspectable Common Lisp operating environment built above the
NetBSD kernel and drivers: a modern Lisp machine.

- `lisp-machine-spec.org` is the normative design specification.
- `AGENTS.md` contains enduring architectural and repository policy.

Development happens on Linux x86-64 hosts. The operating-system side is built
from a NetBSD source tree with its own `build.sh`, and every runnable artifact
is exercised under QEMU virtual machines driven by checked-in scripts. The
target platform is NetBSD/amd64, the Common Lisp implementation is SBCL, and
the canonical UI direction is the black-and-white bitmap desktop inspired by
Xerox PARC Interlisp-D and Medley.

## Status

The project is at the very beginning; see the implementation sequence in
`lisp-machine-spec.org` for the staged plan and the workspace agenda for what
is currently in flight.
