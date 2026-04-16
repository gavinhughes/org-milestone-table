# CLAUDE.md — Project conventions for org-milestone-table

## What this is

An Emacs Lisp package providing in-buffer milestone scheduling for Org mode.
Users define milestones with IDs and predecessor relationships in Org tables,
then compute dates automatically.

## Build & test

```bash
make test           # Run the ERT test suite (27 tests)
make byte-compile   # Byte-compile with warnings-as-errors
make lint           # Run package-lint (skips gracefully if not installed)
make clean          # Remove .elc files
```

Always run `make test && make byte-compile` before considering any change done.

After committing, update `CHANGELOG.org` with a dated entry describing the change.

## Project structure

```
org-milestone-table.el       # All source code (single file package)
org-milestone-table-test.el  # All tests (ERT)
Makefile                     # Build automation
recipes/org-milestone-table  # MELPA recipe
README.org                   # User-facing docs
CHANGELOG.org                # Version history
BACKLOG.org                  # Future feature ideas
EXAMPLE.org                  # Real-world usage example
```

## Coding conventions

- **Naming:** Public functions use `org-milestone-table-` prefix.
  Private helpers use `omt--` prefix.
- **Lexical binding:** Always. Every .el file starts with
  `;;; filename --- description -*- lexical-binding: t; -*-`
- **Elisp style:** Use `cl-lib` macros (cl-loop, cl-block, cl-return-from)
  where they improve clarity. Prefer built-in calendar and org-table APIs.
- **Date format:** ISO 8601 `YYYY-MM-DD` throughout. Internally represented
  as Emacs gregorian triples `(month day year)`.
- **No external dependencies** beyond `emacs 28.1+` and `org 9.5+`.
  Everything else (`calendar`, `cl-lib`, `org-table`) ships with Emacs.

## Test conventions

- Test names: `omt-test-<function-or-feature>` (e.g., `omt-test-parse-date-valid`)
- Use the `omt-test-with-table` macro to set up temp Org buffers with table content
- Integration tests go in the "Integration" section of the test file
- Every public command should have at least one integration test

## Package header requirements (for MELPA)

The file header in `org-milestone-table.el` must include:
- `;;; filename --- description -*- lexical-binding: t; -*-`
- `Author:`, `Version:`, `Package-Requires:`, `Homepage:`, `Keywords:`
- `;;; Commentary:` section with usage overview
- `;;; Code:` section before any code
- `(provide 'org-milestone-table)` and `;;; filename ends here` at the end

## Developer environment

The author uses Doom Emacs. When suggesting Emacs configuration snippets,
use Doom conventions:
- `package!` in `~/.doom.d/packages.el` for package declarations
- `use-package!` in `~/.doom.d/config.el` for configuration
- `doom sync` to apply package changes (not `package-install` or similar)
- Do not suggest manual `load-path` manipulation — Doom manages this

## Org file formatting

When writing or editing .org files in this project, avoid using all caps for
headings and emphasis.

## CI

GitHub Actions runs `make byte-compile` and `make test` against Emacs
28.2, 29.4, and 30.1 on every push to master and every PR.

