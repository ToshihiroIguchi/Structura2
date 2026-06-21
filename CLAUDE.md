# Structura2 - Claude-Specific Guidelines

> **Read [AGENT.md](./AGENT.md) first.** This file supplements AGENT.md with Claude-specific instructions.

## Language Rules

- **All direct communication with the USER must be in Japanese.**
- **All code, comments, commit messages, documentation, and file contents must be written in English.**

## Code Style

- Use consistent R coding style: `snake_case` for variable and function names.
- Preserve all existing comments and docstrings unless they are directly contradicted by a code change.
- When modifying `app.R`, maintain the existing section structure (marked with `# ---- Section Name ----` comments).
- Always refer to the application name as "Structura2" (do not use "Structura").

## Error Handling

- Always wrap user-facing operations in `tryCatch()`.
- Never let an unhandled error crash the Shiny app.
- Provide actionable, user-friendly error messages that suggest specific fixes.

## Testing

- Before making changes to core functionality, verify that the current app runs without errors using `shiny::runApp(".")`.
- After making changes, test with edge-case data (empty CSV, constant columns, perfect multicollinearity).

## WebR Awareness

- Do not introduce dependencies on packages that are not available as WASM binaries.
- Do not use `Sys.setlocale()` or other system-level calls that fail in the WebR sandbox.
- Check `repo.r-wasm.org` or R-universe before adding new package dependencies.

## Approach

- When asked to modify the codebase, research the existing code thoroughly before proposing changes.
- Prefer minimal, targeted edits over wholesale rewrites.
- When encountering a WebR compatibility issue, first create a minimal test to verify the problem before committing to a workaround.
