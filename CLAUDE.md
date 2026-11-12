# Structura2 - Claude-Specific Guidelines

@AGENT.md

The shared rules (language, code style, error handling, testing, WebR constraints) live in AGENT.md and are
imported above. This file only adds Claude-specific instructions.

## Approach

- When asked to modify the codebase, research the existing code thoroughly before proposing changes.
- Prefer minimal, targeted edits over wholesale rewrites.
- When encountering a WebR compatibility issue, first create a minimal test to verify the problem before committing to a workaround.
