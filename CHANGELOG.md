# Changelog

## 1.0.0 — 2026-06-10

### Added

- **Phase 1: Markdown parser** — Menhir/ocamllex parser supporting paragraphs, ATX headings, fenced code blocks, thematic breaks, blockquotes, lists, bold, italic, inline code, links, and hard breaks.
- **Phase 2: DAG pipeline engine** — GADT-based functional pipeline with pure values, `arr`/`map`, monadic `bind`, `pair` for parallel composition, named nodes, and error handling.
- **Phase 3: Core transforms** — Parse, syntax highlight, and table-of-contents transforms with pipeline integration.
- **Phase 4: HTML generation** — Tyxml-based renderer producing valid HTML5 output.
- **Phase 5: File watcher + hot reload** — File-system watcher for incremental rebuilds during development.
- **Phase 6: Incremental build cache** — Disk-based cache with mtime tracking and automatic invalidation.
- **Phase 7: Plugin system** — Dynamic module loading via `Dynlink` with transforms, doc hooks (`Post_parse`, `Pre_render`), and HTML hooks.
- **Phase 8: Theme engine** — Template inheritance, partials, conditionals, for loops, and static asset copying.

### Tests

- 176 unit tests across 8 test suites (parser, pipeline, transform, renderer/build, watcher, cache, plugin, theme).
