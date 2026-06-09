# crt — Implementation Plan

## Project Overview

Static site generator that treats content as a signal pipeline — markdown in, processed through a functional DAG of transforms, HTML out.

**Language:** OCaml  
**Constraint:** Text is the universal interface  
**Stack:** dune, menhir, omd/markdown parser, tyxml

---

## Phase Breakdown

### Phase 1: Markdown parser (menhir grammar)

**Goal:** Phase 1: Markdown parser (menhir grammar)

**Deliverables:**
- [x] Core implementation
- [x] Tests
- [x] Documentation update

**Notes:**
- Implemented a menhir/ocamllex parser in `lib/parser.mly`, `lib/lexer.mll`, `lib/markdown.ml`, backed by `lib/ast.ml`.
- Supported block elements: paragraphs, ATX headings (# ## ###), fenced code blocks, thematic breaks, blockquotes, unordered/ordered lists.
- Supported inline elements: text, bold (`**` / `__`), italic (`*` / `_`), inline code (`` ` ``), links (`[text](url)`), hard breaks.
- Added 16 parser unit tests in `test/test_parser.ml` covering all supported block and inline forms.
- Updated `dune-project` to use OCaml 5.2+ (required for GCC 16 compatibility on the build host).

---

### Phase 2: DAG pipeline engine (functional composition)

**Goal:** Phase 2: DAG pipeline engine (functional composition)

**Deliverables:**
- [x] Core implementation
- [x] Tests
- [x] Documentation update

**Notes:**
- Implemented a GADT-based DAG pipeline engine in `lib/pipeline.ml`/`lib/pipeline.mli`.
- Supports pure values, function application (`arr`/`map`), monadic bind for sequencing, and `pair` for parallel composition (fork/join).
- Named nodes via `node` for observability/introspection.
- Error handling via `run` which catches exceptions and returns `('a, exn) result`.
- Added `trace` for debugging side effects.
- Added 28 pipeline unit tests in `test/test_pipeline.ml` covering pure, map, bind, pair, complex DAGs, error handling, and AST integration.
- All 44 tests pass (16 parser + 28 pipeline).

---

### Phase 3: Core transforms: parse, highlight, toc

**Goal:** Phase 3: Core transforms: parse, highlight, toc

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

### Phase 4: HTML generation (tyxml)

**Goal:** Phase 4: HTML generation (tyxml)

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

### Phase 5: File watcher + hot reload

**Goal:** Phase 5: File watcher + hot reload

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

### Phase 6: Incremental build cache

**Goal:** Phase 6: Incremental build cache

**Deliverables:**
- [x] Core implementation
- [x] Tests
- [x] Documentation update

**Notes:**
- Implemented `Cache` module in `lib/cache.ml` with disk-based persistence, mtime tracking, and automatic invalidation.
- Added `Cached` constructor to pipeline GADT in `lib/pipeline.ml`.
- Added `Pipeline.cached` combinator for wrapping subtrees with caching.
- Added `Pipeline.run_cached` for cache-aware pipeline execution that stores and reuses intermediate results.
- Integrated cache into `Build.build_file` — skips processing when input file mtime matches cached entry.
- Added 13 cache unit tests in `test/test_cache.ml` covering create, get/set, mtime invalidation, missing keys, invalidate, clear, is_valid, and pipeline caching.
- All existing tests continue to pass. 

---

### Phase 7: Plugin system (dynamic module loading)

**Goal:** Phase 7: Plugin system (dynamic module loading)

**Deliverables:**
- [x] Core implementation
- [x] Tests
- [x] Documentation update

**Notes:**
- Implemented `Plugin` module in `lib/plugin.ml`/`lib/plugin.mli` with dynamic module loading via OCaml's `Dynlink`.
- Plugin interface supports:
  - **Transforms**: Named `Ast.doc -> Ast.doc` functions applied during build
  - **Doc hooks**: Stage-specific hooks (`Post_parse`, `Pre_render`) for AST manipulation
  - **HTML hooks**: Post-render string transforms for HTML output modification
- Global plugin registry with `register`/`clear_registry`/`registry` functions.
- Dynamic loading: `load_cmxs` for single `.cmxs` files, `load_dir` for scanning plugin directories.
- Integrated into `Build.build_file` pipeline: Post_parse hooks run after parsing, plugin transforms run after built-in transforms, Pre_render hooks run before rendering, HTML hooks run after HTML generation.
- Added `?plugin_dir` parameter to `build`, `build_with_watch`, and `serve` functions.
- Added 34 plugin unit tests in `test/test_plugin.ml` covering creation, registration, hooks, transforms, HTML hooks, multiple plugins, error handling, and integration.
- All 137 tests pass (16 parser + 28 pipeline + 16 transform + 15 renderer/build + 34 plugin + 20 cache + 8 watcher).

---

### Phase 8: Theme engine and template inheritance

**Goal:** Phase 8: Theme engine and template inheritance

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

## Architecture Notes

### Key Decisions

- 

### Data Flow

```
[Input] → [Parse] → [Transform] → [Output]
```

### Error Handling Strategy

- 

---

## Testing Strategy

- Unit tests for core functions
- Integration tests for full pipeline
- Benchmarks for performance-critical paths

---

## Open Questions

1. 
2. 

---

*Generated for opencode sprint. Implement phase by phase. DO NOT RESEARCH. Build directly.*
