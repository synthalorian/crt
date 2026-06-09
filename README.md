# crt

> Static site generator that treats content as a signal pipeline — markdown in, processed through a functional DAG of transforms, HTML out.

**Language:** OCaml  
**Constraint:** Text is the universal interface  
**Stack:** dune, menhir, custom markdown parser, tyxml

---

## Features

- Markdown input with YAML frontmatter
- Functional DAG transform pipeline
- Each node: pure function (parse → highlight → toc → linkcheck → minify)
- Hot reload via file watcher
- Incremental builds (only changed nodes)
- Plugin system (OCaml modules)
- Theme engine with template inheritance

---

## Development Plan

- [x] Phase 1: Markdown parser (menhir grammar)
- [x] Phase 2: DAG pipeline engine (functional composition)
- [ ] Phase 3: Core transforms: parse, highlight, toc
- [ ] Phase 4: HTML generation (tyxml)
- [ ] Phase 5: File watcher + hot reload
- [x] Phase 6: Incremental build cache
- [x] Phase 7: Plugin system (dynamic module loading)
- [x] Phase 8: Theme engine and template inheritance

---

## Getting Started

### Prerequisites

- OCaml toolchain

### Build

```bash
dune build
```

### Test

```bash
dune test
```

### Run

```bash
dune exec crt
```

---

## Architecture

See `PLAN.md` for detailed architecture decisions and implementation notes.

---

## License

MIT
