# crt

> Static site generator that treats content as a signal pipeline — markdown in, processed through a functional DAG of transforms, HTML out.

**Language:** OCaml  
**Constraint:** Text is the universal interface  
**Stack:** dune, menhir, omd/markdown parser, tyxml

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

1. Phase 1: Markdown parser (menhir grammar)
2. Phase 2: DAG pipeline engine (functional composition)
3. Phase 3: Core transforms: parse, highlight, toc
4. Phase 4: HTML generation (tyxml)
5. Phase 5: File watcher + hot reload
6. Phase 6: Incremental build cache
7. Phase 7: Plugin system (dynamic module loading)
8. Phase 8: Theme engine and template inheritance

---

## Getting Started

### Prerequisites

- OCaml toolchain

### Build

```bash
# See PLAN.md for detailed build instructions per phase
cd crt
```

### Run

```bash
# See PLAN.md for run instructions
```

---

## Architecture

See `PLAN.md` for detailed architecture decisions and implementation notes.

---

## License

MIT
