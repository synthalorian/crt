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
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

### Phase 2: DAG pipeline engine (functional composition)

**Goal:** Phase 2: DAG pipeline engine (functional composition)

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

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
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

---

### Phase 7: Plugin system (dynamic module loading)

**Goal:** Phase 7: Plugin system (dynamic module loading)

**Deliverables:**
- [ ] Core implementation
- [ ] Tests
- [ ] Documentation update

**Notes:**
- 

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
