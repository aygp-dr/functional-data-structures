# Functional Data Structures Project Guide

## Build & Test Commands

### Quick Start
- **All tasks**: `make all` - Tangle code, run tests, and lint
- **Install dependencies**: `make install` - Install Python dependencies with Poetry
- **Activate shell**: `make shell` - Activate Poetry shell environment
- **Help**: `make help` - Show all available commands

### Code Generation
- **Tangle**: `make tangle` - Extract source code from org files
- **Detangle**: `make detangle` - Update org files with changes from source
- **Clean**: `make clean` - Remove generated and temporary files

### Running Code
- **Scheme**: 
  - Run implementation: `make run-scheme` or `guile -L . scheme/okasaki.scm`
  - Test individual module: `guile -L . -c "(use-modules (okasaki MODULE)) (test-FUNCTION)"`
  - REPL: `guile -L .` then `(use-modules (okasaki MODULE))`
  - Compile: `make compile-scheme`
- **Hy**:
  - Run implementation: `make run-hy` or `hy hy/okasaki.hy` 
  - Test individual module: `hy -c "from okasaki.MODULE import *; test_FUNCTION()"`
  - REPL: `hy` then `(import okasaki.MODULE)`

### Testing
- **All tests**: `make test`
- **Scheme tests**: `make test-scheme`
- **Hy tests**: `make test-hy`

### Code Quality
- **Lint all**: `make lint`
- **Lint Scheme**: `make lint-scheme`
- **Lint Hy**: `make lint-hy`
- **Format all**: `make format`
- **Format Scheme**: `make format-scheme` (manual only)
- **Format Hy**: `make format-hy`

### Documentation & Resources
- **Generate docs**: `make doc` - Generate HTML from org files
- **Get paper**: `make get-paper` - Download Okasaki's thesis paper

### Git Workflow
- **Conventional commit**: `make commit-conventional msg="type(scope): message"`
  - Example: `make commit-conventional msg="feat(queue): add persistent queue implementation"`

## Code Style Guidelines
- **Formatting**: 2-space indentation for all code blocks
- **Naming**: Use hyphenated-names in Scheme, underscored_names in Hy
- **Functions**:
  - Pure functions only - no side effects
  - Follow persistent data structure principles - never modify in place
- **Error handling**: Use condition systems in Scheme, exceptions in Hy
- **Documentation**: Add docstrings to all functions
- **Language specifics**:
  - **Scheme**: Use `define-module` and `#:export` for module definitions
  - **Hy**: Use `defn` for functions, Python-compatible data structures
  
Follow the existing patterns in the code. All data structures should be purely functional with no mutation of existing structures.