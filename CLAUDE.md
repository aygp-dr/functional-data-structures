# Functional Data Structures Project Guide

## Build & Test Commands
- **Scheme**: 
  - Run code: `guile -L . scheme/okasaki.scm`
  - Test individual module: `guile -L . -c "(use-modules (okasaki MODULE)) (test-FUNCTION)"`
  - REPL: `guile -L .` then `(use-modules (okasaki MODULE))`
- **Hy**:
  - Run code: `hy hy/okasaki.hy` 
  - Test individual module: `hy -c "from okasaki.MODULE import *; test_FUNCTION()"`
  - REPL: `hy` then `(import okasaki.MODULE)`
- **Org-mode**: 
  - Tangle code: `emacs -Q --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"okasaki-data-structures.org\")"`

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