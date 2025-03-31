.PHONY: help clean tangle scheme hy test lint format doc run-scheme run-hy install

# Default target is help
.DEFAULT_GOAL := help

# Directories
SCHEME_DIR := scheme
HY_DIR := hy
TEST_DIR := tests
ORG_FILES := $(wildcard *.org)
SCHEME_SOURCES := $(wildcard $(SCHEME_DIR)/**/*.scm)
HY_SOURCES := $(wildcard $(HY_DIR)/**/*.hy)

# Commands
GUILE := guile
HY := hy
EMACS := emacs -Q --batch
GUILD := guild
HY_FORMATTER := black
GUILEC := guild compile

# Color definitions
CYAN := \033[36m
GREEN := \033[32m
YELLOW := \033[33m
RESET := \033[0m

# Simple help command
help:  ## Show this help message
	@echo "$(GREEN)Functional Data Structures - Makefile Help$(RESET)"
	@echo "$(YELLOW)Usage:$(RESET) make [target]"
	@echo ""
	@echo "$(YELLOW)Available targets:$(RESET)"
	@echo "  $(CYAN)all$(RESET)             Tangle code, run tests, and lint"
	@echo "  $(CYAN)clean$(RESET)           Remove generated and temporary files"
	@echo "  $(CYAN)tangle$(RESET)          Tangle source code from org files"
	@echo "  $(CYAN)detangle$(RESET)        Update org files with changes from source (reverse tangle)"
	@echo "  $(CYAN)compile-scheme$(RESET)  Compile Scheme files"
	@echo "  $(CYAN)install$(RESET)         Install Python dependencies"
	@echo "  $(CYAN)shell$(RESET)           Activate Poetry shell"
	@echo "  $(CYAN)test$(RESET)            Run all tests"
	@echo "  $(CYAN)test-scheme$(RESET)     Run Scheme tests"
	@echo "  $(CYAN)test-hy$(RESET)         Run Hy tests"
	@echo "  $(CYAN)lint$(RESET)            Run linters for both Scheme and Hy"
	@echo "  $(CYAN)format$(RESET)          Format both Scheme and Hy code"
	@echo "  $(CYAN)doc$(RESET)             Generate documentation"
	@echo "  $(CYAN)run-scheme$(RESET)      Run Scheme implementation"
	@echo "  $(CYAN)run-hy$(RESET)          Run Hy implementation"
	@echo "  $(CYAN)get-paper$(RESET)       Download Okasaki's thesis paper"

all: tangle test lint  ## Tangle code, run tests, and lint

clean:  ## Remove generated and temporary files
	@echo "$(CYAN)Cleaning generated files...$(RESET)"
	@if [ -f scheme/.generated ]; then \
		echo "Removing generated Scheme files..."; \
		rm -rf $(SCHEME_DIR); \
	else \
		echo "Cleaning Scheme compiled files..."; \
		rm -rf $(SCHEME_DIR)/**/*.go; \
	fi
	@if [ -f hy/.generated ]; then \
		echo "Removing generated Hy files..."; \
		rm -rf $(HY_DIR); \
	else \
		echo "Cleaning Hy cache files..."; \
		rm -rf $(HY_DIR)/**/__pycache__; \
	fi
	@rm -rf __pycache__
	@rm -rf .pytest_cache
	@rm -rf dist
	@rm -rf build
	@rm -rf *.egg-info
	@find . -name "*~" -delete
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -delete
	@echo "$(GREEN)Done cleaning$(RESET)"

# Code generation
tangle:  ## Tangle source code from org files
	@echo "$(CYAN)Tangling Org files...$(RESET)"
	@for file in $(ORG_FILES); do \
		echo "Tangling $$file..."; \
		$(EMACS) --eval "(require 'org)" --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo "$(GREEN)Done tangling files to scheme/ and hy/ (marked as generated)$(RESET)"
	@touch scheme/.generated
	@touch hy/.generated

detangle:  ## Update org files from source code (reverse of tangle)
	@echo "$(CYAN)Detangling source files...$(RESET)"
	@for file in $(ORG_FILES); do \
		echo "Processing $$file..."; \
		$(EMACS) --eval "(require 'org)" --eval "(org-babel-detangle \"$$file\")"; \
	done
	@echo "$(GREEN)Done detangling$(RESET)"

# Build targets
compile-scheme: tangle  ## Compile Scheme files
	@echo "Compiling Scheme files..."
	@for file in $(SCHEME_SOURCES); do \
		echo "Compiling $$file..."; \
		$(GUILEC) -o $$file.go $$file; \
	done
	@echo "Done compiling Scheme files"

install:  ## Install Python dependencies and activate environment
	@echo "Installing Python dependencies..."
	@poetry install --with dev
	@echo "Done installing dependencies"

shell:  ## Activate Poetry shell
	@echo "Activating Poetry shell..."
	@poetry shell

# Test targets
test: test-scheme test-hy  ## Run all tests

test-scheme: tangle  ## Run Scheme tests
	@echo "$(CYAN)Running Scheme tests...$(RESET)"
	@mkdir -p $(SCHEME_DIR)/okasaki
	@for file in $(SCHEME_DIR)/*.scm; do \
		module_name=$$(basename $$file .scm); \
		echo "Extracting $$module_name module..."; \
		$(GUILE) -L $(SCHEME_DIR) -c "(use-modules (ice-9 regex)) \
                  (let* ((content (call-with-input-file \"$$file\" get-string-all)) \
                         (pattern (make-regexp \"\\(define-module \\(okasaki $$module_name\\)[^)]*\\)([^)]*\\))\" #:extended? #t)) \
                         (match (string-match pattern content) \
                           (#f (display \"Module not found\\n\")) \
                           (m (call-with-output-file \"$(SCHEME_DIR)/okasaki/$$module_name.scm\" \
                                (lambda (port) (display (match:substring m) port) \
                                               (newline port) \
                                               (display (substring content (match:end m)) port))))))"; \
	done
	@$(GUILE) -L . -e main $(TEST_DIR)/scheme/test-runner.scm || (echo "$(RED)Scheme tests failed$(RESET)"; exit 1)
	@echo "$(GREEN)Scheme tests passed$(RESET)"

test-hy: tangle  ## Run Hy tests
	@echo "$(CYAN)Running Hy tests...$(RESET)"
	@mkdir -p $(HY_DIR)/okasaki
	@touch $(HY_DIR)/okasaki/__init__.hy
	@for file in $(HY_DIR)/*.hy; do \
		module_name=$$(basename $$file .hy); \
		echo "Extracting $$module_name module..."; \
		grep -A 1000 "$$module_name.hy" $$file > $(HY_DIR)/okasaki/$$module_name.hy; \
	done
	@poetry run pytest $(TEST_DIR)/hy || (echo "$(RED)Hy tests failed$(RESET)"; exit 1)
	@echo "$(GREEN)Hy tests passed$(RESET)"

# Lint and format
lint: lint-scheme lint-hy  ## Run linters for both Scheme and Hy

lint-scheme:  ## Lint Scheme code
	@echo "Linting Scheme code..."
	@if command -v $(GUILD) >/dev/null 2>&1; then \
		$(GUILD) lint $(SCHEME_SOURCES) || (echo "Scheme linting failed"; exit 1); \
	else \
		echo "Guild lint not available. Skipping Scheme linting."; \
	fi
	@echo "Scheme linting passed"

lint-hy:  ## Lint Hy code
	@echo "Linting Hy code..."
	@poetry run ruff check $(HY_DIR) || (echo "Hy linting failed"; exit 1)
	@echo "Hy linting passed"

format: format-scheme format-hy  ## Format both Scheme and Hy code

format-scheme:  ## Format Scheme code
	@echo "Formatting Scheme code..."
	@echo "Note: Automatic formatting for Scheme is not available."
	@echo "Please format Scheme code manually."

format-hy:  ## Format Hy code
	@echo "Formatting Hy code..."
	@$(HY_FORMATTER) $(HY_DIR)
	@echo "Hy code formatted"

# Documentation
doc:  ## Generate documentation
	@echo "Generating documentation..."
	@$(EMACS) --eval "(require 'org)" --eval "(require 'ox-html)" \
		--eval "(dolist (file '($(ORG_FILES))) (with-current-buffer (find-file file) (org-html-export-to-html)))"
	@echo "Documentation generated"

# Run targets
run-scheme: tangle  ## Run Scheme implementation
	@echo "Running Scheme implementation..."
	@$(GUILE) -L . $(SCHEME_DIR)/okasaki.scm

run-hy: tangle  ## Run Hy implementation
	@echo "Running Hy implementation..."
	@$(HY) $(HY_DIR)/okasaki.hy

# Git and contribution helpers
commit-conventional:  ## Create a conventional commit (usage: make commit-conventional msg="type(scope): message")
	@if [ -z "$(msg)" ]; then \
		echo "Error: Missing commit message."; \
		echo "Usage: make commit-conventional msg=\"type(scope): message\""; \
		echo "Example: make commit-conventional msg=\"feat(queue): add persistent queue implementation\""; \
		exit 1; \
	fi
	@echo "Creating conventional commit..."
	@git commit -m "$(msg)" --trailer "Signed-off-by: $$(git config user.name) <$$(git config user.email)>"
	@echo "Commit created"

get-paper:  ## Download Okasaki's thesis paper
	@echo "Downloading Okasaki's thesis paper..."
	@mkdir -p references
	@curl -L https://www.cs.cmu.edu/~rwh/students/okasaki.pdf -o references/okasaki-thesis.pdf
	@echo "$(GREEN)Paper downloaded to references/okasaki-thesis.pdf$(RESET)"