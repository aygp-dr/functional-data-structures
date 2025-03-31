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
	@echo "Cleaning generated files..."
	@rm -rf $(SCHEME_DIR)/**/*.go
	@rm -rf __pycache__
	@rm -rf $(HY_DIR)/**/__pycache__
	@rm -rf .pytest_cache
	@rm -rf dist
	@rm -rf build
	@rm -rf *.egg-info
	@find . -name "*~" -delete
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -delete
	@echo "Done cleaning"

# Code generation
tangle:  ## Tangle source code from org files
	@echo "Tangling Org files..."
	@for file in $(ORG_FILES); do \
		echo "Tangling $$file..."; \
		$(EMACS) --eval "(require 'org)" --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo "Done tangling"

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
	@echo "Running Scheme tests..."
	@$(GUILE) -L . -e main $(TEST_DIR)/scheme/test-runner.scm || (echo "Scheme tests failed"; exit 1)
	@echo "Scheme tests passed"

test-hy: tangle  ## Run Hy tests
	@echo "Running Hy tests..."
	@poetry run pytest $(TEST_DIR)/hy || (echo "Hy tests failed"; exit 1)
	@echo "Hy tests passed"

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