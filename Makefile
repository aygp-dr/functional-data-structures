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
HY := poetry run hy
EMACS := emacs -Q --batch
GUILD := guild
HY_FORMATTER := poetry run black
GUILEC := guild compile

# Colors for terminal output
RESET := \033[0m
GREEN := \033[32m
YELLOW := \033[33m
BLUE := \033[34m
RED := \033[31m

## Main targets

help: ## Show this help message
	@echo "$(GREEN)Functional Data Structures - Makefile Help$(RESET)"
	@echo "$(YELLOW)Usage:$(RESET) make [target]"
	@echo ""
	@echo "$(YELLOW)Available targets:$(RESET)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(GREEN)%-15s$(RESET) %s\n", $$1, $$2}'

all: tangle test lint ## Tangle code, run tests, and lint

clean: ## Remove generated and temporary files
	@echo "$(BLUE)Cleaning generated files...$(RESET)"
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
	@echo "$(GREEN)Done cleaning$(RESET)"

## Code generation

tangle: ## Tangle source code from org files
	@echo "$(BLUE)Tangling Org files...$(RESET)"
	@for file in $(ORG_FILES); do \
		echo "Tangling $$file..."; \
		$(EMACS) --eval "(require 'org)" --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo "$(GREEN)Done tangling$(RESET)"

## Build targets

compile-scheme: tangle ## Compile Scheme files
	@echo "$(BLUE)Compiling Scheme files...$(RESET)"
	@for file in $(SCHEME_SOURCES); do \
		echo "Compiling $$file..."; \
		$(GUILEC) -o $$file.go $$file; \
	done
	@echo "$(GREEN)Done compiling Scheme files$(RESET)"

install: ## Install Python dependencies
	@echo "$(BLUE)Installing Python dependencies...$(RESET)"
	@poetry install
	@echo "$(GREEN)Done installing dependencies$(RESET)"

## Test targets

test: test-scheme test-hy ## Run all tests

test-scheme: tangle ## Run Scheme tests
	@echo "$(BLUE)Running Scheme tests...$(RESET)"
	@$(GUILE) -L . -e main $(TEST_DIR)/scheme/test-runner.scm || (echo "$(RED)Scheme tests failed$(RESET)"; exit 1)
	@echo "$(GREEN)Scheme tests passed$(RESET)"

test-hy: tangle ## Run Hy tests
	@echo "$(BLUE)Running Hy tests...$(RESET)"
	@poetry run pytest $(TEST_DIR)/hy || (echo "$(RED)Hy tests failed$(RESET)"; exit 1)
	@echo "$(GREEN)Hy tests passed$(RESET)"

## Lint and format

lint: lint-scheme lint-hy ## Run linters for both Scheme and Hy

lint-scheme: ## Lint Scheme code
	@echo "$(BLUE)Linting Scheme code...$(RESET)"
	@$(GUILD) lint $(SCHEME_SOURCES) || (echo "$(RED)Scheme linting failed$(RESET)"; exit 1)
	@echo "$(GREEN)Scheme linting passed$(RESET)"

lint-hy: ## Lint Hy code
	@echo "$(BLUE)Linting Hy code...$(RESET)"
	@poetry run ruff check $(HY_DIR) || (echo "$(RED)Hy linting failed$(RESET)"; exit 1)
	@echo "$(GREEN)Hy linting passed$(RESET)"

format: format-scheme format-hy ## Format both Scheme and Hy code

format-scheme: ## Format Scheme code
	@echo "$(BLUE)Formatting Scheme code...$(RESET)"
	@echo "$(YELLOW)Note: Automatic formatting for Scheme is not available.$(RESET)"
	@echo "$(YELLOW)Please format Scheme code manually.$(RESET)"

format-hy: ## Format Hy code
	@echo "$(BLUE)Formatting Hy code...$(RESET)"
	@$(HY_FORMATTER) $(HY_DIR)
	@echo "$(GREEN)Hy code formatted$(RESET)"

## Documentation

doc: ## Generate documentation
	@echo "$(BLUE)Generating documentation...$(RESET)"
	@$(EMACS) --eval "(require 'org)" --eval "(require 'ox-html)" \
		--eval "(dolist (file '($(ORG_FILES))) (with-current-buffer (find-file file) (org-html-export-to-html)))"
	@echo "$(GREEN)Documentation generated$(RESET)"

## Run targets

run-scheme: tangle ## Run Scheme implementation
	@echo "$(BLUE)Running Scheme implementation...$(RESET)"
	@$(GUILE) -L . $(SCHEME_DIR)/okasaki.scm

run-hy: tangle ## Run Hy implementation
	@echo "$(BLUE)Running Hy implementation...$(RESET)"
	@$(HY) $(HY_DIR)/okasaki.hy

## Git and contribution helpers

commit-conventional: ## Create a conventional commit (usage: make commit-conventional msg="type(scope): message")
	@if [ -z "$(msg)" ]; then \
		echo "$(RED)Error: Missing commit message.$(RESET)"; \
		echo "$(YELLOW)Usage: make commit-conventional msg=\"type(scope): message\"$(RESET)"; \
		echo "$(YELLOW)Example: make commit-conventional msg=\"feat(queue): add persistent queue implementation\"$(RESET)"; \
		exit 1; \
	fi
	@echo "$(BLUE)Creating conventional commit...$(RESET)"
	@git commit -m "$(msg)" --trailer "Signed-off-by: $$(git config user.name) <$$(git config user.email)>"
	@echo "$(GREEN)Commit created$(RESET)"

prepare-commit-msg: ## Setup git hooks for conventional commits
	@echo "$(BLUE)Setting up git commit message template...$(RESET)"
	@mkdir -p .git/hooks
	@cat > .git/hooks/prepare-commit-msg << 'EOF'
#!/bin/sh
#
# Git commit message template hook
# This hook prepares a template for the commit message

# Exit if this is not a new commit or if the message is already specified
case "$2,$3" in
  merge,|template,|squash,|commit,*|message,*)
    exit 0 ;;
esac

# Template for conventional commits
cat > "$1" << 'EOT'
# <type>(<scope>): <subject>
# |<---- Using a Maximum Of 50 Characters ---->|

# Explain why this change is being made
# |<---- Try To Limit Each Line to a Maximum Of 72 Characters ---->|

# --- COMMIT END ---
# Type can be
#    feat     (new feature)
#    fix      (bug fix)
#    refactor (refactoring code)
#    style    (formatting, missing semi colons, etc; no code change)
#    docs     (changes to documentation)
#    test     (adding or refactoring tests; no production code change)
#    chore    (updating grunt tasks etc; no production code change)
# --------------------
# Remember to:
#    Use the imperative mood in the subject line
#    Do not end the subject line with a period
#    Separate subject from body with a blank line
#    Use the body to explain what and why vs. how
#    Can use multiple lines with "-" for bullet points in body
# --------------------
EOT
exit 0
EOF
	@chmod +x .git/hooks/prepare-commit-msg
	@echo "$(GREEN)Git commit message template installed$(RESET)"