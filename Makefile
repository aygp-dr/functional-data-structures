.PHONY: help clean tangle scheme hy test lint format doc run-scheme run-hy install tmux-start tmux-attach tmux-stop tmux-status emacs-repl

# Default target is help
.DEFAULT_GOAL := help

# Directories
SCHEME_DIR := scheme
HY_DIR := hy
TEST_DIR := tests
ORG_FILES := $(wildcard *.org)
SCHEME_SOURCES := $(wildcard $(SCHEME_DIR)/**/*.scm)
HY_SOURCES := $(wildcard $(HY_DIR)/**/*.hy)

# Project configuration  
PROJECT_NAME := functional-data-structures
PROJECT_ROOT := $(shell pwd)
EMACS_CONFIG := $(PROJECT_ROOT)/$(PROJECT_NAME).el
TMUX_SESSION := $(PROJECT_NAME)

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
	@echo ""
	@echo "$(YELLOW)Development environment:$(RESET)"
	@echo "  $(CYAN)tmux-start$(RESET)      Start tmux session with Emacs"
	@echo "  $(CYAN)tmux-attach$(RESET)     Attach to existing tmux session"
	@echo "  $(CYAN)tmux-stop$(RESET)       Stop tmux session"
	@echo "  $(CYAN)tmux-status$(RESET)     Show tmux session status"
	@echo "  $(CYAN)emacs-repl$(RESET)      Start Emacs with Geiser REPL"

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
	@GUILE_LOAD_PATH="./scheme:$$GUILE_LOAD_PATH" $(GUILE) -L . -e main $(TEST_DIR)/scheme/test-runner.scm || (echo "$(RED)Scheme tests failed$(RESET)"; exit 1)
	@echo "$(GREEN)Scheme tests passed$(RESET)"

test-hy: tangle  ## Run Hy tests
	@echo "$(CYAN)Running Hy tests...$(RESET)"
	@PYTHONPATH="./hy:$$PYTHONPATH" poetry run pytest $(TEST_DIR)/hy || (echo "$(RED)Hy tests failed$(RESET)"; exit 1)
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
	@GUILE_LOAD_PATH="./scheme:$$GUILE_LOAD_PATH" $(GUILE) -c "(use-modules (okasaki stack)) (display \"Stack module loaded successfully\n\")"

run-hy: tangle  ## Run Hy implementation
	@echo "Running Hy implementation..."
	@PYTHONPATH="./hy:$$PYTHONPATH" $(HY) -c "(import okasaki.stack) (print \"Hy stack module loaded successfully\")"

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

# Tmux and Emacs development environment
tmux-start:  ## Start tmux session with project-specific Emacs configuration
	@echo "$(CYAN)Starting tmux session '$(TMUX_SESSION)' with Emacs...$(RESET)"
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		echo "Session '$(TMUX_SESSION)' already exists. Attaching..."; \
		tmux attach-session -t $(TMUX_SESSION); \
	else \
		echo "Creating new session '$(TMUX_SESSION)'..."; \
		tmux new-session -d -s $(TMUX_SESSION) "emacs -nw -l $(EMACS_CONFIG)"; \
		echo "Session created. Getting TTY..."; \
		tmux list-panes -t $(TMUX_SESSION) -F "Pane TTY: #{pane_tty}"; \
		echo "$(GREEN)Session '$(TMUX_SESSION)' started$(RESET)"; \
		echo "Attach with: tmux attach-session -t $(TMUX_SESSION)"; \
	fi

tmux-attach:  ## Attach to existing tmux session
	@echo "$(CYAN)Attaching to tmux session '$(TMUX_SESSION)'...$(RESET)"
	@tmux attach-session -t $(TMUX_SESSION) || echo "$(YELLOW)No session found. Run 'make tmux-start' first.$(RESET)"

tmux-stop:  ## Stop tmux session
	@echo "$(CYAN)Stopping tmux session '$(TMUX_SESSION)'...$(RESET)"
	@tmux kill-session -t $(TMUX_SESSION) 2>/dev/null || echo "$(YELLOW)Session '$(TMUX_SESSION)' not found$(RESET)"
	@echo "$(GREEN)Session stopped$(RESET)"

tmux-status:  ## Show tmux session status
	@echo "$(CYAN)Tmux session status:$(RESET)"
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		echo "$(GREEN)Session '$(TMUX_SESSION)' is running$(RESET)"; \
		tmux list-panes -t $(TMUX_SESSION) -F "  Pane: #{pane_index} TTY: #{pane_tty} Size: #{pane_width}x#{pane_height}"; \
	else \
		echo "$(YELLOW)Session '$(TMUX_SESSION)' is not running$(RESET)"; \
	fi

emacs-repl:  ## Start Emacs with Geiser REPL for Scheme development
	@echo "$(CYAN)Starting Emacs with Geiser REPL...$(RESET)"
	@emacs -l $(EMACS_CONFIG) --eval "(fds-open-repl)"