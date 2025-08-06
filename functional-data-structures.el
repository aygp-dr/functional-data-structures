;;; functional-data-structures.el --- Emacs configuration for Functional Data Structures project

;;; Commentary:
;; Project-specific Emacs configuration for Scheme development
;; with Geiser, Guile, org-mode, TRAMP, and paredit support

;;; Code:

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar required-packages
  '(geiser
    geiser-guile
    paredit
    org
    company
    flycheck
    rainbow-delimiters))

(dolist (pkg required-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Geiser configuration for Guile
(require 'geiser)
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-default-implementation 'guile)

;; Add project directories to Guile load path
(defvar project-root (or (getenv "PROJECT_ROOT") default-directory))
(setq geiser-guile-load-path
      (list (expand-file-name "scheme" project-root)
            (expand-file-name "." project-root)))

;; Paredit for structured editing
(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Rainbow delimiters for better visibility
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Company mode for auto-completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)

;; Org-mode configuration for literate programming
(require 'org)
(setq org-babel-load-languages
      '((scheme . t)
        (emacs-lisp . t)
        (python . t)
        (shell . t)))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)

;; TRAMP configuration for remote development
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name
      (expand-file-name ".tramp" project-root))

;; Flycheck for syntax checking
(require 'flycheck)
(add-hook 'scheme-mode-hook 'flycheck-mode)

;; Project-specific key bindings
(global-set-key (kbd "C-c g") 'geiser-mode-switch-to-repl)
(global-set-key (kbd "C-c C-g") 'geiser-mode-switch-to-repl-and-enter)
(global-set-key (kbd "C-c r") 'geiser-eval-region)
(global-set-key (kbd "C-c b") 'geiser-eval-buffer)
(global-set-key (kbd "C-c d") 'geiser-doc-symbol-at-point)

;; Custom functions for project
(defun fds-run-tests ()
  "Run project tests."
  (interactive)
  (compile "make test"))

(defun fds-tangle-org ()
  "Tangle org files."
  (interactive)
  (compile "make tangle"))

(defun fds-open-repl ()
  "Open Guile REPL with project configuration."
  (interactive)
  (run-geiser 'guile))

;; Key bindings for custom functions
(global-set-key (kbd "C-c t") 'fds-run-tests)
(global-set-key (kbd "C-c o") 'fds-tangle-org)
(global-set-key (kbd "C-c R") 'fds-open-repl)

;; Set up project files
(when (file-exists-p project-root)
  (cd project-root)
  (message "Functional Data Structures project loaded from %s" project-root))

;; Scheme-specific configuration
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 2)
            (setq-local scheme-indent-offset 2)))

;; Start with a clean environment
(message "Emacs configured for Functional Data Structures project")

(provide 'functional-data-structures)
;;; functional-data-structures.el ends here