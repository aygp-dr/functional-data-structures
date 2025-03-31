;; Functional Data Structures Project - Emacs Configuration
;; This configuration sets up Emacs for developing with Org-mode, Scheme, and Hy

;; Package system setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Org-mode configuration
(use-package org
  :config
  ;; Enable syntax highlighting in org-mode code blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)
  
  ;; Enable babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . t)
     (shell . t)
     (python . t))))

;; Geiser for Scheme
(use-package geiser
  :config
  ;; Set Guile as the default Scheme implementation
  (setq geiser-active-implementations '(guile)
        geiser-default-implementation 'guile))

(use-package geiser-guile
  :after geiser)

;; Hy-mode for Hy language
(use-package hy-mode
  :mode "\\.hy\\'")

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Company for code completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2))

;; Helpful keybindings for org-babel
(global-set-key (kbd "C-c C-v t") 'org-babel-tangle)
(global-set-key (kbd "C-c C-v e") 'org-babel-execute-src-block)
(global-set-key (kbd "C-c C-v b") 'org-babel-execute-buffer)

;; Custom functions for this project
(defun okasaki-tangle-all ()
  "Tangle all org files in the project."
  (interactive)
  (org-babel-tangle-file "okasaki-data-structures.org"))

(defun okasaki-run-scheme ()
  "Run the Scheme implementation."
  (interactive)
  (okasaki-tangle-all)
  (let ((default-directory (projectile-project-root)))
    (geiser-run-guile "guile")
    (geiser-eval-buffer)))

(defun okasaki-run-hy ()
  "Run the Hy implementation."
  (interactive)
  (okasaki-tangle-all)
  (let ((default-directory (projectile-project-root)))
    (run-python)
    (python-shell-send-string "import hy")))

;; Custom keybindings for project-specific functions
(global-set-key (kbd "C-c o t") 'okasaki-tangle-all)
(global-set-key (kbd "C-c o s") 'okasaki-run-scheme)
(global-set-key (kbd "C-c o h") 'okasaki-run-hy)

;; Initial message
(setq initial-scratch-message ";; Functional Data Structures Project
;; Useful commands:
;;   C-c o t  Tangle all org files
;;   C-c o s  Run Scheme implementation
;;   C-c o h  Run Hy implementation
;;   C-c C-v t  Tangle current org file
;;   C-c C-v e  Execute current src block
;;   C-c C-v b  Execute all src blocks in buffer

")

(provide 'init)