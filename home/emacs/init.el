;;; package --- Summary - My minimal Emacs init file -*- lexical-binding: t -*-

;;; Commentary:
;;; Simple Emacs setup I carry everywhere

;;; Code:
(load custom-file 'noerror)            ;; no error on missing custom file

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(use-package emacs
  :init
  (global-auto-revert-mode t)          ;; revert automatically on external file changes
  (savehist-mode)                      ;; save minibuffer history

  ;; base visual
  (menu-bar-mode -1)                   ;; no menu bar
  (toggle-scroll-bar -1)               ;; no scroll bar
  (tool-bar-mode -1)                   ;; no tool bar either
  (global-hl-line-mode +1)             ;; always highlight current line
  (blink-cursor-mode -1)               ;; stop blinking
  (global-display-line-numbers-mode 1) ;; always show line numbers
  (column-number-mode t)               ;; column number in the mode line
  (size-indication-mode t)             ;; file size in the mode line
  (pixel-scroll-precision-mode)        ;; smooth mouse scroll
  (fset 'yes-or-no-p 'y-or-n-p)        ;; y/n is good enough
  (electric-pair-mode)                 ;; i mean ... parens should auto create
  (recentf-mode)                       ;; keep track of recently opened files

  ;; font of the century
  (set-frame-font "Iosevka Nerd Font 12" nil t)

  :bind
  (("C-<wheel-up>"   . pixel-scroll-precision) ; dont zoom in please, just scroll
   ("C-<wheel-down>" . pixel-scroll-precision) ; dont zoom in either, just scroll
   ("C-x k"          . kill-current-buffer))   ; kill the buffer, dont ask
  )

(use-package nerd-icons
  :custom
  ;; disable bright icon colors
  (nerd-icons-color-icons nil))

(use-package doom-modeline
  :custom
  (inhibit-compacting-font-caches t)    ;; speed
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)   ;; distracting icons, no thank you
  (doom-modeline-buffer-encoding nil)   ;; everything is utf-8 anyway
  (doom-modeline-buffer-state-icon nil) ;; the filename already shows me
  (doom-modeline-lsp nil)               ;; lsp state is too distracting, too often
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :commands doom-themes-visual-bell-config
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config))

(use-package diminish :demand t)         ;; declutter the modeline
(use-package eldoc :diminish eldoc-mode) ;; docs for everything

(use-package whitespace-cleanup-mode
  :commands global-whitespace-cleanup-mode
  :custom
  (whitespace-cleanup-mode-only-if-initially-clean nil)
  :hook
  (after-init . global-whitespace-cleanup-mode))

(use-package pulsar
  :commands pulsar-global-mode pulsar-recenter-top pulsar-reveal-entry
  :init
  (defface pulsar-nord
    '((default :extend t)
      (((class color) (min-colors 88) (background light))
       :background "#2e3440")
      (((class color) (min-colors 88) (background dark))
       :background "#81a1c1")
      (t :inverse-video t))
    "Alternative nord face for `pulsar-face'."
    :group 'pulsar-faces)
  :custom
  (pulsar-face 'pulsar-nord)
  :hook
  (after-init . pulsar-global-mode))

(use-package which-key
  :commands which-key-mode
  :diminish which-key-mode
  :hook
  (after-init . which-key-mode))

(use-package expreg
  :bind ("M-m" . expreg-expand))

(use-package vundo) ;; undo tree

;; better structured editing
(use-package puni
  :commands puni-global-mode
  :hook
  (after-init . puni-global-mode))

(use-package avy
  :bind
  ("M-i" . avy-goto-char-2)
  :custom
  (avy-background t))

(use-package consult
  :bind
  ("C-x b"   . consult-buffer)     ;; orig. switch-to-buffer
  ("M-y"     . consult-yank-pop)   ;; orig. yank-pop
  ("M-g M-g" . consult-goto-line)  ;; orig. goto-line
  :custom
  (consult-narrow-key "<"))

(use-package vertico
  :commands vertico-mode
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  :init
  (vertico-mode)
  :config
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package marginalia
  :commands marginalia-mode
  :hook (after-init . marginalia-mode))

(use-package crux
  :bind
  ("C-c M-e" . crux-find-user-init-file)
  ("C-c C-w" . crux-transpose-windows)
  ("C-a"     . crux-move-beginning-of-line))

(use-package magit
  :bind (("C-M-g" . magit-status)))

(use-package nerd-icons-corfu
  :commands nerd-icons-corfu-formatter
  :defines corfu-margin-formatters)

(use-package corfu
  :commands global-corfu-mode
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay  1)
  (corfu-auto-prefix 3)
  (corfu-separator ?_)
  :hook
  (after-init . global-corfu-mode)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape)

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets :after yasnippet)

(use-package projectile
  :commands projectile-mode
  :diminish projectile-mode
  :custom
  (projectile-globally-ignored-directories (append '("node_modules")))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-arguments nil)
  :hook
  (after-init . exec-path-from-shell-initialize))

(use-package flycheck
  :commands global-flycheck-mode
  :diminish
  :hook
  (after-init . global-flycheck-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-format-buffer
                 lsp-organize-imports
                 orderless-dispatch-flex-first
                 cape-capf-buster lsp-completion-at-point)
  :defines lsp-file-watch-ignored-directories
  :diminish lsp-lens-mode
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-lens-enable nil)
  (lsp-idle-delay 0.500)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-csharp-omnisharp-roslyn-binary-path "OmniSharp")
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-eldoc-render-all t)
  :init
  (defun orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  ;; Configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'orderless-dispatch-flex-first nil 'local)

  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions
              (list (cape-capf-buster #'lsp-completion-at-point)))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\Temp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\Logs\\'")
  (defun lsp-cleanup ()
    (lsp-format-buffer)
    (lsp-organize-imports)
    (whitespace-cleanup))
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion)
  (lsp-mode . lsp-enable-which-key-integration)
  (before-save . lsp-cleanup)
  (rust-mode . lsp-deferred)
  (csharp-ts-mode . lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-diagnostic-max-lines 4)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-eldoc-enable-hover nil)
  )

(use-package rust-mode
:ensure t
:init
(setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))



(use-package nixpkgs-fmt
  :custom
  (nixpkgs-fmt-command "nixfmt"))

(use-package typescript-ts-mode
  :custom
  (lsp-javascript-preferences-import-module-specifier :relative)
  (typescript-indent-level 2)
  (typescript-ts-mode-indent-offset 2))

(use-package eat
  :bind
  (("C-c e p" . eat-project)
   ("C-c e t" . eat)))
(use-package hcl-mode)
(use-package jinja2-mode)
(use-package f :demand t)

(use-package envrc
  :commands envrc-global-mode
  :hook
  (after-init . envrc-global-mode))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

(use-package shell-pop
  :custom
  (shell-pop-universal-key "M-o"))

(use-package copilot
  :defines copilot-completion-map
  :hook prog-mode
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("M-n" . copilot-next-completion)
        ("M-p" . copilot-previous-completion)
        ("C-g" . copilot-clear-overlay)))

(use-package gptel
  :commands gptel-make-anthropic f-read-text
  :config
  (gptel-make-anthropic "Claude"
    :stream t :key (f-read-text "/run/secrets/claude_key")))

(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-capture org-agenda)
  :init
  (defvar org-journal-file "~/nextcloud/org/journal.org")
  (defvar org-archive-file "~/nextcloud/org/archive.org")
  (defvar org-notes-file "~/nextcloud/org/notes.org")
  (defvar org-inbox-file "~/nextcloud/org/inbox.org")
  (defvar org-work-file "~/nextcloud/org/work.org")
  (defun my/org-capture-project-target-heading ()
    "Determine Org target headings from the current file's project path.

  This function assumes a directory structure like '~/projects/COMPANY/PROJECT/'.
  It extracts 'COMPANY' and 'PROJECT' to use as nested headlines
  for an Org capture template.

  If the current buffer is not visiting a file within such a
  project structure, it returns nil, causing capture to default to
  the top of the file."
    (when-let ((path (buffer-file-name))) ; Ensure we are in a file-visiting buffer
      (let ((path-parts (split-string path "/" t " ")))
        (when-let* ((projects-pos (cl-position "projects" path-parts :test #'string=))
                    (company      (nth (+ 1 projects-pos) path-parts))
                    (project      (nth (+ 2 projects-pos) path-parts)))
          ;; Return a list of headlines for Org to find or create.
          (list company project)))))
  :bind
  (("C-c c" . org-capture)
   ("C-c i" . org-store-link)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c t" . org-toggle-inline-images)
   ("C-c l" . org-toggle-link-display))
  :custom
  (org-agenda-files (list org-inbox-file org-journal-file))
  (org-directory "~/nextcloud/org")
  (org-default-notes-file org-inbox-file)
  (org-archive-location (concat org-archive-file "::* From %s"))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-capture-templates '(("t" "Todo" entry (file org-inbox-file)
                            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%a\n\n)")
                           ("j" "Journal" entry (file+olp+datetree org-journal-file)
                            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%a\n\n")
                           ("n" "Note" entry (file org-notes-file)
                            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%a\n\n")
                           ("p" "Project Task" item
                            (file+function org-work-file my/org-capture-project-target-heading)
                            "* TODO %? \n  CLOCK: %U"
                            ))
                         )
  :config
  ;; Enable syntax highlighting in code blocks
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package aider
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  (setenv "ANTHROPIC_API_KEY" (f-read-text "/run/secrets/claude_key"))
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
  (aider-magit-setup-transients))


(provide 'init)

;;; init.el ends here
