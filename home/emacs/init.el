;;; package --- Summary - My minimal Emacs init file -*- lexical-binding: t -*-

  ;;; Commentary:
  ;;; Simple Emacs setup I carry everywhere

  ;;; Code:
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)            ;; no error on missing custom file

(require 'package)
(package-initialize)

(defun reset-custom-vars ()
  "Resets the custom variables that were set to crazy numbers"
  (setopt gc-cons-threshold (* 1024 1024 100))
  (setopt garbage-collection-messages t))

(use-package emacs
  :custom
  (native-comp-async-query-on-exit t)
  (read-answer-short t)
  (use-short-answers t)
  (enable-recursive-minibuffers t)
  (which-func-update-delay 1.0)
  (visible-bell nil)
  (custom-buffer-done-kill t)
  (whitespace-line-column nil)
  (x-underline-at-descent-line t)
  (imenu-auto-rescan t)
  (uniquify-buffer-name-style 'forward)
  (confirm-nonexistent-file-or-buffer nil)
  (create-lockfiles nil)
  (make-backup-files nil)
  (kill-do-not-save-duplicates t)
  (sentence-end-double-space nil)
  (treesit-enabled-modes t)
  :init
  ;; base visual
  (menu-bar-mode -1)                   ;; no menu bar
  (toggle-scroll-bar -1)               ;; no scroll bar
  (tool-bar-mode -1)                   ;; no tool bar either
  (blink-cursor-mode -1)               ;; stop blinking

  ;; font of the century
  (set-frame-font "Aporetic Sans Mono 12" nil t)

  :bind
  (("C-<wheel-up>"   . pixel-scroll-precision) ; dont zoom in please, just scroll
   ("C-<wheel-down>" . pixel-scroll-precision) ; dont zoom in either, just scroll
   ("C-x k"          . kill-current-buffer))   ; kill the buffer, dont ask
  :hook
  (text-mode . delete-trailing-whitespace-mode)
  (prog-mode . delete-trailing-whitespace-mode)
  (after-init . global-display-line-numbers-mode) ;; always show line numbers
  (after-init . column-number-mode)               ;; column number in the mode line
  (after-init . size-indication-mode)             ;; file size in the mode line
  (after-init . pixel-scroll-precision-mode)      ;; smooth mouse scroll
  (after-init . electric-pair-mode)               ;; i mean ... parens should auto create
  (after-init . reset-custom-vars)
  )

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t)
  :hook
  (after-init . global-auto-revert-mode))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(use-package hl-line
  :ensure nil
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  :hook
  (after-init . global-hl-line-mode))

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

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

(load-theme 'catppuccin :no-confirm)

(use-package diminish :demand t)         ;; declutter the modeline
(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)) ;; docs for everything

(use-package eldoc-box
  :defer t
  :config
  (set-face-background 'eldoc-box-border (catppuccin-color 'green))
  (set-face-background 'eldoc-box-body (catppuccin-color 'base))
  :bind
  (("M-h" . eldoc-box-help-at-point)))

(use-package pulsar
  :commands pulsar-global-mode pulsar-recenter-top pulsar-reveal-entry
  :init
  (defface pulsar-catppuccin
    `((default :extend t)
      (((class color) (min-colors 88) (background light))
       :background ,(catppuccin-color 'sapphire))
      (((class color) (min-colors 88) (background dark))
       :background ,(catppuccin-color 'sapphire))
      (t :inverse-video t))
    "Alternative nord face for `pulsar-face'."
    :group 'pulsar-faces)
  :custom
  (pulsar-face 'pulsar-catppuccin)
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
  ("M-g i"   . consult-imenu)      ;; consult version is interactive
  ("M-g r"   . consult-ripgrep)    ;; find in project also works
  :custom
  (consult-narrow-key "<"))

(use-package vertico
  :commands vertico-mode
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (vertico-mode)
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package marginalia
  :commands marginalia-mode
  :hook (after-init . marginalia-mode))

(use-package crux
  :bind
  ("C-c M-e" . crux-find-user-init-file)
  ("C-c C-w" . crux-transpose-windows)
  ("C-c M-d" . crux-find-current-directory-dir-locals-file)
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

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-arguments nil)
  :hook
  (after-init . exec-path-from-shell-initialize))

(use-package nixpkgs-fmt
  :custom
  (nixpkgs-fmt-command "nixfmt"))

(use-package eat
  :bind
  (("C-c e p" . eat-project)
   ("C-c e t" . eat)))

(use-package f :demand t)

(use-package envrc
  :commands envrc-global-mode
  :hook
  (after-init . envrc-global-mode))

(use-package gptel
  :commands gptel-make-anthropic f-read-text
  :config
  (gptel-make-anthropic "Claude"
    :stream t :key (f-read-text "/run/secrets/claude_key")))

(use-package sideline-flymake)
(use-package sideline-eglot)
(use-package sideline
  :custom
  (sideline-backends-right '(sideline-flymake sideline-eglot))
  :hook
  (eglot-managed-mode . sideline-mode)
  (flymake-mode . sideline-mode))

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (jsonrpc-event-hook nil)
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  (before-save . eldoc-format-buffer)
  :bind
  (:map eglot-mode-map
        ("C-c l a" . eglot-code-actions)
        ("C-c l r" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l g" . xref-find-references)
        ("C-c l w" . eglot-reconnect)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  (proced-enable-color-flag t)
  (proced-show-remote-processes t))

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

    If the current buffer is not visi
ting a file within such a
    project structure, it returns nil, causing capture to default to
    the top of the file."
    (when-let* ((path (buffer-file-name))) ; Ensure we are in a file-visiting buffer
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

;; extras
(use-package comp-run
  :ensure nil
  :config
  (push "tramp-loaddefs.el.gz" native-comp-jit-compilation-deny-list)
  (push "cl-loaddefs.el.gz" native-comp-jit-compilation-deny-list))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))

(provide 'init)

  ;;; init.el ends here
