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
  (display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-use-some-window)))
  :init
  ;; base visual
  (menu-bar-mode -1)                   ;; no menu bar
  (toggle-scroll-bar -1)               ;; no scroll bar
  (tool-bar-mode -1)                   ;; no tool bar either
  (blink-cursor-mode -1)               ;; stop blinking

  ;; font of the century
  (add-to-list 'default-frame-alist
               '(font . "Aporetic Sans Mono-12"))

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
    "Alternative Catppuccin face for `pulsar-face'."
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
  :bind (("C-M-g" . magit-status)
         :map project-prefix-map
         ("m" . magit-project-status))
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

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

(use-package ghostel
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("M-<backspace>" . ghostel-backward-kill-word)
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("t" . ghostel-project)
         ("T" . ghostel-project-list-buffers))
  :config
  (defun ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
     Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

(use-package f :demand t)

(use-package ben
  :diminish ben-global-mode
  :bind
  (:map ben-mode-map
        ("C-c b" . ben-command-map))
  :hook
  (after-init . ben-global-mode))

;; Pinned in home/emacs/default.nix (Nix fetchFromGitHub + melpaBuild):
;; gptel 092bbc3, Macher fe36224, gptel-agent 326e0ab,
;; gptel-quick 36fe296, and gptel-magit f27c018.
(use-package gptel
  :demand t
  :commands (gptel gptel-send)
  :custom
  (gptel-model 'gpt-5.6-sol)
  (gptel-openai-oauth-login-method 'device)
  (gptel-cache t)
  (gptel-confirm-tool-calls nil)
  (gptel-include-reasoning 'ignore)
  (gptel-include-tool-results 'auto)
  (gptel-log-level nil)
  (gptel-system-prompt
   "You are an assistant inside Emacs. Help me understand and improve code that I write.
Use active read-only tools when the request needs project evidence.
Distinguish project evidence from inference. Cite files and symbols.
Explain the relevant control flow before you propose a change.
Do not propose code unless I ask for a change.
Use gptel-rewrite for one local proposal and Macher for a multi-file proposal.
Do not stage, commit, push, or change Git state.
Prefer a native Eglot action when it already supplies the requested change.
After a proposal, name the native diagnostic, build, or test command that establishes correctness.")
  :config
  (setq gptel-backend (gptel-make-openai-oauth "OpenAI-sub"))
  (setf (alist-get 'assistant gptel-directives) gptel-system-prompt))

(use-package gptel-rewrite
  :after gptel
  :commands gptel-rewrite
  :custom
  (gptel-rewrite-default-action nil))

(use-package gptel-org
  :after gptel
  :custom
  (gptel-org-branching-context t))

(use-package macher
  :after gptel
  :demand t
  :commands (macher-discuss macher-implement macher-revise)
  :custom
  (macher-workspace-functions
   '(macher-workspace-project macher-workspace-file))
  :config
  (macher-install)
  (macher-enable))

(use-package gptel-agent
  :after gptel
  :demand t)

(use-package emacs-assistant-context
  :after gptel
  :demand t)

(use-package emacs-assistant-activate
  :after (gptel macher gptel-agent emacs-assistant-context)
  :demand t
  :config
  (emacs-assistant-activate-install-base))

(use-package gptel-quick
  :commands gptel-quick)

(use-package gptel-magit
  ;; Fallback: select the Magit diff text and send it with ordinary gptel.
  :after (gptel magit)
  :config
  (require 'markdown-mode)
  (gptel-magit-install))

(use-package embark
  :demand t
  :config
  (keymap-set embark-identifier-map "?" #'gptel-quick)
  (keymap-set embark-region-map "?" #'gptel-quick))

(defvar-keymap emacs-assistant-command-map
  :doc "Quiet LLM assistant commands."
  "c" #'gptel
  "s" #'gptel-send
  "r" #'gptel-rewrite
  "q" #'gptel-quick
  "d" #'macher-discuss
  "p" #'macher-implement
  "m" #'gptel-menu
  "i" #'gptel-menu
  "x" #'gptel-abort)

(keymap-global-set "C-c a" emacs-assistant-command-map)

(use-package sideline-flymake)
(use-package sideline
  :custom
  (sideline-backends-right '(sideline-flymake))
  :hook
  (flymake-mode . sideline-mode))

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (jsonrpc-event-hook nil)
  :hook
  (eglot-managed-mode . (lambda ()
                          (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  :bind
  (:map eglot-mode-map
        ("C-c l a" . eglot-code-actions)
        ("C-c l b" . flymake-show-buffer-diagnostics)
        ("C-c l r" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l g" . xref-find-references)
        ("C-c l p" . flymake-show-project-diagnostics)
        ("C-c l w" . eglot-reconnect)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  (proced-enable-color-flag t)
  (proced-show-remote-processes t))

;; extras
(use-package comp-run
  :ensure nil
  :config
  (push "tramp-loaddefs.el.gz" native-comp-jit-compilation-deny-list)
  (push "cl-loaddefs.el.gz" native-comp-jit-compilation-deny-list))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . eglot-ensure))

(provide 'init)

;;; init.el ends here
