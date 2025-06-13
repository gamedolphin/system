;;; package --- early init -*- lexical-binding: t -*-

;;; Commentary:
;;; Prevents white flash and better Emacs defaults

;;; Code:
(set-language-environment "UTF-8")

(setq-default
 default-frame-alist
 '((background-color . "#3F3F3F")
   (bottom-divider-width . 1)            ; Thin horizontal window divider
   (foreground-color . "#DCDCCC")        ; Default foreground color
   (fullscreen . maximized)              ; Maximize the window by default
   (horizontal-scroll-bars . nil)        ; No horizontal scroll-bars
   (left-fringe . 8)                     ; Thin left fringe
   (menu-bar-lines . 0)                  ; No menu bar
   (right-divider-width . 1)             ; Thin vertical window divider
   (right-fringe . 8)                    ; Thin right fringe
   (tool-bar-lines . 0)                  ; No tool bar
   (undecorated . t)                     ; Remove extraneous X decorations
   (vertical-scroll-bars . nil))         ; No vertical scroll-bars

 user-full-name "Sandeep Nambiar"                ; ME!

 ;; memory configuration
 ;; Higher garbage collection threshold, prevents frequent gc locks
 gc-cons-threshold most-positive-fixnum
 ;; Ignore warnings for (obsolete) elisp compilations
 byte-compile-warnings '(not obsolete)
 ;; And other log types completely
 warning-suppress-log-types '((comp) (bytecomp))
 ;; Large files are okay in the new millenium.
 large-file-warning-threshold 100000000

 ;; Read more based on system pipe capacity
 read-process-output-max (max (* 10240 10240) read-process-output-max)

 ;; scroll configuration
 scroll-margin 0                                 ; Lets scroll to the end of the margin
 scroll-conservatively 100000                    ; Never recenter the window
 scroll-preserve-screen-position 1               ; Scrolling back and forth

 ;; frame config
 ;; Improve emacs startup time by not resizing to adjust for custom settings
 frame-inhibit-implied-resize t
 ;; Dont resize based on character height / width but to exact pixels
 frame-resize-pixelwise t

 ;; backups & files
 backup-directory-alist '(("." . "~/.backups/")) ; Don't clutter
 backup-by-copying t                             ; Don't clobber symlinks
 create-lockfiles nil                            ; Don't have temp files
 delete-old-versions t                           ; Cleanup automatically
 kept-new-versions 6                             ; Update every few times
 kept-old-versions 2                             ; And cleanup even more
 version-control t                               ; Version them backups
 delete-by-moving-to-trash t                     ; Dont delete, send to trash instead

 ;; startup
 inhibit-startup-screen t                        ; I have already done the tutorial. Twice
 inhibit-startup-message t                       ; I know I am ready
 inhibit-startup-echo-area-message t             ; Yep, still know it
 initial-scratch-message nil                     ; I know it is the scratch buffer!

 ;; tabs
 tab-width 4                                     ; Always tab 4 spaces.
 indent-tabs-mode nil                            ; Never use actual tabs.

 ;; rendering
 cursor-in-non-selected-windows nil              ; dont render cursors other windows

 ;; packages
 use-package-always-defer t

 ;; custom
 custom-file (concat user-emacs-directory "custom.el")

 load-prefer-newer t

 default-input-method nil
 )
 ;;; early-init.el ends here
