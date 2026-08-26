;;; emacs-assistant-context.el --- Bounded live-buffer and Xref tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only gptel tools for live buffer text and semantic Xref results.
;; Installed through Emacs 32's `user-lisp-directory'.

;;; Code:

(require 'gptel)
(require 'seq)
(require 'subr-x)
(require 'xref)

(defgroup emacs-assistant-context nil
  "Read-only context tools for gptel."
  :group 'gptel)

(defcustom emacs-assistant-context-max-output 8000
  "Maximum number of characters returned by one context tool call."
  :type 'natnum)

(defcustom emacs-assistant-context-max-xrefs 50
  "Maximum number of Xref results returned by one tool call."
  :type 'natnum)

(defun emacs-assistant-context--buffer (name)
  "Return the live buffer named NAME or signal a clear error."
  (or (get-buffer name)
      (user-error "No live buffer named %S" name)))

(defun emacs-assistant-context--character-bounds (start end)
  "Return validated character bounds from START and END."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (unless (and (integerp start) (integerp end)
                 (<= (point-min) start end (point-max)))
      (user-error "Character range must satisfy %d <= START <= END <= %d"
                  (point-min) (point-max)))
    (cons start end)))

(defun emacs-assistant-context--line-bounds (start end)
  "Return character bounds for one-based inclusive lines START through END."
  (let* ((last-line (line-number-at-pos (point-max) t))
         (start (or start 1))
         (end (or end last-line)))
    (unless (and (integerp start) (integerp end)
                 (<= 1 start end last-line))
      (user-error "Line range must satisfy 1 <= START <= END <= %d" last-line))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start))
      (let ((from (point)))
        (goto-char (point-min))
        (forward-line end)
        (cons from (point))))))

(defun emacs-assistant-context-read-buffer (buffer &optional start end unit)
  "Read bounded live text from BUFFER.

START and END are character positions when UNIT is \"character\" (the
default), with END exclusive.  When UNIT is \"line\", they are one-based,
inclusive line numbers."
  (with-current-buffer (emacs-assistant-context--buffer buffer)
    (save-restriction
      (widen)
      (let* ((unit (or unit "character"))
             (range-start (or start (if (equal unit "line") 1 (point-min))))
             (range-end (or end (if (equal unit "line")
                                    (line-number-at-pos (point-max) t)
                                  (point-max))))
             (bounds (pcase unit
                       ("character"
                        (emacs-assistant-context--character-bounds start end))
                       ("line"
                        (emacs-assistant-context--line-bounds start end))
                       (_ (user-error "UNIT must be \"character\" or \"line\""))))
             (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (header (format "Buffer: %s\n%s range: %d..%d\n\n"
                             (buffer-name) unit range-start range-end)))
        (when (> (+ (length header) (length text))
                 emacs-assistant-context-max-output)
          (user-error "Requested range is too large; maximum output is %d characters"
                      emacs-assistant-context-max-output))
        (concat header text)))))

(defun emacs-assistant-context--bound-output (text)
  "Return TEXT capped at `emacs-assistant-context-max-output'."
  (if (<= (length text) emacs-assistant-context-max-output)
      text
    (concat (substring text 0 (max 0 (- emacs-assistant-context-max-output 20)))
            "\n[output truncated]")))

(defun emacs-assistant-context--goto-identifier (identifier)
  "Place point at an exact occurrence of IDENTIFIER in the target buffer."
  (goto-char (point-min))
  (unless (or (re-search-forward
               (concat "\\_<" (regexp-quote identifier) "\\_>") nil t)
              (progn (goto-char (point-min))
                     (search-forward identifier nil t)))
    (user-error "Identifier %S is not present in buffer %s"
                identifier (buffer-name)))
  (goto-char (match-beginning 0)))

(defun emacs-assistant-context--xref (kind buffer identifier &optional limit)
  "Return KIND Xrefs for IDENTIFIER using BUFFER's active backend."
  (when (string-empty-p identifier)
    (user-error "IDENTIFIER must not be empty"))
  (let ((limit (or limit 20)))
    (unless (and (integerp limit) (<= 1 limit emacs-assistant-context-max-xrefs))
      (user-error "LIMIT must be between 1 and %d"
                  emacs-assistant-context-max-xrefs))
    (save-window-excursion
      (with-current-buffer (emacs-assistant-context--buffer buffer)
        (save-excursion
          (save-restriction
            (widen)
            (if-let* ((backend (xref-find-backend)))
                (progn
                  (emacs-assistant-context--goto-identifier identifier)
                  (let* ((items
                          (pcase kind
                            ('definitions
                             (xref-backend-definitions backend identifier))
                            ('references
                             (xref-backend-references backend identifier))))
                         (total (length items))
                         (lines
                          (mapcar
                           (lambda (item)
                             (let ((location (xref-item-location item)))
                               (format "%s:%s: %s [identifier: %s]"
                                       (xref-location-group location)
                                       (or (xref-location-line location) "?")
                                       (string-replace
                                        "\n" " "
                                        (substring-no-properties
                                         (xref-item-summary item)))
                                       identifier)))
                           (seq-take items limit))))
                    (emacs-assistant-context--bound-output
                     (format "Backend: %s\nIdentifier: %s\nReturned: %d of %d\n\n%s"
                             backend identifier (length lines) total
                             (if lines (string-join lines "\n") "No results.")))))
              (format "No semantic Xref backend is active for buffer %s."
                      (buffer-name)))))))))

(defun emacs-assistant-context-xref-definitions (buffer identifier &optional limit)
  "Return bounded semantic definitions for IDENTIFIER in BUFFER."
  (emacs-assistant-context--xref 'definitions buffer identifier limit))

(defun emacs-assistant-context-xref-references (buffer identifier &optional limit)
  "Return bounded semantic references for IDENTIFIER in BUFFER."
  (emacs-assistant-context--xref 'references buffer identifier limit))

(defvar emacs-assistant-context-read-buffer-tool
  (gptel-make-tool
   :name "ReadBuffer"
   :function #'emacs-assistant-context-read-buffer
   :description "Read bounded plain text, including unsaved edits, from a live Emacs buffer. This result contains no Flymake annotations."
   :args '((:name "buffer" :type string :description "Exact live buffer name.")
           (:name "start" :type integer :optional t :description "Start character position or one-based line number.")
           (:name "end" :type integer :optional t :description "Exclusive character end or inclusive line end.")
           (:name "unit" :type string :enum ["character" "line"] :optional t :description "Range unit; defaults to character."))
   :category "emacs-assistant"
   :confirm nil
   :include t))

(defvar emacs-assistant-context-xref-definitions-tool
  (gptel-make-tool
   :name "XrefDefinitions"
   :function #'emacs-assistant-context-xref-definitions
   :description "Ask the active semantic Xref backend in a live source buffer for bounded definitions."
   :args '((:name "buffer" :type string :description "Exact live source buffer name.")
           (:name "identifier" :type string :description "Identifier to resolve semantically.")
           (:name "limit" :type integer :optional t :description "Maximum results; defaults to 20 and cannot exceed 50."))
   :category "emacs-assistant"
   :confirm nil
   :include t))

(defvar emacs-assistant-context-xref-references-tool
  (gptel-make-tool
   :name "XrefReferences"
   :function #'emacs-assistant-context-xref-references
   :description "Ask the active semantic Xref backend in a live source buffer for bounded references."
   :args '((:name "buffer" :type string :description "Exact live source buffer name.")
           (:name "identifier" :type string :description "Identifier to resolve semantically.")
           (:name "limit" :type integer :optional t :description "Maximum results; defaults to 20 and cannot exceed 50."))
   :category "emacs-assistant"
   :confirm nil
   :include t))

(provide 'emacs-assistant-context)
;;; emacs-assistant-context.el ends here
