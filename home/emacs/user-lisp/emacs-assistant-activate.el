;;; emacs-assistant-activate.el --- Progressive gptel tool groups -*- lexical-binding: t; -*-

;;; Commentary:
;; Activate a small static set of registered gptel tools per conversation.
;; Installed through Emacs 32's `user-lisp-directory'.
;; Native provider tool search should replace this adapter when gptel supports it.
;; Automatic continuation is intentionally disabled: current public gptel tool
;; hooks stop through the error path, which fails the quietness gate in the plan.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-agent)
(require 'gptel-agent-tools-introspection)
(require 'seq)
(require 'subr-x)

(defconst emacs-assistant-activate-groups
  '((base
     "activate_tools"
     "ReadBuffer"
     "Diagnostics"
     "read_file_in_workspace"
     "list_directory_in_workspace"
     "search_in_workspace"
     "XrefDefinitions"
     "XrefReferences")
    (emacs "introspection" "Eval")
    (patch
     "edit_file_in_workspace"
     "multi_edit_file_in_workspace"
     "write_file_in_workspace"
     "move_file_in_workspace"
     "delete_file_in_workspace")
    (research "WebSearch" "WebFetch")
    (delegate "Agent"))
  "Static allowlist of tool names and categories by group.")

(defconst emacs-assistant-activate-directives
  '((emacs . "Inspect the live Emacs runtime, package source, and manuals when that evidence answers the request.\nUse Eval only for a bounded investigation. Propose persistent changes for human review.")
    (patch . "Create the smallest requested patch. Keep all changes in the Macher workspace until I apply selected hunks.")
    (delegate . "Use one read-only subagent for a bounded research or review task.\nUse at most two when separate evidence sources or review lenses justify them.\nGive it the project root, exact scope, and required evidence.\nIf a named skill applies, tell the researcher to load that skill before analysis.\nCheck its evidence and integrate only the relevant findings."))
  "Additional stable directive text for optional tool groups.")

(defconst emacs-assistant-activate-allowed-agents
  ["researcher" "introspector"]
  "Only subagent definitions exposed through the delegate group.")

(defcustom emacs-assistant-activate-max-research-output 20000
  "Maximum characters returned by one research tool call."
  :type 'natnum
  :group 'gptel)

(defun emacs-assistant-activate-lifecycle-hooks-p ()
  "Return non-nil when this gptel has both public tool lifecycle hooks."
  (and (boundp 'gptel-pre-tool-call-functions)
       (boundp 'gptel-post-tool-call-functions)))

(defun emacs-assistant-activate--tool-list (name)
  "Resolve registered tool or category NAME to a list of tool specs."
  (let ((value (condition-case nil
                   (gptel-get-tool name)
                 (error nil))))
    (cond ((gptel-tool-p value) (list value))
          ((and value (listp value) (seq-every-p #'gptel-tool-p value)) value)
          (t (user-error "Required gptel tool or category is missing: %s" name)))))

(defun emacs-assistant-activate--bound-research-result (callback result)
  "Call CALLBACK with bounded research RESULT."
  (let* ((limit (max 32 emacs-assistant-activate-max-research-output))
         (suffix "\n[output truncated]"))
    (funcall callback
             (if (and (stringp result) (> (length result) limit))
                 (concat (substring result 0 (- limit (length suffix))) suffix)
               result))))

(defun emacs-assistant-activate--bound-search-result (callback count result)
  "Call CALLBACK with at most COUNT bounded search results from RESULT."
  (emacs-assistant-activate--bound-research-result
   callback
   (if (stringp result)
       (string-join
        (seq-take (split-string result "\n\n----\n" t) count)
        "\n\n----\n")
     result)))

(defun emacs-assistant-activate--bound-research-tool (tool)
  "Return a copy of async research TOOL with bounded output."
  (let ((copy (copy-sequence tool))
        (function (gptel-tool-function tool)))
    (setf (gptel-tool-function copy)
          (if (equal (gptel-tool-name tool) "WebSearch")
              (lambda (callback query &optional count)
                (let ((count (min 5 (max 1 (or count 5)))))
                  (funcall function
                           (apply-partially
                            #'emacs-assistant-activate--bound-search-result
                            callback count)
                           query count)))
            (lambda (callback &rest args)
              (apply function
                     (apply-partially
                      #'emacs-assistant-activate--bound-research-result callback)
                     args))))
    copy))

(defun emacs-assistant-activate--resolve (group)
  "Resolve every registered tool declared for GROUP."
  (let ((names (cdr (assq group emacs-assistant-activate-groups))))
    (unless names
      (user-error "Unknown tool group: %s" group))
    (let ((tools (apply #'append
                        (mapcar #'emacs-assistant-activate--tool-list names))))
      (if (eq group 'research)
          (mapcar #'emacs-assistant-activate--bound-research-tool tools)
        tools))))

(defun emacs-assistant-activate--merge (&rest tool-lists)
  "Merge TOOL-LISTS in order, removing duplicate tool names."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (tool (apply #'append tool-lists))
      (unless (gethash (gptel-tool-name tool) seen)
        (puthash (gptel-tool-name tool) t seen)
        (push tool result)))
    (nreverse result)))

(defun emacs-assistant-activate--append-directive (group)
  "Append GROUP's directive once in the current buffer."
  (when-let* ((directive (alist-get group emacs-assistant-activate-directives)))
    (unless (stringp gptel-system-prompt)
      (user-error "Tool activation requires a string gptel system prompt"))
    (unless (string-match-p (regexp-quote directive) gptel-system-prompt)
      (setq-local gptel-system-prompt
                  (concat gptel-system-prompt "\n\n" directive)))))

(defun emacs-assistant-activate--restrict-agents ()
  "Refresh gptel-agent and restrict the Agent argument enum."
  (gptel-agent-update)
  (let* ((tool (gptel-get-tool "Agent"))
         (argument (and (gptel-tool-p tool) (car (gptel-tool-args tool))))
         (discovered (and argument (plist-get argument :enum))))
    (unless (seq-every-p (lambda (name) (seq-contains-p discovered name #'equal))
                         emacs-assistant-activate-allowed-agents)
      (when argument
        (setf (plist-get argument :enum) []))
      (user-error "Delegate group unavailable: required read-only agents are missing"))
    (setf (plist-get argument :enum)
          (copy-sequence emacs-assistant-activate-allowed-agents))
    (unless (equal (plist-get argument :enum)
                   emacs-assistant-activate-allowed-agents)
      (user-error "Delegate group unavailable: agent allowlist check failed"))))

(defun emacs-assistant-activate-tools (group)
  "Activate declared tool GROUP for the current conversation buffer."
  (let ((requested group)
        (group (if (symbolp group) group (intern-soft group))))
    (unless (assq group emacs-assistant-activate-groups)
      (user-error "Unknown tool group: %s" requested))
    (when (eq group 'delegate)
      (emacs-assistant-activate--restrict-agents))
    (let ((tools (emacs-assistant-activate--resolve group)))
      (emacs-assistant-activate--append-directive group)
      (setq-local gptel-tools
                  (emacs-assistant-activate--merge gptel-tools tools))
      (format "Activated %s for the next explicit send: %s. %s"
              group (mapconcat #'gptel-tool-name tools ", ")
              (if (emacs-assistant-activate-lifecycle-hooks-p)
                  "Automatic handoff is disabled because gptel's public stop hook currently reports a request error."
                "Automatic handoff is unavailable because this gptel has no public tool lifecycle hooks.")))))

(defvar emacs-assistant-activate-tool
  (gptel-make-tool
   :name "activate_tools"
   :function #'emacs-assistant-activate-tools
   :description "Activate one declared optional tool group for this conversation. The result states whether another explicit send is required."
   :args '((:name "group"
            :type string
            :enum ["base" "emacs" "patch" "research" "delegate"]
            :description "Static tool group to activate."))
   :category "emacs-assistant"
   :confirm nil
   :include t))

(defun emacs-assistant-activate-install-base ()
  "Install the stable base tools as the default gptel tool set."
  (setq-default gptel-tools
                (emacs-assistant-activate--resolve 'base))
  (add-hook 'gptel-mode-hook #'emacs-assistant-activate-use-base))

(defun emacs-assistant-activate-use-base ()
  "Give a new gptel conversation its own copy of the base tool set."
  (unless (local-variable-p 'gptel-tools)
    (setq-local gptel-tools (copy-sequence (default-value 'gptel-tools)))))

(provide 'emacs-assistant-activate)
;;; emacs-assistant-activate.el ends here
