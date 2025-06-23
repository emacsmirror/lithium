;;; lithium.el --- Lightweight modal interfaces -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/lithium
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, emulations, lisp, tools

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:
;;
;; Lightweight modal interfaces
;;

;;; Code:

(require 'cl-lib)

(cl-defstruct lithium-mode-metadata
  "Metadata for a lithium mode."
  (name nil :documentation "The symbol that is the mode name.")
  (map nil :documentation "The keymap for the mode."))

(defvar-local lithium-mode-stack nil)

(defvar lithium-promoted-map nil
  "The current overriding lithium mode keymap.

A keymap corresponding to a lithium mode that is currently promoted
as an overriding terminal local map, meaning that it takes precedence
over all other keybindings.  From Lithium's perspective, only one of
these may be active at any time, based on context.  We keep track of
which one it is so that we can demote it before promoting another.")

(defun lithium-current-mode ()
  "Current mode at the top of the mode stack."
  (when lithium-mode-stack
    (car lithium-mode-stack)))

(defun lithium-current-mode-name ()
  "Name of the current mode as a symbol."
  (let ((mode (lithium-current-mode)))
    (when mode
      (lithium-mode-metadata-name mode))))

(defun lithium-push-mode (mode)
  "Push MODE onto the mode stack."
  (push mode lithium-mode-stack))

(defun lithium-pop-mode (name)
  "Remove the mode named NAME in the mode stack, if present."
  (setq lithium-mode-stack
        (seq-remove (lambda (m)
                      (equal (lithium-mode-metadata-name m)
                             name))
                    lithium-mode-stack)))

(defun lithium--stack-contents ()
  "Modes currently in the stack, by name."
  (mapcar #'lithium-mode-metadata-name lithium-mode-stack))

(defconst lithium-mundane-errors
  '(beginning-of-line
    end-of-line
    beginning-of-buffer
    end-of-buffer
    user-error)
  "Error signals that are known to be part of normal Emacs operation.")

(defmacro lithium--handle-key (mode action pre-exit post-exit should-exit)
  "Handle a key for MODE.

Execute ACTION.  If the key SHOULD-EXIT, or if an unhandled error is
encountered, exit MODE, invoking PRE-EXIT and POST-EXIT hooks at the
appropriate times."
  (if should-exit
      `(lambda ()
         (interactive)
         ;; if we interrupt execution via `C-g', or if the
         ;; command encounters an error during execution,
         ;; we still want to run post-exit hooks to ensure
         ;; that we leave things in a clean state
         (unwind-protect
             ;; do the action
             ;; TODO: we could also consider suspending the overriding
             ;; map before running the command, and then evaluating it
             ;; (reinstating it) afterwards, both here and for
             ;; non-exiting commands, below. It would be ideal to wait
             ;; until we have an actual issue that demonstrates a
             ;; problem before making this change, however, as it does
             ;; add a little complexity.
             (progn
               (setq this-command #',action)
               (call-interactively #',action))
           ;; As this is an exiting key, exit the mode.
           ;; It's possible that the `action' could itself already
           ;; have exited the mode, so we first check whether the mode
           ;; is active before attempting to exit.
           (lithium-exit-mode ',mode)))
    `(lambda ()
       (interactive)
       (condition-case err
           (progn
             (setq this-command #',action)
             (call-interactively #',action))
         ;; if we interrupt execution via `C-g', that's fine.
         ;; but if the command encounters an error during execution,
         ;; we quit the mode to be on the safe side, and also
         ;; run exit hooks
         (error
          (let ((conditions (get (car err) 'error-conditions)))
            ;; ignore "mundane" errors
            (unless (member (car conditions) lithium-mundane-errors)
              (run-hooks ',pre-exit)
              (funcall #',mode -1)
              (run-hooks ',post-exit))
            ;; re-raise the interrupt
            (signal (car err) (cdr err))))))))

;; TODO: should we define a mode struct that is passed around internally,
;; instead of interning global symbol names to discover hooks?
(cl-defmacro lithium-define-key (mode key action &optional should-exit)
  "Bind KEY to FN in MODE.

If EXIT is true, exit the mode after running the command.

If `exit' is missing, then it's an ordinary binding of key to action,
but if exit is present and set to true, then also exit the MODE after
performing the action."
  (declare (indent 1))
  (let ((keymap (intern
                 (concat (if (lithium-global-mode-p mode)
                             "local-"
                           "")
                         (symbol-name mode)
                         "-map")))
        (pre-exit (intern
                   (concat (symbol-name mode)
                           "-pre-exit-hook")))
        (post-exit (intern
                    (concat (symbol-name mode)
                            "-post-exit-hook"))))
    `(define-key ,keymap
                 (kbd ,key)
                 (lithium--handle-key ,mode
                                      ,action
                                      ,pre-exit
                                      ,post-exit
                                      ,should-exit))))

(defun lithium--define-key-from-spec (mode spec)
  "A helper to rewrite a keybinding SPEC into lithium keybinding for MODE."
  `(lithium-define-key ,mode ,@spec))

(defmacro lithium-define-keys (mode keyspec)
  "Bind keybindings in KEYSPEC in MODE."
  (declare (indent 1))
  `(progn
     ,@(mapcar (apply-partially #'lithium--define-key-from-spec mode) keyspec)))

(defun lithium--set-overriding-map (keymap)
  "Make the KEYMAP take precedence over all other keymaps.

Typically, lithium mode keymaps are enabled and disabled by the minor
mode that defines these maps.  But as the ordinary keymap priority of
minor modes is not sufficient for our purposes, we need to also
promote these keymaps to overriding terminal local upon minor mode
entry.

Yet, since keymap lookup consults these maps prior to any logic
related to minor modes, this map would now take precedence even in
cases where the minor mode is not active.  So we need to be careful to
demote maps in settings outside the jurisdiction of the minor mode,
such as in the minibuffer.

There can be only one such active overriding map, though many
different lithium modes may be active in different buffers and
globally.

This uses the internal `internal-push-keymap' utility, used by Hydra,
Transient, and also by Emacs's built-in `set-transient-map'."
  (internal-push-keymap keymap 'overriding-terminal-local-map)
  (setq lithium-promoted-map keymap))

(defun lithium--remove-overriding-map (keymap)
  "Remove the precedence of KEYMAP over all other keymaps.

This uses the internal `internal-pop-keymap' utility, used by Hydra,
Transient, and also by Emacs's built-in `set-transient-map'."
  (internal-pop-keymap keymap 'overriding-terminal-local-map)
  (setq lithium-promoted-map nil))

(defun lithium--already-overriding-map ()
  "The current global (Emacs) overriding keymap."
  (and overriding-terminal-local-map
       (cadr overriding-terminal-local-map)))

(defun lithium--suspend-overriding-map ()
  "Suspend the current overriding map."
  (when lithium-promoted-map
    (lithium--remove-overriding-map lithium-promoted-map)))

(defun lithium--suspend-overriding-map-advice (_keymap symbol &rest _)
  "Advise suspending a lithium map if another wants to override.

SYMBOL is the name of the keymap (argument to the advised function)."
  (when (eq symbol 'overriding-terminal-local-map)
    (lithium--suspend-overriding-map)))

(defun lithium-evaluate-overriding-map (&rest _)
  "Assess and promote the appropriate modal keymap (if any).

This operation is idempotent, so that if it is called redundantly in
separate hooks, it should not have any effect on these redundant
invocations."
  ;; first, demote any existing promoted lithium map
  (lithium--suspend-overriding-map)
  ;; then promote the appropriate one
  (let ((map-to-promote
         (cond ((minibufferp) ; do not promote any map in the minibuffer
                nil)
               ;; If there is already an overriding map (typically, a
               ;; foreign map, like Hydra or Transient), then do
               ;; nothing. We do not want to assume overriding status
               ;; in this case, as it could lead to undefined behavior.
               ((lithium--already-overriding-map)
                ;; But in rare cases (e.g., entering an edebug
                ;; session), our local tracking of the overriding map
                ;; could go out of sync with its actual state. It
                ;; appears that even though we suspend the map and
                ;; check that it's suspended locally, Emacs still
                ;; reinstates it after emerging from the edebug
                ;; session. As a result, we lose any record of that
                ;; promoted map and no longer control it.
                ;; To recover in this case, we check here if the
                ;; current lithium mode map happens to be the one
                ;; already promoted, and reintegrate it into our
                ;; records.
                (when (and (lithium-current-mode)
                           (eq (lithium-mode-metadata-map (lithium-current-mode))
                               (lithium--already-overriding-map)))
                  (lithium--already-overriding-map)))
               ((lithium-current-mode)
                (lithium-mode-metadata-map (lithium-current-mode)))
               ;; take no action otherwise
               (t nil))))
    (when map-to-promote
      (lithium--set-overriding-map map-to-promote))))

(defmacro lithium-define-mode (name
                               docstring
                               local-name
                               keymap-spec
                               &rest
                               body)
  "Define a lithium mode named NAME.

The entry hook is called after entering the mode, and the exit hook is
called after exiting the mode.  If there is a keybinding that exits,
the action is performed _before_ exiting the mode, and thus before
running the exit hook.

A mode may be exited intrinsically or extrinsically.  We consider a
command defined as \"exiting\" to result in an intrinsic exit, and an
external interrupt to exit the mode is considered extrinsic.  For
intrinsic exits, the lithium implementation is responsible for calling
the post-exit hook.  For extrinsic exits, the external agency is
responsible for doing it.

If the mode is global, then its LOCAL-NAME differs from the global
NAME.  In such cases, the local name is used as the name of the minor
mode itself, while the global name is used in exiting commands so that
we exit the mode globally rather than locally.

DOCSTRING and BODY are forwarded to `define-minor-mode'.  KEYMAP-SPEC
is parsed and then forwarded, as well."
  (declare (indent defun))
  (let ((keymap (intern (concat (symbol-name local-name) "-map"))))
    `(progn

       (define-minor-mode ,local-name
         ,docstring
         :keymap (make-sparse-keymap)

         (if ,local-name
             ;; push the mode onto the local mode stack
             ;; the local name is an implementation detail - we push
             ;; the mode as we know it, that is, its "name."
             (lithium-push-mode
              (make-lithium-mode-metadata :name ',name
                                          :map ,keymap))
           (lithium-pop-mode ',name))
         ,@body)
       (lithium-define-keys ,name ,keymap-spec))))

(defmacro lithium-define-global-mode (name
                                      docstring
                                      keymap-spec
                                      &rest
                                      body)
  "Define a global lithium mode named NAME.

This considers entry and exit to occur globally rather than in a
buffer-specific way.  That is, entering such a mode from any buffer
enters the mode in all buffers, and any entry hooks are run just once
at this time.  Likewise, exiting while in any buffer exits the mode in
all buffers, and the exit hooks are run just once.

This also defines `NAME-enter' and `NAME-exit' functions which accept
no arguments and enter and exit the mode, respectively.

DOCSTRING, KEYMAP-SPEC and BODY are forwarded to
`lithium-define-mode'.

Note that BODY is executed in each buffer during activation or
deactivation of the local mode rather than once for the global mode.
As the global mode isn't enabled until the local mode has been enabled
in all buffers, if you'd like to condition on the state of the mode in
your code in BODY, use the *local* name of the mode, i.e. `local-'
prefixed to NAME.  To execute code once after enabling or disabling the
global mode, use the post-entry and post-exit hooks.  This behavior is
perhaps a bit awkward.  It seems to support all possibilities for
executing code at certain times, but not necessarily in an intuitive
way.  This may be improved in the future and may entail backwards
incompatibility at that stage (please create an issue on the source
repo if you have a specific opinion about this!)."
  (declare (indent defun))
  (let ((pre-entry (intern (concat (symbol-name name) "-pre-entry-hook")))
        (post-entry (intern (concat (symbol-name name) "-post-entry-hook")))
        (pre-exit (intern (concat (symbol-name name) "-pre-exit-hook")))
        (post-exit (intern (concat (symbol-name name) "-post-exit-hook")))
        (local-name (intern (concat "local-" (symbol-name name))))
        (exit-mode (intern
                    (concat (symbol-name name)
                            "-exit")))
        (enter-mode (intern
                     (concat (symbol-name name)
                             "-enter"))))
    ;; Mark this mode as a global mode for the internal purposes of
    ;; determining the appropriate keymap to use in
    ;; `lithium-define-mode'. We do this even before the mode is
    ;; defined, as `lithium-define-mode' is called as part of mode
    ;; definition below.
    ;; As symbol properties set during macroexpansion seem to persist
    ;; to runtime, this property could also be used to support an
    ;; application-level predicate (i.e., `lithium-global-mode-p')
    (put name 'lithium-global t)

    `(progn

       (defvar ,pre-entry nil
         ,(concat "Pre-entry hook for " (symbol-name name) "."))
       (defvar ,post-entry nil
         ,(concat "Post-entry hook for " (symbol-name name) "."))
       (defvar ,pre-exit nil
         ,(concat "Pre-exit hook for " (symbol-name name) "."))
       (defvar ,post-exit nil
         ,(concat "Post-exit hook for " (symbol-name name) "."))

       (lithium-define-mode ,name
         ,docstring
         ,local-name
         ,keymap-spec
         ,@body)

       (define-globalized-minor-mode ,name ,local-name
         (lambda ()
           (unless (minibufferp)
             (,local-name 1)))
         (if ,name
             ;; we handle promotion and demotion of the keymap here
             ;; and in the wrapping local mode macro rather than in
             ;; the underlying minor mode macro since, as this is a
             ;; "globalized" minor mode, we invoke local minor mode
             ;; entry in every buffer, and that would result in the
             ;; map being promoted N times, and would prevent us from
             ;; detecting a real problem with any improper promoted
             ;; keymap state prior to promotion of the current keymap.
             (progn
               ;; ensure the new mode's keymap now takes precedence
               (lithium-evaluate-overriding-map)
               (run-hooks
                (quote ,post-entry)))
           ;; if there is a prior top mode, ensure that the precedence of its
           ;; keymap is restored
           (lithium-evaluate-overriding-map)))

       (defun ,enter-mode ()
         "Enter mode."
         (interactive)
         (lithium-enter-mode ',name))

       (defun ,exit-mode ()
         "Exit mode."
         (interactive)
         (lithium-exit-mode ',name)))))

(defmacro lithium-define-local-mode (name
                                     docstring
                                     keymap-spec
                                     &rest
                                     body)
  "Define a lithium mode named NAME that's local to a buffer.

This also defines `NAME-enter' and `NAME-exit' functions which accept
no arguments and enter and exit the mode, respectively.

DOCSTRING, KEYMAP-SPEC and BODY are forwarded to
`lithium-define-mode'."
  (declare (indent defun))
  (let ((pre-entry (intern (concat (symbol-name name) "-pre-entry-hook")))
        (post-entry (intern (concat (symbol-name name) "-post-entry-hook")))
        (pre-exit (intern (concat (symbol-name name) "-pre-exit-hook")))
        (post-exit (intern (concat (symbol-name name) "-post-exit-hook")))
        (exit-mode (intern
                    (concat (symbol-name name)
                            "-exit")))
        (enter-mode (intern
                     (concat (symbol-name name)
                             "-enter"))))
    `(progn

       (defvar ,pre-entry nil
         ,(concat "Pre-entry hook for " (symbol-name name) "."))
       (defvar ,post-entry nil
         ,(concat "Post-entry hook for " (symbol-name name) "."))
       (defvar ,pre-exit nil
         ,(concat "Pre-exit hook for " (symbol-name name) "."))
       (defvar ,post-exit nil
         ,(concat "Post-exit hook for " (symbol-name name) "."))

       (lithium-define-mode ,name
         ,docstring
         ,name
         ,keymap-spec
         ,@body
         ;; TODO: this symex is identical to the one in global
         ;; use a macro of some kind? `lithium-mode-toggle-syntax'
         (if ,name
             (progn
               ;; ensure the new mode's keymap now takes precedence
               (lithium-evaluate-overriding-map)
               (run-hooks
                (quote ,post-entry)))
           ;; if there is a prior top mode, ensure that the precedence of its
           ;; keymap is restored
           (lithium-evaluate-overriding-map)))

       (defun ,enter-mode ()
         "Enter mode."
         (interactive)
         (lithium-enter-mode ',name))

       (defun ,exit-mode ()
         "Exit mode."
         (interactive)
         (lithium-exit-mode ',name)))))

(defun lithium-global-mode-p (mode)
  "Is MODE a global mode?"
  (get mode 'lithium-global))

(defun lithium-local-mode-p (mode)
  "Is MODE a local mode?"
  (not
   (lithium-global-mode-p mode)))

(defun lithium-exit-mode (name)
  "Exit mode NAME."
  (when (eval name)
    (run-hooks
     (intern
      (concat (symbol-name name)
              "-pre-exit-hook")))
    (funcall
     (intern (symbol-name name))
     -1)
    (run-hooks
     (intern
      (concat (symbol-name name)
              "-post-exit-hook")))))

(defun lithium-enter-mode (name)
  "Enter mode NAME."
  (unless (eval name)
    (run-hooks
     (intern
      (concat (symbol-name name)
              "-pre-entry-hook")))
    (funcall
     (intern (symbol-name name)))))

(defun lithium-initialize ()
  "Initialize any global state necessary for Lithium mode operation."
  (add-hook 'window-buffer-change-functions
            #'lithium-evaluate-overriding-map)
  (add-hook 'window-selection-change-functions
            #'lithium-evaluate-overriding-map)
  (add-hook 'window-configuration-change-hook
            #'lithium-evaluate-overriding-map)
  (advice-add #'internal-push-keymap :before #'lithium--suspend-overriding-map-advice))

(defun lithium-disable ()
  "Remove any global state defined by Lithium."
  (remove-hook 'window-buffer-change-functions
               #'lithium-evaluate-overriding-map)
  (remove-hook 'window-selection-change-functions
               #'lithium-evaluate-overriding-map)
  (remove-hook 'window-configuration-change-hook
               #'lithium-evaluate-overriding-map)
  (advice-remove #'internal-push-keymap #'lithium--suspend-overriding-map-advice))

;;;###autoload
(define-minor-mode lithium-mode
  "Minor mode for managing necessary global state for Lithium modes.

The only purpose for this for the moment is to register hooks that
enable and disable overriding keymaps for lithium modes in certain
cases, such as entry into the minibuffer.

There are no keybindings associated with this minor mode -- it is not
itself a \"lithium mode\"."
  :lighter " lithium"
  :global t
  :group 'lithium
  (if lithium-mode
      (lithium-initialize)
    (lithium-disable)))

(provide 'lithium)
;;; lithium.el ends here
