;;; lithium.el --- Lightweight modal interfaces -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/lithium
;; Version: 0.0
;; Package-Requires: ((emacs "24.4"))
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

(defvar lithium-overriding-map nil
  "The overriding terminal local map currently in effect.

There can be only one, and it's put in place upon lithium mode entry.
As there are cases where minor mode controls are not in effect, and
terminal local is a global concern, we need to keep track of this
overriding map globally so that it can be suspended, if need be,
outside the jurisdiction of the minor mode.")

;; TODO: should we define a mode struct that is passed around internally,
;; instead of interning global symbol names to discover hooks?
(defun lithium--define-key (keyspec keymap mode)
  "Helper to define an individual key according to spec.

Sample invocation:
 (lithium--define-key (list \"a\" \\='some-function t)
                      some-mode-map
                      \\='some-mode)

Parse the KEYSPEC to define the key in KEYMAP for MODE.

KEYSPEC is expected to be (key action [exit]). If `exit' is missing,
then it's an ordinary binding of key to action, but if exit is present
and set to true, then also exit the MODE after performing the action."
  (let ((key (car keyspec))
        (action (or (cadr keyspec)
                    (lambda ()
                      (interactive))))
        (should-exit (and (> (length keyspec) 2)
                          (caddr keyspec)))
        (pre-exit (intern
                   (concat (symbol-name mode)
                           "-pre-exit-hook")))
        (post-exit (intern
                    (concat (symbol-name mode)
                            "-post-exit-hook"))))
    (if should-exit
        (define-key keymap
          (kbd key)
          (lambda ()
            (interactive)
            ;; exit first so that the modal UI doesn't get
            ;; in the way of whatever this command is
            ;; TODO: now that modes are "globalized" and explicitly
            ;; disabled in the minibuffer, can we just exit after
            ;; running the command?
            (run-hooks pre-exit)
            (funcall mode -1)
            ;; do the action
            (condition-case err
                (call-interactively action)
              ;; if we interrupt execution via `C-g', or if the
              ;; command encounters an error during execution,
              ;; we still want to run post-exit hooks to ensure
              ;; that we leave things in a clean state
              ((quit error)
               (progn (run-hooks post-exit)
                      ;; re-raise the interrupt
                      (signal (car err) (cdr err)))))
            ;; run post-exit hook "intrinsically"
            (run-hooks post-exit)))
      (define-key keymap
        (kbd key)
        action))))

(defmacro lithium-keymap (spec mode)
  "Specify a keymap for the MODE.

SPEC is the set of keybinding specifications."
  `(let ((keymap (make-sparse-keymap)))
     (dolist (keyspec (quote ,spec))
       (lithium--define-key keyspec keymap ,mode))
     keymap))

(defun lithium--push-overriding-map (keymap)
  "Make the KEYMAP take precedence over all other keymaps.

This uses the internal `internal-push-keymap' utility, used by Hydra,
Transient, and also by Emacs's built-in `set-transient-map'."
  (internal-push-keymap keymap 'overriding-terminal-local-map))

(defun lithium--pop-overriding-map (keymap)
  "Remove the precedence of KEYMAP over all other keymaps.

This uses the internal `internal-pop-keymap' utility, used by Hydra,
Transient, and also by Emacs's built-in `set-transient-map'."
  (internal-pop-keymap keymap 'overriding-terminal-local-map))

(defun lithium-suspend-overriding-map ()
  "Suspend the current overriding terminal local map.

Typically, lithium mode keymaps are enabled and disabled by the minor
mode that defines these maps.  But the ordinary keymap priority of
minor modes is not sufficient for our purposes, and we need to also
promote these keymaps to overriding terminal local upon minor mode
entry.  Yet, since keymap lookup consults these maps prior to any logic
related to minor modes, it also means that this map now takes
precedence even in cases where the minor mode is not active.  In such
cases, we need to explicitly suspend the keymap from terminal local,
and reinstate it upon re-entering a context where the usual minor mode
controls are sufficient."
  (when lithium-overriding-map
    (lithium--pop-overriding-map lithium-overriding-map)))

(defun lithium-reinstate-overriding-map ()
  "Reinstate a suspended overriding terminal local map."
  (when lithium-overriding-map
    (lithium--push-overriding-map lithium-overriding-map)))

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
external interrup to exit the mode is considered extrinsic.  For
intrinsic exits, the lithium implementation is responsible for calling
the post-exit hook.  For extrinsic exits, the external agency is
responsible for doing it.

If the mode is global, then its LOCAL-NAME may differ from the global
name. The global name is used in exiting commands so that we exit the
mode globally rather than locally.  The local name is used as the name
of the minor mode itself.  DOCSTRING and BODY are forwarded to
`define-minor-mode'.  KEYMAP-SPEC is parsed and then forwarded, as
well."
  (declare (indent defun))
  `(progn

     (define-minor-mode ,local-name
       ,docstring
       :keymap (lithium-keymap ,keymap-spec ',name)
       ;; TODO: consider making local modes promote to overriding-local-map
       ;; and global modes, to overriding-terminal-local-map, so that
       ;; local modes can remain enabled while global modes are enabled
       ;; and so that the latter will take precedence.
       ,@body)))

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
`lithium-define-mode'."
  (declare (indent defun))
  (let ((pre-entry (intern (concat (symbol-name name) "-pre-entry-hook")))
        (post-entry (intern (concat (symbol-name name) "-post-entry-hook")))
        (pre-exit (intern (concat (symbol-name name) "-pre-exit-hook")))
        (post-exit (intern (concat (symbol-name name) "-post-exit-hook")))
        (local-name (intern (concat "local-" (symbol-name name))))
        ;; note the keymap is part of the local rather than global mode
        (keymap (intern (concat "local-" (symbol-name name) "-map")))
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
               (when lithium-overriding-map
                 (error "%s is already an overriding map!"
                        lithium-overriding-map))
               (lithium--push-overriding-map ,keymap)
               ;; register this mode's keymap as having been promoted to terminal local
               (setq lithium-overriding-map ,keymap)
               (run-hooks
                (quote ,post-entry)))
           (lithium--pop-overriding-map ,keymap)
           ;; unregister this as a promoted overriding map
           (setq lithium-overriding-map nil)))

       (defun ,enter-mode ()
         "Enter mode."
         (lithium-enter-mode ',name))

       (defun ,exit-mode ()
         "Exit mode."
         (lithium-exit-mode ',name))

       ;; mark this mode as a global mode
       ;; for use in application-level predicates
       (put ',name 'lithium-global t))))

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
        (keymap (intern (concat (symbol-name name) "-map")))
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
               (when lithium-overriding-map
                 (error "%s is already an overriding map!"
                        lithium-overriding-map))
               (lithium--push-overriding-map ,keymap)
               ;; register this mode's keymap as having been promoted to terminal local
               (setq lithium-overriding-map ,keymap)
               (run-hooks
                (quote ,post-entry)))
           (lithium--pop-overriding-map ,keymap)
           ;; unregister this as a promoted overriding map
           (setq lithium-overriding-map nil)))

       (defun ,enter-mode ()
         "Enter mode."
         (lithium-enter-mode ',name))

       (defun ,exit-mode ()
         "Exit mode."
         (lithium-exit-mode ',name))

       ;; mark this mode as a local mode - not technically needed
       ;; since properties default to nil, but for good measure
       (put ',name 'lithium-global nil))))

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
  (add-hook 'minibuffer-setup-hook
            #'lithium-suspend-overriding-map)
  (add-hook 'minibuffer-exit-hook
            #'lithium-reinstate-overriding-map))

(defun lithium-disable ()
  "Remove any global state defined by Lithium."
  (remove-hook 'minibuffer-setup-hook
               #'lithium-suspend-overriding-map)
  (remove-hook 'minibuffer-exit-hook
               #'lithium-reinstate-overriding-map))

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
