;;; lithium.el --- Lightweight modal interfaces -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/lithium
;; Version: 0.0
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

;; TODO: should we define a mode struct that is passed around internally,
;; instead of interning global symbol names to discover hooks?
(defun lithium--define-key (keyspec keymap mode)
  "Helper to define an individual key according to spec.

Sample invocation:
 (lithium--define-key (list \"a\" 'some-function t)
                      some-mode-map
                      'some-mode)

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

(defmacro lithium-define-mode (name
                               docstring
                               local-name
                               keymap-spec
                               &rest
                               body)
  "Define a lithium mode.

The entry hook is called after entering the mode, and the exit hook is
called after exiting the mode. If there is a keybinding that exits,
the action is performed _before_ exiting the mode, and thus before
running the exit hook.

A mode may be exited intrinsically or extrinsically. We consider a
command defined as \"exiting\" to result in an intrinsic exit, and an
external interrup to exit the mode is considered extrinsic. For
intrinsic exits, the lithium implementation is responsible for calling
the post-exit hook. For extrinsic exits, the external agency is
responsible for doing it."
  (declare (indent defun))
  (let* ((keymap (intern
                  (concat
                   (symbol-name local-name)
                   "-map"))))
   `(progn

      (define-minor-mode ,local-name
        ,docstring
        :keymap (lithium-keymap ,keymap-spec ',name)
        ;; TODO: consider making local modes promote to overriding-local-map
        ;; and global modes, to overriding-terminal-local-map, so that
        ;; local modes can remain enabled while global modes are enabled
        ;; and so that the latter will take precedence.
        (if ,local-name
            (lithium--push-overriding-map ,keymap)
          (lithium--pop-overriding-map ,keymap))
        ,@body))))

(defmacro lithium-define-global-mode (name
                                      docstring
                                      keymap-spec
                                      &rest
                                      body)
  "Define a global lithium mode.

This considers entry and exit to occur globally rather than in a
buffer-specific way. That is, entering such a mode from any buffer
enters the mode in all buffers, and any entry hooks are run just once
at this time. Likewise, exiting while in any buffer exits the mode in
all buffers, and the exit hooks are run just once."
  (declare (indent defun))
  (let ((pre-entry (intern (concat (symbol-name name) "-pre-entry-hook")))
        (post-entry (intern (concat (symbol-name name) "-post-entry-hook")))
        (pre-exit (intern (concat (symbol-name name) "-pre-exit-hook")))
        (post-exit (intern (concat (symbol-name name) "-post-exit-hook")))
        (local-name (intern (concat "local-" (symbol-name name))))
        (disable-mode (intern
                       (concat "lithium-disable-"
                               (symbol-name name)))))
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
         (when ,name
           (run-hooks
            (quote ,post-entry))))

       (defun ,disable-mode ()
         "Disable this mode."
         (lithium-exit-mode ',name))

       (add-hook 'minibuffer-setup-hook
                 #',disable-mode)

       ;; mark this mode as a global mode
       ;; for use in application-level predicates
       (put ',name 'lithium-global t))))

(defmacro lithium-define-local-mode (name
                                     docstring
                                     keymap-spec
                                     &rest
                                     body)
  "Define a lithium mode that's local to a buffer."
  (declare (indent defun))
  (let ((pre-entry (intern (concat (symbol-name name) "-pre-entry-hook")))
        (post-entry (intern (concat (symbol-name name) "-post-entry-hook")))
        (pre-exit (intern (concat (symbol-name name) "-pre-exit-hook")))
        (post-exit (intern (concat (symbol-name name) "-post-exit-hook")))
        (local-name (intern (concat "local-" (symbol-name name)))))
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
         (when ,name
           (run-hooks
            (quote ,post-entry))))

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


(provide 'lithium)
;;; lithium.el ends here
