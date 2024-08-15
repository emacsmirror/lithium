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
        (exit (and (> (length keyspec) 2)
                   (caddr keyspec))))
    (if exit
        (define-key keymap
          (kbd key)
          (lambda ()
            (interactive)
            ;; exit first so that the modal UI doesn't get
            ;; in the way of whatever this command is
            ;; TODO: now that modes are "globalized" and explicitly
            ;; disabled in the minibuffer, can we just exit after
            ;; running the command?
            (run-hooks
             (intern
              (concat (symbol-name mode)
                      "-pre-exit-hook")))
            (funcall mode -1)
            ;; do the action
            (condition-case err
                (call-interactively action)
              ;; if we interrupt execution via `C-g', or if the
              ;; command encounters an error during execution,
              ;; we still want to run post-exit hooks to ensure
              ;; that we leave things in a clean state
              ((quit error)
               (progn (run-hooks
                       (intern
                        (concat (symbol-name mode)
                                "-post-exit-hook")))
                      ;; re-raise the interrupt
                      (signal (car err) (cdr err)))))
            ;; run post-exit hook "intrinsically"
            (run-hooks
             (intern
              (concat (symbol-name mode)
                      "-post-exit-hook")))))
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
  `(progn

     (define-minor-mode ,local-name
       ,docstring
       :keymap (lithium-keymap ,keymap-spec ',name)
       ,@body)

     ;; Based on: https://stackoverflow.com/a/5340797
     (defun ,(intern (concat "lithium-promote-"
                             (symbol-name local-name)
                             "-keymap"))
         (_file)
       "Ensure that these keybindings take precedence over other minor modes.

This may still be overridden by other minor mode keymaps that employ
the same approach, if their functions are run after this one, but this
should be rare.

Called via the `after-load-functions' special hook."
       (unless (eq (caar minor-mode-map-alist) ',local-name)
         (let ((this-mode (assq ',local-name minor-mode-map-alist)))
           (assq-delete-all ',local-name minor-mode-map-alist)
           (add-to-list 'minor-mode-map-alist this-mode))))

     ;; Minor mode keymaps take precedence based on their placement in
     ;; `minor-mode-map-alist'. This placement corresponds to the order
     ;; in which modules are loaded. So, to ensure that Lithium mode maps
     ;; take precedence, re-promote them to the front of the alist
     ;; after each module is loaded.
     ;; Note that evil keybindings still somehow manage to take precedence,
     ;; so if Evil is in use, Normal state should be deactivated via
     ;; a post-entry hook. That seems outside the scope of Lithium, however.
     (add-hook 'after-load-functions
               ',(intern (concat "lithium-promote-"
                                 (symbol-name local-name)
                                 "-keymap")))))

(defmacro lithium-define-global-mode (name
                                      docstring
                                      keymap-spec
                                      &rest
                                      body)
  "Define a global lithium mode."
  (declare (indent defun))
  `(progn

     (defvar ,(intern (concat (symbol-name name) "-pre-entry-hook")) nil
       ,(concat "Pre-entry hook for" (symbol-name name) "."))
     (defvar ,(intern (concat (symbol-name name) "-post-entry-hook")) nil
       ,(concat "Post-entry hook for" (symbol-name name) "."))
     (defvar ,(intern (concat (symbol-name name) "-pre-exit-hook")) nil
       ,(concat "Pre-exit hook for" (symbol-name name) "."))
     (defvar ,(intern (concat (symbol-name name) "-post-exit-hook")) nil
       ,(concat "Post-exit hook for" (symbol-name name) "."))

     (lithium-define-mode ,name
       ,docstring
       ,(intern (concat "local-" (symbol-name name)))
       ,keymap-spec
       ,@body)

     (define-globalized-minor-mode ,name ,(intern (concat "local-" (symbol-name name)))
       (lambda ()
         (unless (minibufferp)
           (,(intern (concat "local-" (symbol-name name))) 1)))
       (when ,name
         (run-hooks
          (quote ,(intern
                   (concat (symbol-name name)
                           "-post-entry-hook"))))))))

(defun lithium-exit-mode (name)
  "Exit mode NAME."
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
            "-post-exit-hook"))))

(defun lithium-enter-mode (name)
  "Enter mode NAME."
  (run-hooks
   (intern
    (concat (symbol-name name)
            "-pre-entry-hook")))
  (funcall
   (intern (symbol-name name))))


(provide 'lithium)
;;; lithium.el ends here
