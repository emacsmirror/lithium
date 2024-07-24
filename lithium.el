;;; lithium.el --- Lightweight modal interfaces -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/lithium

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
            (funcall action)
            (funcall mode -1)))
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
                               keymap
                               &rest
                               body)
  "Define a lithium mode."
  (declare (indent defun))
  `(progn

     (defvar ,(intern (concat (symbol-name name) "-entry-hook")) nil
       ,(concat "Entry hook for" (symbol-name name) "."))
     (defvar ,(intern (concat (symbol-name name) "-exit-hook")) nil
       ,(concat "Exit hook for" (symbol-name name) "."))

     (define-minor-mode ,name
       ,docstring
       :global t
       :keymap (lithium-keymap ,keymap ',name)
       ,@body
       (if ,name
           (run-hooks
            (quote ,(intern
                     (concat (symbol-name name)
                             "-entry-hook"))))
         (run-hooks
          (quote ,(intern
                   (concat (symbol-name name)
                           "-exit-hook"))))))

     (add-to-list 'emulation-mode-map-alists
                  (let ((keymap ,(intern
                                  (concat
                                   (symbol-name name)
                                   "-map"))))
                    (list (cons (quote ,name) keymap))))))
