;;; keyswap-mode.el --- swap bindings between two keys

;; Copyright (C) 2016 Matthew Malcomson

;;; Licence:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Matthew Malcomson <hardenedapple@gmail.com>
;; Maintainer: Matthew Malcomson <hardenedapple@gmail.com>
;; Created: 23 Jul 2016
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Version: 20160722.2100
;; URL: http://github.com/hardenedapple/keyswap-mode
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; keyswap-mode is a minor mode that allows swapping the commands of two keys.
;; It comes with a default set of keys to swap of the number keys and their
;; shifted counterparts along with the '-' and '_' key.
;; This is generally useful in programming languages where symbols are more
;; often used than numbers.
;;
;; To use keyswap-mode, make sure this file is in the Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require keyswap-mode:
;;   (require 'keyswap-mode)

;; To toggle between swapped and not-swapped sets of keys, use the command
;; (keyswap-mode) or M-x keyswap-mode
;;
;; Keys are swapped on a major-mode basis.
;; If you change the swapped keys in one buffer these changes are propagated to
;; all other major modes.
;;
;; The set of keys to swap is stored in the buffer local `keyswap-pairs'
;; variable.
;; This variable is an alist of vectors ready for passing to `define-key' that
;; should be swapped when `keyswap-mode' is turned on.
;; Its default is to swap all number keys and their shifted alternatives, along
;; with the - and _ keys.
;;
;; In order to change the current swapped keys one should modify this list with
;; `keyswap-add-pairs' or `keyswap-remove-pairs', and then run
;; `keyswap-update-keys' like so
;; (keyswap-add-pairs ?\: ?\;)
;; (keyswap-remove-pairs ?\- ?\_)
;; (keyswap-update-keys)
;;
;; Without running `keyswap-update-keys' the changes in `keyswap-pairs' will not
;; be propagated into the action of `keyswap-mode'.
;;
;; There are some provided hooks for common modifications of `keyswap-pairs'
;; that modify the pairs to swap and call `keyswap-update-keys' accordingly.
;; These are `keyswap-include-braces' to swap [ and ] with { and },
;; `keyswap-include-quotes' to swap ' with ", `keyswap-tac-underscore-exception'
;; to *not* swap - and _, and finally `keyswap-colon-semicolon' to swap : and ;.
;;
;; It is recommended to turn on `keyswap-mode' by default in programming buffers
;; with
;; (add-hook 'prog-mode-hook 'keyswap-mode)
;;
;; and then add modifications for each major-mode you desire accordingly, e.g.
;;
;; (with-eval-after-load 'cc-vars
;;   (add-hook 'c-mode-common-hook 'keyswap-include-quotes))
;;
;; (with-eval-after-load 'lisp-mode
;;   (add-hook 'emacs-lisp-mode-hook 'keyswap-tac-underscore-exception)
;;   (add-hook 'lisp-mode-hook 'keyswap-tac-underscore-exception))
;;
;; To toggle between having keys swapped and not, just turn on and off
;; `keyswap-mode'.



;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst keyswap-command-docstring "CHAR COMMAND WRAPPER ")

(defun keyswap-equivalent-binding (key)
  "NOTE -- this function is broken but useful.

At the moment I can't find a way to fix it, but I'm using it with
all its kludges anyway.

Finds the command that is run when `key' is pressed.

If this commands `documentation' starts with
`keyswap-command-docstring' assume it is a wrapper previously
created by this function.
Then return the result of (command nil t).

Otherwise create a `lambda' function that runs that command under
the false environment where `last-command-event' is KEY"
  (let ((current-binding (key-binding key)))
    (if (and (listp current-binding)
             (string-prefix-p keyswap-command-docstring
                              (documentation current-binding)))
        (funcall current-binding nil t)
      ;; `lexical-let' uses `cl--function-convert' which wraps any lambda functions
      ;; in another lambda function, passing in arguments to pretend that we're
      ;; using lexical variables.
      ;; `cl--function-convert' uses a special case for docstrings and (interactive)
      ;; forms to bring them outside the `lambda' form being enclosed.
      ;; This special case doesn't work unless the docstring is recogniseable as a
      ;; string at compile time.
      ;; Hence I'm separating creating the form and evaluating it into two steps
      ;; so that I can create the form with a string literal for
      ;; `cl--function-convert' to recognise.
      ;; That's the reason for the `eval' layer of indirection.
      (let ((docstring (format (concat keyswap-command-docstring "\"%c\"")
                               (aref key 0))))
        (eval
         `(lexical-let ((current-key (aref key 0)) (old-binding current-binding))
            (lambda (&optional arg return-command)
              ,docstring
              (interactive "p")
              (if return-command
                  old-binding
                (let ((last-command-event current-key))
                  (call-interactively old-binding))))))))))

(defun keyswap-swap-these (left-key right-key keymap)
  "Puts alternate bindings of LEFT-KEY and RIGHT-KEY into KEYMAP.

LEFT-KEY and RIGHT-KEY should be two objects valid in a call to
`key-binding'.  Makes a new binding in KEYMAP or the local
keymap, as used by `local-set-key'.

If a mapping has a normal function, we bind the other key to a
`lambda' function that calls the original with a masked
`last-command-event' pretending to be the first key.

These `lambda' functions are marked by their documentation
string (*very* hacky -- I know), and if this documentation string
is noticed, they are called with arguments so they return their
wrapped function.

If KEYMAP is defined, binds keys in that map, else uses
`current-local-map'"
  (let ((left-function (keyswap-equivalent-binding left-key))
        (right-function (keyswap-equivalent-binding right-key)))
    (define-key keymap left-key right-function)
    (define-key keymap right-key left-function)))

;;;###autoload
(defvar-local keyswap-pairs
  (mapcar (lambda (pair) (cons (vector (car pair)) (vector (cdr pair))))
          (list '(?1 . ?!) '(?2 . ?@) '(?3 . ?#) '(?4 . ?$) '(?5 . ?%)
                '(?6 . ?^) '(?7 . ?&) '(?8 . ?*) '(?9 . ?\() '(?0 . ?\))
                '(?- . ?_)))
  "Pairs of characters to swap in `keyswap-mode'.")

(defun keyswap-swapped-keymap ()
  "Create a swapped keymap for this buffer.
Take the keys currently active, and create a keymap that takes
inverts the bindings of those key pairs in `keyswap-pairs'.
Returns the resulting keymap with these bindings, but doesn't do
anything other than create and return the keymap."
  (let ((return-map (make-sparse-keymap)))
    (dolist (key-pair keyswap-pairs return-map)
      (keyswap-swap-these (car key-pair) (cdr key-pair) return-map))))

(defun keyswap-update-keys ()
  "Update the buffer-local keymap currently used for `keyswap-mode'"
  (interactive)
  (when (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (let ((currently-on keyswap-mode))
      (when currently-on (keyswap-mode 0))
      (setf (cdr (assoc 'keyswap-mode minor-mode-overriding-map-alist))
            (keyswap-swapped-keymap))
      (when currently-on (keyswap-mode t)))))

;;;###autoload
(define-minor-mode keyswap-mode
  "Minor mode for programming where number keys are swapped with their shifted
counterparts.

This effectively makes the keyboard a \"programmers\" version of the keyboard.

In order to to have a different set of keys swapped for each
buffer, I abuse `minor-mode-overriding-map-alist' and never
actually have a minor mode map in the main `minor-mode-map-alist'
variable.

When this mode is activated, it first checks whether the current
buffer already has a local overriding keymap in
`minor-mode-overriding-map-alist', and if so does nothing but
activate that keymap.

If there is no relevant keymap in
`minor-mode-overriding-map-alist' it finds the mappings in the
`current-buffer' that relate to the keys to be swapped in
`keyswap-pairs', creates a keymap that has functionaly swapped
these keys, and stores that as the keymap in `minor-mode-overriding-map-alist'
to be used in all future invocations of this minor-mode.

When using this minor-mode along with others there are a few things to watch out
for.
First off, if this minor mode is activated before others that change the current
"
  nil
  " keyswap"
  nil
  ;; Body is executed every time the mode is toggled
  ;; If keyswap-mode is not in the `minor-mode-overriding-map-alist' variable,
  ;; then create a new map with `keyswap-swapped-keymap', and add that to the list of
  ;; `minor-mode-overriding-map-alist'.
  (unless (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (push (cons 'keyswap-mode (keyswap-swapped-keymap))
          minor-mode-overriding-map-alist)))


(defun keyswap-act-on-pairs (action-fn keyswaps)
  "Call ACTION-FN on successive pairs of KEYSWAPS."
  (cl-loop for remaining-keyswaps on keyswaps by #'cddr
        do (let ((left-key (car remaining-keyswaps))
                 (right-key (cadr remaining-keyswaps)))
             (if (and left-key right-key)
                 (funcall action-fn (cons (vector left-key)
                                          (vector right-key)))))))

(defun keyswap-add-pairs (&rest keyswaps)
  "Add KEYSWAPS into `keyswap-pairs'."
  (keyswap-act-on-pairs
   (lambda (pair)
     (setq-local
      keyswap-pairs
      (cl-adjoin pair keyswap-pairs
                 :test #'(lambda (left right)
                           (or (equal left right)
                               (equal (cons (cdr left) (car left)) right))))))
   keyswaps))

(defun keyswap-remove-pairs (&rest keyswaps)
  "Remove KEYSWAPS from `keyswap-pairs'."
  (keyswap-act-on-pairs
   (lambda (pair)
     (setq-local keyswap-pairs
                 (remove (cons (cdr pair) (car pair))
                         (remove pair keyswap-pairs))))
   keyswaps))

(defun keyswap-include-braces ()
  "Hook to make `keyswap-mode' include {,[, and },]."
  (keyswap-add-pairs ?\[ ?\{   ?\] ?\} )
  (keyswap-update-keys))

(defun keyswap-include-quotes ()
  "Hook to make `keyswap-mode' include \" and '."
  (keyswap-add-pairs ?\' ?\")
  (keyswap-update-keys))

(defun keyswap-tac-underscore-exception ()
  "Hook to make `keyswap-mode' ignore - and _."
  (keyswap-remove-pairs ?- ?_)
  (keyswap-update-keys))

(defun keyswap-colon-semicolon ()
  "Hook to make `keyswap-mode' swap : and ;."
  (keyswap-add-pairs ?: ?\;)
  (keyswap-update-keys))

(provide 'keyswap-mode)

;;; keyswap-mode.el ends here
