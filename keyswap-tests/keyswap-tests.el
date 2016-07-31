;;; Testing keyswap.el
;;;
;;; emacs -batch -l ert -L /home/matthew/.emacs.d/my-packages/keyswap-mode/ -l keyswap-tests.el -f ert-run-tests-batch-and-exit

(require 'keyswap)

;; Check that `keyswap-equivalent-binding' creates something that has the
;; following properties.
;;
;; 1) Can't be affected by bindings in the outside environment.
;;
;; ELISP> (setq one-binding (key-binding [?1]))
;; (lambda
;;   (&optional arg return-command)
;;   "CHAR COMMAND WRAPPER \"!\""
;;   (interactive "p")
;;   (if return-command --old-binding--
;;     (let
;;         ((last-command-event 33))
;;       (call-interactively --old-binding--))))
;;
;; ELISP> (let ((--old-binding-- 100))
;;          (funcall one-binding 0 t))
;; self-insert-command
;; ELISP>
;;
;; 2) Returns something that can be called successfully.
;;
;; ELISP> (let ((last-command-event ?e))
;;          (call-interactively (funcall one-binding 0 t) 0))
;; nil
;; ELISP> e
;;
;; 3) Can be called interactively itself.
;;
;; ELISP> (let ((last-command-event ?e))
;;          (call-interactively one-binding 0))
;; nil
;; ELISP> !
;;
;; 4) Doesn't shadow variables called inside.
;;
;; ELISP> (define-key (current-local-map)
;;          [?\}] (lambda (&optional arg) (interactive)
;;                  (let ((--old-binding-- 100))
;;                    (message "%s" --old-binding--))))
;; (lambda
;;   (&optional arg)
;;   (interactive)
;;   (let
;;       ((--old-binding-- 100))
;;     (message "%s" --old-binding--)))
;;
;; ELISP> (keyswap-equivalent-binding [?\}])
;; (lambda
;;   (&optional arg return-command)
;;   "CHAR COMMAND WRAPPER \"}\""
;;   (interactive "p")
;;   (if return-command --old-binding--
;;     (let
;;         ((last-command-event 125))
;;       (call-interactively --old-binding--))))
;;
;; ELISP> (setq shadow-test *)
;; (lambda
;;   (&optional arg return-command)
;;   "CHAR COMMAND WRAPPER \"}\""
;;   (interactive "p")
;;   (if return-command --old-binding--
;;     (let
;;         ((last-command-event 125))
;;       (call-interactively --old-binding--))))
;;
;; ELISP> (funcall shadow-test 0)
;; "100"
;; ELISP>
;;

(defmacro keyswap--with-temp-list (&rest body)
  "Create a temporary list of known values, bind it to the symbol
  `keyswap-pairs', execute BODY in that environment and return
  the value of `keyswap-pairs'."
  `(let ((keyswap-pairs
          (mapcar (lambda (pair) (cons (vector (car pair)) (vector (cdr pair))))
                  (list '(?1 . ?!) '(?2 . ?@) '(?3 . ?#) '(?4 . ?$) '(?5 . ?%)
                        '(?6 . ?^) '(?7 . ?&) '(?8 . ?*) '(?9 . ?\() '(?0 . ?\))
                        '(?- . ?_)))))
     ,@body))

(defmacro keyswap--test-with-temp-list (&rest body)
  `(keyswap--with-temp-list ,@body keyswap-pairs))

(ert-deftest keyswap-test-remove-pairs ()
  "Ensures that `keyswap-remove-pairs' does what it advertises."
  (keyswap--test-with-temp-list
   ;; * Removes a pair
   (should (equal (keyswap--test-with-temp-list (keyswap-remove-pairs ?1 ?!))
                  (remove '([?1] . [?!]) keyswap-pairs)))
   ;; * Removes any pair in the list
   (should (equal (keyswap--test-with-temp-list (keyswap-remove-pairs ?3 ?#))
                  (remove '([?3] . [?#]) keyswap-pairs)))
   ;; * Does nothing if given pair isn't in `keyswap-pairs'
   (should (equal (keyswap--test-with-temp-list (keyswap-remove-pairs ?1 ?a))
                  keyswap-pairs))
   ;; * Takes multiple arguments and removes them all
   (should (equal (keyswap--test-with-temp-list
                   (keyswap-remove-pairs ?1 ?! ?@ ?2 ?* ?8 ?a ?b))
                  (remove-if
                   (lambda (arg)
                     (member
                      arg
                      (list '([?1] . [?!]) '([?2] . [?@]) '([?8] . [?*]))))
                   keyswap-pairs)))
   ;; * Order of pair doesn't matter
   (should (equal (keyswap--test-with-temp-list (keyswap-remove-pairs ?! ?1))
                  (remove '([?1] . [?!]) keyswap-pairs)))))

(ert-deftest keyswap-test-add-pairs ()
  "Ensures that `keyswap-remove-pairs' does what it advertises."
  (keyswap--test-with-temp-list
   ;; * Adds a pair into the list
   (should (equal (keyswap--test-with-temp-list (keyswap-add-pairs ?a ?b))
                  (cons '([?a] . [?b]) keyswap-pairs)))
   ;; * Does nothing if the given pair is already in `keyswap-pairs'
   (should (equal (keyswap--test-with-temp-list (keyswap-add-pairs ?1 ?!))
                  keyswap-pairs))
   ;; * TODO -- what should happen if one element in the pair is already in
   ;; `keyswap-pairs'? Should I just let whoever is doing that do it and assume
   ;; they can figure out what will happen?

   ;; * Takes multiple arguments and adds them all
   (should (equal (keyswap--test-with-temp-list
                   (keyswap-add-pairs ?a ?b ?1 ?! ?c ?d))
                  (append (list '([?c] . [?d]) '([?a] . [?b])) keyswap-pairs)))))

(ert-deftest keyswap-test-equivalent-command ()
  "Check `keyswap--equivalent-command' returns a function that
  may be used as the equivalent of the binding it was currently
  on."
  (let* ((test-command (lambda () (interactive)
                         (cons last-command-event (this-command-keys-vector))))
         (wrapped-command (keyswap--equivalent-command [?a] test-command)))
    ;; * Return value calls COMMAND with `last-command-event' bound to KEY.
    (should (equal (car (funcall wrapped-command)) ?a))
    ;; * Return value has secondary argument to access original COMMAND
    (should (equal (funcall wrapped-command nil t) test-command))
    ;; * Return value has `keyswap-command-docstring' as `documentation' prefix
    (let ((wrapped-docstring (documentation wrapped-command)))
      (should (string-prefix-p keyswap-command-docstring wrapped-docstring))
     ;; * Return command mentions the key it's wrapping in its `documentation'
     (should (string-match-p "\"a\"" wrapped-docstring))
     ;; * Return command describes the command it's wrapping in its `documentation'
     (should (string-match-p (format "%S" test-command) wrapped-docstring))
     ;; *Return command describes the command vector it wraps with in its
     ;; `documentation'
     (should (string-match-p (format "%S" [?a]) wrapped-docstring)))))

(ert-deftest keyswap-test-equivalent-binding ()
  "Check that `keyswap-equivalent-binding' makes the correct decision about what
to do depending on the state of the keymap."
  (let ((test-map `(keymap (?a . self-insert-command)
                           (?b . ,(keyswap--equivalent-command
                                   [?c] 'self-insert-command)))))
    ;; * By default searches the current emacs envirenmont
    (should (equal (keyswap-equivalent-binding [?a])
                   (keyswap--equivalent-command [?a] (key-binding [?a]))))
    ;; * Can provide a keymap to search in
    (should (equal (keyswap-equivalent-binding [?a] test-map)
                   (keyswap--equivalent-command [?a] (lookup-key test-map [?a]))))

    ;; * Calls `keyswap--equivalent-command' when asked about binding of
    ;; standard mappings
    (should (equal (keyswap-equivalent-binding [?a] test-map)
                   (keyswap--equivalent-command [?a] 'self-insert-command)))

    ;; * Unwraps `keyswap--equivalent-command' when asked about bindings of
    ;; wrapped mappings
    (should (equal (keyswap-equivalent-binding [?b] test-map)
                   (funcall (lookup-key test-map [?b]) nil t)))

    ;; * Handles commands that have been `remap' -ed
    (let ((remapped-map (make-sparse-keymap)))
      (define-key remapped-map [remap self-insert-command] #'next-line)
      (set-keymap-parent remapped-map test-map)
      (should (equal (keyswap-equivalent-binding [?a] remapped-map)
                     (keyswap--equivalent-command [?a] 'next-line))))))

(defun keyswap--test-all-pairs-swapped (keyswap-pairs current-map original-map
                                                      &optional in-place)
  "Check all and only `keyswap-pairs' are swapped."
  (let ((keyswap-both-ways
         (append keyswap-pairs (mapcar (lambda (val) (cons (cdr val) (car val)))
                                       keyswap-pairs)))
        (keyswap-all-elements
         (loop for (left . right) in keyswap-pairs append (list left right))))
    ;; All keys in `keyswap-pairs' have been mapped as their partner.
    (dolist (key-pair keyswap-both-ways)
      (should (equal (lookup-key current-map (car key-pair))
                     (keyswap--equivalent-command
                      (cdr key-pair)
                      (lookup-key original-map (cdr key-pair))))))
    ;; Other keys are unaffected
    (loop for key from 1 upto 255
          when (not (member (vector key) keyswap-all-elements))
          do (should (equal (lookup-key current-map (vector key))
                            (if in-place (lookup-key original-map (vector key))
                              nil))))))

(ert-deftest keyswap-isearch-hooks ()
  "Ensure that `keyswap-isearch-start-hook' and
  `keyswap-isearch-end-hook' behave properly.

This includes cancelling each other out, creating the correct
functions and other such niceties."
  (keyswap--with-temp-list
   (let* ((original-isearch-map (copy-keymap isearch-mode-map))
          (isearch-mode-map (copy-keymap isearch-mode-map))
          (keyswap-mode t))
     (keyswap-isearch-start-hook)
     (unwind-protect
         (progn
           ;; Replaces commands in place in `isearch-mode-map'
           (keyswap--test-all-pairs-swapped
            keyswap-pairs isearch-mode-map original-isearch-map t)
           ;; Should add `keyswap-mode-end-hook' to `isearch-mode-end-hook'
           (should (member 'keyswap-isearch-end-hook isearch-mode-end-hook)))
       (keyswap-isearch-end-hook))
     (should (equal isearch-mode-map original-isearch-map)))))

(ert-deftest keyswap-test-swapped-keymap ()
  "`keyswap-swapped-keymap' does what it's supposed to."
  (keyswap--with-temp-list
   ;; We temporarily set `overriding-terminal-local-map' to `global-map', which
   ;; means all keys looked up will be found in that single map.
   ;; We use `global-map' because that's a keymap that has most keys bound to
   ;; something.
   (let ((overriding-terminal-local-map global-map))
     (keyswap--test-all-pairs-swapped
      keyswap-pairs (keyswap-swapped-keymap) overriding-terminal-local-map))))

(defmacro keyswap--matches-newgen ()
  `(equal (cdr (assoc 'keyswap-mode minor-mode-overriding-map-alist))
          (keyswap-swapped-keymap)))

(ert-deftest keyswap-update-modifications ()
  "Check that `keyswap-update-keys' does what it's supposed to."
  (keyswap--with-temp-list
   (let ((minor-mode-overriding-map-alist
          (list (list 'keyswap-mode 'keymap)))
         (keyswap-mode nil))
     (keyswap-update-keys)
     (should (keyswap--matches-newgen))
     (keyswap-add-pairs ?a ?b)
     (keyswap-update-keys)
     (should (keyswap--matches-newgen))
     (setq keyswap-mode t)
     (should-not (keyswap--matches-newgen)))))



(ert-deftest keyswap-future-enhancements ()
  "Tests that future enhancements should make pass."
  :expected-result :failed
  (let* ((test-command (lambda () (interactive)
                         (cons last-command-event (this-command-keys-vector))))
         (wrapped-command (keyswap--equivalent-command [?a] test-command)))
    ;; * Return value calls COMMAND with `this-command-keys-vector' set to [KEY]
    ;; XXX This is actually quite strange -- as far as I can see, the
    ;; `call-interactively' function just doesn't do what its documentation says
    ;; it will.
    ;; I may be misreading, but currently I can't find out how to get this to
    ;; work at all.
    (should (equal (cdr (funcall wrapped-command)) [?a]))))


;;; TODO
;;;   I'm starting with unit testing because that's what the info manual starts
;;;   with, but the aim is to end up with end-to-end testing that things behave
;;;   as they should in the default environment.
;;;   Tests that I want to make are:
;;;     1 Number characters insert symbols & symbol characters insert numbers
;;;     2 When modify a mapping and run `keyswap-update-keys' that modification
;;;       is propagated up.
;;;     3 Each of the premade exceptions work when used before *or* after
;;;       turning on the mode.
;;;     4 Isearch mode has the same keys swapped
;;;     5 jump-char mode works well too.

(provide 'keyswap-tests)
