;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^\\(.+\\) should\\( not\\|\\) be active$"
       (lambda (mode activep)
         (let* ((symbol (intern mode))
                (active (and (boundp symbol) (symbol-value symbol))))
           (if (s-blank? activep) (should active) (should-not active)))))

(When "^I swap\\( only\\|\\) keys \"\\(.\\)\" and \"\\(.\\)\"$"
      "Swaps some keys -- these must be single characters."
      (lambda (only left-string right-string)
        (let ((left-event (aref left-string 0))
              (right-event (aref right-string 0)))
          (if (s-blank? only) (keyswap-add-pairs left-event right-event)
            (keyswap-set-pairs left-event right-event)))
        (execute-kbd-macro
         (vconcat [?\M-x] (string-to-vector "keyswap-update-keys")))))

;; Here because espuds.el has "I turn on ..." but not the opposite
(When "^I turn off \\(.+\\)$"
      "Turns on some mode."
      (lambda (mode)
        (let ((v (vconcat [?\C-0 ?\M-x] (string-to-vector mode))))
          (execute-kbd-macro v))))

(Given "^Mode integration is \\(on\\|off\\)$"
       (lambda (on-or-off)
         (funcall (if (string= on-or-off "off") #'remove-hook #'add-hook)
                  'isearch-mode-hook 'keyswap-isearch-start-hook)))

(When "^I isearch for \"\\(.+\\)\"$"
      (lambda (search-string)
        (Given "I start an action chain")
        (And "I press \"C-s\"")
        (And (format "I type \"%s\"" search-string))
        (And "I execute the action chain")))

(When "^I jump-char to \"\\(.\\)\""
      (lambda (search-char)
        (require 'jump-char)
        (Given "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"jump-char-forward\"")
        (And "I press \"<return>\"")
        (And (format "I press \"%s\"" search-char))
        (And "I press \"C-f\"")
        (And "I press \"C-b\"")
        (And "I execute the action chain")))
