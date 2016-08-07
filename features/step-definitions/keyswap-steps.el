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
      (lambda (only left-key right-key)
        (if (s-blank? only) (keyswap-add-pairs left-key right-key)
          (keyswap-set-pairs left-key right-key))))

(When "^I update the keyswap map$"
      (lambda () (keyswap-update-keys)))

(When "^I turn off keyswap-mode$"
      (lambda () (keyswap-mode 0)))
