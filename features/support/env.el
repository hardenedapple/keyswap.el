(require 'f)

(defvar keyswap-support-path
  (f-dirname load-file-name))

(defvar keyswap-features-path
  (f-parent keyswap-support-path))

(defvar keyswap-root-path
  (f-parent keyswap-features-path))

(add-to-list 'load-path keyswap-root-path)

(require 'keyswap)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
