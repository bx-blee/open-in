;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'xah-open-in)
(require 'b:open-in::alias)

;; (transient-define-prefix git-timemachine-help ()
;;  "Show online help."
;;  ["Navigate"
;;   [("p" "show previous revision" git-timemachine-show-previous-revision)
;;    ("n" "show next revision" git-timemachine-show-next-revision)
;;    ("g" "show nth revision" git-timemachine-show-nth-revision)
;;    ("h" "show nearest revision" git-timemachine-show-nearest-revision)
;;    ("t" "show fuzzy revision" git-timemachine-show-revision-fuzzy)
;;    ("i" "show revision introducing" git-timemachine-show-revision-introducing)]]
;;  ["Kill current revision"
;;   [("w" "kill abbreviated revision" git-timemachine-kill-abbreviated-revision)
;;    ("W" "kill revision" git-timemachine-kill-revision)]]
;;  ["Misc"
;;   [("b" "blame current revision" git-timemachine-blame)
;;    ("c" "show commit" git-timemachine-show-commit)
;;    ("?" "show help" git-timemachine-help)
;;    ("q" "quit" git-timemachine-quit)]])



(provide 'b:open-in::transient)
