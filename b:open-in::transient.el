;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'xah-open-in)
(require 'b:open-in::alias)

(transient-define-prefix b:open-in/transient ()
  "Open-In menu."
  ["Editor & Terminal"
   [("v" "open in vscode" b:open-in/vscode)
    ("t" "open in terminal" b:open-in/terminal)]]
  ["External & Desktop"
   [("e" "open in external app" b:open-in/external-app)
    ("d" "show in desktop" b:open-in/show-in-desktop)]])


(provide 'b:open-in::transient)
