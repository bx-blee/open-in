;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'xah-open-in)

(defalias 'b:open-in/vscode 'xah-open-in-vscode)
(defalias 'b:open-in/terminal 'xah-open-in-terminal)
(defalias 'b:open-in/external-app 'xah-open-in-external-app)
(defalias 'b:open-in/show-in-desktop 'xah-show-in-desktop)

(provide 'b:open-in::alias)
