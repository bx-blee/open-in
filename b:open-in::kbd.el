;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'b:open-in::alias)

(defun b:open-in|kbd ()
  (define-key global-map [(f8) (o)] nil)
  (define-key global-map [(f8) (o) (o)] 'b:open-in/transient)
  (define-key global-map [(f8) (o) (\0)] (lambda () (interactive) (popup-menu (symbol-value (b:open-in:menu|define)))))
  (define-key global-map [(f8) (o) (v)] 'b:open-in/vscode)
  (define-key global-map [(f8) (o) (t)] 'b:open-in/terminal)
  (define-key global-map [(f8) (o) (e)] 'b:open-in/external-app)
  (define-key global-map [(f8) (o) (d)] 'b:open-in/show-in-desktop)
  )

(provide 'b:open-in::kbd)
