;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'easymenu)
(require 'cl-lib)  ;; For using cl functions like cl-loop, cl-letf, etc.
(require 'subr-x)  ;; For using string manipulation functions like string-trim

(require 'compile-time-function-name)
(require 'xah-open-in)
(require 'b:open-in::alias)

;; (b:open-in:menu:plugin|install modes:menu:global (s-- 6))
(defun b:open-in:menu:plugin|install (<menuLabel <menuDelimiter)
  "Adds this as a submenu to menu labeled <menuLabel at specified delimited <menuDelimiter."

  (easy-menu-add-item
   <menuLabel
   nil
   (b:open-in:menu|define :active t)
   <menuDelimiter
   )

  ;; (add-hook 'menu-bar-update-hook 'b:open-in:menu|update-hook)
  )

(defun b:open-in:menu|update-hook ()
  "This is to be added to menu-bar-update-hook.
It runs everytime any menu is invoked.
As such what happens below should be exactly what is necessary and no more."
  ;;(modes:menu:global|define)
  )

;;
;; (b:open-in:menu|define :active nil)
;; (popup-menu (symbol-value (b:open-in:menu|define)))
;;
(defun b:open-in:menu|define (&rest <namedArgs)
  "Returns b:open-in:menu.
:active can be specified as <namedArgs.
"
  (let (
	(<active (get-arg <namedArgs :active t))
	($thisFuncName (compile-time-function-name))
	)

    (easy-menu-define
      b:open-in:menu
      nil
      (format "Open-In Menu")
      `(
	,(format "Open-In :: Menu")
	:help "open-in menu"
	:active ,<active
	:visible t
	,(s-- 3)
	,(s-- 4)
	,(s-- 5)
	,(s-- 6)
	,(s-- 7)
	,(s-- 8)
	))

    (easy-menu-add-item
     b:open-in:menu nil
     (b:open-in:menuItem:vscode|define)
       (s-- 3))

   (dolist (item '(b:open-in:menuItem:terminal|define
                   ))
      (easy-menu-add-item
       b:open-in:menu nil
       (funcall item)
       (s-- 4)))

   (dolist (item '(b:open-in:menuItem:external-app|define
                   b:open-in:menuItem:show-in-desktop|define
                   ))
      (easy-menu-add-item
       b:open-in:menu nil
       (funcall item)
       (s-- 5)))

   (easy-menu-add-item
    b:open-in:menu nil
    (b:menu:panelAndHelp|define
      :panelName "/bisos/panels/blee-core/AI/gptel/_nodeBase_"
      :funcName $thisFuncName
      :pkgRepoUrl "https://github.com/karthink/gptel"
      )
     (s-- 8))

    'b:open-in:menu
    ))

(defun b:open-in:menuItem:vscode|define ()
  (car `(
    [,(format "Open in vscode.")
     (call-interactively 'b:open-in/vscode)
     :help "Open current file or dir in vscode."
     ]
    )))

(defun b:open-in:menuItem:terminal|define ()
  (car `(
    [,(format "Open current dir in terminal")
     (b:open-in/terminal)
     :help "Open the current dir in a new terminal window."
     ]
    )))

(defun b:open-in:menuItem:external-app|define ()
  (car `(
    [,(format "Open in external app.")
     (b:open-in/external-app)
     :help "Open the current file or dired marked files in external app."
     ]
    )))

(defun b:open-in:menuItem:show-in-desktop|define ()
  (car `(
    [,(format "Show current file in desktop")
     (b:open-in/show-in-desktop)
     :help "Show current file in desktop."
     ]
    )))


(provide 'b:open-in::menu)
