;;;-*- Mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.
URL `http://xahlee.info/emacs/emacs/emacs_open_in_vscode.html'

Version: 2020-02-13 2021-01-18 2022-08-04 2023-06-26"
  (interactive)
  (let ((xpath (if buffer-file-name buffer-file-name (expand-file-name default-directory))))
    (message "path is %s" xpath)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument xpath))))
     ((eq system-type 'windows-nt)
      (shell-command (format "code.cmd %s" (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (shell-command (format "code %s" (shell-quote-argument xpath)))))))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Created: 2019-11-04
Version: 2025-04-18"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (string-replace "'" "`'" x))) nil)))
           xfileList)))
       ((eq system-type 'darwin)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Microsoft Windows File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_show_in_desktop.html'
Created: 2020-11-20
Version: 2023-09-09"
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if buffer-file-name buffer-file-name default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let ((xcmd (format "Explorer /select,%s"
      ;;                     (replace-regexp-in-string "/" "\\" xpath t t)
      ;;                     ;; (shell-quote-argument (replace-regexp-in-string "/" "\\" xpath t t ))
      ;;                     )))
      ;;   (shell-command xcmd))
      )
     ((eq system-type 'darwin)
      (shell-command
       (concat "open -R " (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory xpath)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))


(defvar xah-fly-mswin-terminal
  "wt"
  "A string. Value should be one of: wt (for Windows Terminal) or pwsh (for PowerShell Core (cross-platform)) or powershell (for Microsoft PowerShell).")

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, which terminal it starts depends on `xah-fly-mswin-terminal'.

URL `http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html'
Version: 2020-11-21 2022-08-04 2023-03-01 2023-06-26"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (cond
     ((string-equal xah-fly-mswin-terminal "wt") (shell-command (format "wt -d \"%s\"" default-directory)))
     ((string-equal xah-fly-mswin-terminal "pwsh") (shell-command (format "pwsh -Command Start-Process pwsh -WorkingDirectory '%s'" (shell-quote-argument default-directory))))
     ((string-equal xah-fly-mswin-terminal "powershell") (shell-command (format "powershell -Command Start-Process powershell -WorkingDirectory '%s'" (shell-quote-argument default-directory))))
     (t (error "Error 702919: value of `xah-fly-mswin-terminal' is not expected. Its value is %s" xah-fly-mswin-terminal))))
   ((eq system-type 'darwin)
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((eq system-type 'berkeley-unix)
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

(defun xah-dired-open-in-textedit ()
  "Open the current file or `dired' marked files in Mac's TextEdit.
This command is for macOS only.

URL `http://xahlee.info/emacs/emacs/emacs_open_in_textedit.html'
Version: 2017-11-21 2021-02-07 2023-06-26"
  (interactive)
  (when (not (eq system-type 'darwin)) (user-error "Error: textedit only run in Mac"))
  (let* (
         (xFList (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list buffer-file-name)))
         (xDoIt (if (<= (length xFList) 10) t (y-or-n-p "Open more than 10 files? "))))
    (when xDoIt
      (mapc (lambda (x) (shell-command (format "open -a TextEdit.app \"%s\"" x))) xFList))))

(provide 'xah-open-in)
