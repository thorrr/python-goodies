;;; python-goodies.el --- customizations of builtin emacs mode

;; Copyright (C) 2017 Jason Bell

;; Author: Jason Bell <jbellthor@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "0.24.1")
;;                    cl
;;                    load-relative
;;                    (ac-python-async "20170425")
;;                    flymake
;;                   )
;; Keywords: python
;; URL: https://github.com/thorrr/python-goodies

;;; Commentary:

;; Provides functions to automatically detect and setup virtual environments,
;; reasonable keybindings, setup rope correctly, auto-complete, and more.

;;; Code:
(require 'python)
(require 'cl)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'load-relative)
(load-relative "custom-variables.el")
(load-relative "global-functions.el") ;; subsequent modules can depend on these
(load-relative "keybindings.el")
(load-relative "inferior-python-shell-setup.el")
(load-relative "pymacs-rope.el")
(load-relative "autocomplete.el")
(load-relative "flymake-python.el")
(load-relative "py-eldoc.el")
(load-relative "source-file.el")
(load-relative "pdb.el")
(load-relative "virtualenv.el")
(load-relative "commands.el")
(load-relative "hide-show.el")
(load-relative "eshell-support.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (< emacs-major-version 25) (progn
  (defmacro python-shell--add-to-path-with-priority (pathvar paths)
    "Modify PATHVAR and ensure PATHS are added only once at beginning."
    `(dolist (path (reverse ,paths))
       (cl-delete path ,pathvar :test #'string=)
       (cl-pushnew path ,pathvar :test #'string=)))
  (defalias 'python-shell-calculate-command 'python-shell-parse-command)

  (defun python-shell-send-setup-code-to-process (process)
    "Gallina's python-shell-send-setup-code doesn't allow a buffer
     argument"
    (cl-letf ((symbol-function 'get-buffer-process) (lambda (_) process))
      (python-shell-send-setup-code)
      process))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
  ;; don't override the global fill column width
  (setq-local fill-column python-column-width)
  (set-variable 'python-indent-offset 4)
  (set-variable 'indent-tabs-mode nil)
))

(add-hook 'inferior-python-mode-hook (lambda ()
  ;; jump to the bottom of the comint buffer if you start typing
  (setq-local comint-scroll-to-bottom-on-input t)))

;; turn off "Active processes exist" warning for *Python* processes
(add-hook 'comint-exec-hook (lambda ()
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook (lambda ()
  ;; (python-shell-setup python-inferior-shell-type)  ;; TODO - make this emacs24 specific

  ;; Set up each file's virtualenv before calling python-just-source-file
  (virtualenv-hook)
  
  ;; don't source a file if it's a python repl buffer or other non-filename buffer
  (if (buffer-file-name) (progn
    (python-goodies/get-or-start-completion-process)
    (python-source-file-to-completion-process (buffer-file-name))))

  ;; start pymacs now that virtualenv is set up
  (pymacs-setup)
  ;; something repeatedly calls pymacs-load "ropemacs" so you have to switch it back on
  (python-goodies/turn-on-ropemacs)

  ;; delay running these to give the prompt a chance to come back
  (run-at-time "3 sec" nil (lambda ()
    ;; clear extra prompts from "source file" (doesn't work)
    ;; (python-shell-send-string "\n" (python-goodies/get-or-start-completion-process))
    (if (check-for-virtualenv (python-goodies/get-or-start-completion-process))
        (message-no-echo (concat "Virtualenv successfully activated in completion python process for "
                                 (buffer-file-name))))
    (if (check-for-readline (python-goodies/get-or-start-completion-process)) 't
      (message
       "Warning:  readline not detected on system.  "
       "autocomplete from process won't work.\npip install pyreadline to set it up"))))
))

(provide 'python-goodies)

;;; python-goodies.el ends here
