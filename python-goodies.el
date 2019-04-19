;;; python-goodies.el --- customizations of builtin emacs mode

;; Copyright (C) 2017 Jason Bell

;; Author: Jason Bell <jbellthor@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "0.24.1")
;;                    cl
;;                    load-relative
;;                    (ac-python-async "20170425")
;;                    flymake
;;                    el-get
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package load-relative
  :ensure t)

(use-package el-get
  :ensure t)

(el-get-bundle ac-python-async
  :url "https://github.com/thorrr/ac-python-async.git")

(use-package ac-python-async
  :ensure nil  ;; use el-get package
)

(el-get-bundle Pymacs
  :url "https://github.com/pinard/Pymacs.git"
  (shell-command-to-string 
   (concat "cd " package-user-dir "Pymacs && make"
           (if (eq system-type 'windows-nt) " && make install" ""))))
(setq pymacs-parent-dir shared-externals)
;; can't do use-package because Pymacs and pymacs.el are differently cased
;; besides, (require 'pymacs) happens inside pymacs-setup()
;;(require 'pymacs)

;; these don't use (require '<package>) - they're python code specially hooked by pymacs
(el-get-bundle rope
  :url "https://github.com/python-rope/rope.git")
(el-get-bundle ropemacs
  :url "https://github.com/python-rope/ropemacs.git")
(el-get-bundle ropemode
  :url "https://github.com/python-rope/ropemode.git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'load-relative)
(load-relative "custom-variables")
(load-relative "global-functions") ;; subsequent modules can depend on these
(load-relative "keybindings")
(load-relative "inferior-python-shell-setup")
(load-relative "pymacs-rope")
(load-relative "autocomplete")
(load-relative "flymake-python")
(load-relative "py-eldoc")
(load-relative "source-file")
(load-relative "pdb")
(load-relative "virtualenv")
(load-relative "commands")
(load-relative "hide-show")
(load-relative "eshell-support")

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

  ;; the emacs25 version returns the created process 
  (defun run-python (&optional cmd dedicated show)
    "Run an inferior Python process.
  
  Argument CMD defaults to `python-shell-calculate-command' return
  value.  When called interactively with `prefix-arg', it allows
  the user to edit such value and choose whether the interpreter
  should be DEDICATED for the current buffer.  When numeric prefix
  arg is other than 0 or 4 do not SHOW.
  
  For a given buffer and same values of DEDICATED, if a process is
  already running for it, it will do nothing.  This means that if
  the current buffer is using a global process, the user is still
  able to switch it to use a dedicated one.
  
  Runs the hook `inferior-python-mode-hook' after
  `comint-mode-hook' is run.  (Type \\[describe-mode] in the
  process buffer for a list of commands.)"
    (interactive
     (if current-prefix-arg
         (list
          (read-shell-command "Run Python: " (python-shell-calculate-command))
          (y-or-n-p "Make dedicated process? ")
          (= (prefix-numeric-value current-prefix-arg) 4))
       (list (python-shell-calculate-command) nil t)))
    (get-buffer-process
     (python-shell-make-comint
      (or cmd (python-shell-calculate-command))
      (python-shell-get-process-name dedicated) show)))

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
