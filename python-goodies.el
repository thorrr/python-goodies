;;; python-goodies.el --- customizations of builtin emacs mode

;; Copyright (C) 2017 Jason Bell

;; Author: Jason Bell <jbellthor@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1")
;;                    (python "0.25.1")
;;                    (cl "0.0.1")
;;                    (load-relative "20160716.438")
;;                    (ac-python-async "20170424")
;;                    (flymake "")
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
(load-relative "eldoc.el")
(load-relative "source-file.el")
(load-relative "pdb.el")
(load-relative "virtualenv.el")
(load-relative "commands.el")
(load-relative "hide-show.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
  ;; don't override the global fill column width
  (setq-local fill-column python-column-width)
  (set-variable 'python-indent-offset 4)
  (set-variable 'indent-tabs-mode nil)
))

;; make sure our inferior buffers are properly virtualenv aware too
(add-hook 'inferior-python-mode-hook (lambda ()
  (python-shell-setup python-inferior-shell-type)  ;; TODO - get rid of this
  (virtualenv-hook)))

(add-hook 'inferior-python-mode-hook (lambda ()
  ;; jump to the bottom of the comint buffer if you start typing
  (setq-local comint-scroll-to-bottom-on-input t)))

;; turn off "Active processes exist" warning for *Python* processes
(add-hook 'comint-exec-hook (lambda ()
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(add-hook 'python-mode-hook (lambda ()
  (python-shell-setup python-inferior-shell-type)  ;; TODO - get rid of this

  ;; Set up each file's virtualenv before calling python-just-source-file so that each virtualenv
  ;; will have a single internal process
  (virtualenv-hook)
  
  ;; source the file and then send our virtualenv and shell complete code to the internal process
  ;; don't source a file if it's a python repl buffer or other non-filename buffer
  (if (buffer-file-name) (progn
    (python-source-file-to-internal-process (buffer-file-name))
    ;; send an newline to clear the internal buffer because ipython sometimes hangs with a
    ;; "WARNING: Attempting to work in a virtualenv" message
    (if (< emacs-major-version 25)
        (run-at-time "3 sec"
            nil 'python-shell-send-string "\n" (python-shell-internal-get-or-create-process)
            "Warning: python-source-file-to-internal-process was called 3 seconds ago "
            "but there's no live process"))
    (if (check-for-virtualenv (python-get-named-else-internal-process))
        (message (concat "Virtualenv successfully activated in internal python process for "
                         (buffer-file-name))))))
  (if (check-for-readline (python-get-named-else-internal-process)) 't
    (message
     "Warning:  readline not detected on system.  "
     "autocomplete from process won't work.\npip install pyreadline to set it up"))

  ;; start pymacs now that virtualenv is set up
  (pymacs-setup)
  ;;something repeatedly calls pymacs-load "ropemacs" so you have to switch it back on
  (python-goodies/turn-on-ropemacs)
))

(provide 'python-goodies)

;;; python-goodies.el ends here
