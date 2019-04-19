;;; eshell-support.el --- make env work with eshell  -*- lexical-binding: t -*-

(defcustom python-goodies/virtualenv-name
  (if (eq system-type 'windows-nt) "env-win" "env")
  "Try to activate a virtualenv with this name if we don't
  specify a directory in eshell/sv")

(add-hook 'eshell-mode-hook (lambda ()
  ;; create buffer-local customizations so we don't affect other buffers
  (make-local-variable 'python-shell-virtualenv-path)
  (make-local-variable 'eshell-prompt-function)
  (make-local-variable 'python-goodies/venv-prompt)
  (make-local-variable 'python-goodies/_eshell-prompt-function-orig)
  (setq python-goodies/_eshell-prompt-function-orig eshell-prompt-function)
  ))

(defun python-goodies/_make_clean_path ()
  (setenv "PATH"
          (replace-regexp-in-string "/"
              (if (eq system-type 'windows-nt) "\\\\" "/") eshell-path-env)))

(defun eshell/sv (&optional arg)
  "sv - 'start virtualenv'.  Optionally specify a virtualenv
   directory.  If no directory is specified, attempt to activate a
   directory called python-goodies/virtualenv-name"
  (interactive "D")
  (let* ((dirname
          (if arg (file-truename "arg")
            (concat (eshell/pwd) "/" python-goodies/virtualenv-name)))
         (venv-bin-path (concat dirname "/" bin-python-dir))
         (project-path (file-name-directory (directory-file-name dirname)))
         (is-venv? (file-exists-p venv-bin-path)))
    (if is-venv? (progn
        (eshell/deactivate)
        (setq python-shell-virtualenv-path venv-bin-path)
        ;; eshell doesn't use this variable but change it just in case
        (add-to-list 'exec-path venv-bin-path)
        ;; eshell uses eshell-path-env to resolve executables. -b means add to front of path
        (eshell/addpath "-b" venv-bin-path)
        ;; the previous function brutally changes PATH.  Clean it up.
        (python-goodies/_make_clean_path)
        (setq python-goodies/venv-prompt (file-name-base (directory-file-name project-path)))
        (setq eshell-prompt-function (lambda ()
            (concat "(" python-goodies/venv-prompt "-env) " (eshell/pwd) " $ "))))
      (error "sv can't find virtualenv at path %s" dirname))
   (message (format "activated %s" dirname))
))

(defun eshell/deactivate ()
  ;; split eshell-path-env into a list and remove the current virtualenv from it
  (let* ((eshell-path-list (remove python-shell-virtualenv-path (parse-colon-path eshell-path-env))))
    ;; then join the list back into a single PATH-style string
    (setq eshell-path-env (mapconcat 'identity eshell-path-list path-separator)))
  ;; unfortunately eshell changes the global PATH for emacs when you do eshell/addpath.  Undo this.
  (python-goodies/_make_clean_path)
  (delete python-shell-virtualenv-path exec-path)
  (setq python-shell-virtualenv-path nil)
  (setq eshell-prompt-function python-goodies/_eshell-prompt-function-orig)
  nil)

(provide 'python-goodies-eshell-support)
