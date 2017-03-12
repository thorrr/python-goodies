;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenv support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst bin-python-dir
  (if (eq system-type 'windows-nt) "Scripts/" "bin/")
  "root directory of the python executable based on emacs architecture")

(defconst bin-python
  (concat bin-python-dir (if (eq system-type 'windows-nt)  "python.exe" "python"))
  "path to the python executable based on emacs architecture")

(defun set-virtualenv (dir)
  (interactive "D")
  (setq python-shell-virtualenv-path (expand-file-name dir))
  (virtualenv-hook)
  't)

(defun detect-virtualenv (filename)
  "return directory containing virtualenv python[.exe] if it can
  detect this python file has a virtualenv in its path"
  (if filename
  (ldf-compat (file-name-directory filename)
   (lambda (cd)
     (let* ((subdir-p (lambda (dir) (if (file-directory-p dir) dir nil)))
            (ls (mapcar (lambda (f) (concat cd f)) (directory-files cd)))
            (subdirs (delq nil (mapcar subdir-p ls)))
            (subdirs-that-have-bin/python (delq nil (mapcar (lambda (dir)
              (if (file-regular-p (concat (file-name-as-directory dir) bin-python))
                  dir nil))
                 subdirs)))
            (virtualenv-dir (car subdirs-that-have-bin/python))
            (virtualenv-dir (if virtualenv-dir
              ;; sometimes we're left with a relative path i.e. scripts/lib/..
              (file-name-as-directory (expand-file-name virtualenv-dir)) nil)))
       (if virtualenv-dir (progn
         (message (concat "Found " virtualenv-dir " as virtualenv for " filename))
         virtualenv-dir) nil)))) nil))

(defvar python-goodies/virtualenv-activate-command ""
  "The current virtualenv activate command.  We can't make this
  buffer-local since the comint hooks don't run under our
  buffer's scope")

(defun virtualenv-hook ()
  "This should be run before any comints are run.  And re-run when opening a new file."
  (make-local-variable 'python-shell-virtualenv-path)
  (add-to-list 'python-shell-setup-codes 'python-goodies/virtualenv-activate-command)
  (if auto-detect-virtualenv
      (let ((detected-virtualenv (detect-virtualenv (buffer-file-name))))
        (if detected-virtualenv
            (setq python-shell-virtualenv-path detected-virtualenv))))
  
  ;; the following doesn't work because pymacs has already been called at this point
  ;; (make-local-variable 'pymacs-python-command)
  ;; (setq pymacs-python-command (concat python-shell-virtualenv-path bin-python))
  
  ;; If we've detected a virtualenv specialize setup codes to
  ;; activate it in all new shells.
  (if (file-exists-p (concat python-shell-virtualenv-path bin-python-dir "activate_this.py")) (progn
     (setq python-goodies/virtualenv-activate-command
           (concat "af = \"" python-shell-virtualenv-path bin-python-dir
                   "activate_this.py\"; execfile(af, dict(__file__=af))\n"))
     ))

  ;; finally, if we're using ipython, update the current value of python-shell-interpreter-args
  (if (eq python-inferior-shell-type 'ipython)
      (let ((ipython-script
             (expand-file-name "ipython-script.py"
                               (format "%s/%s" python-shell-virtualenv-path bin-python-dir))))
        (if (not (file-exists-p ipython-script))
            (message (concat "Warning:  inferior-shell-type is 'ipython but we can't find "
                             "ipython-script.py in the virtualenv.\nOn Windows make sure "
                             "you've installed it with pip install ipython --no-use-wheel."))
          ;; else call the interpreter with the ipython inside the virtualenv
          (setq python-shell-interpreter-args (concat "-u " ipython-script))))))

