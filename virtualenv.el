;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenv support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvaralias 'python-shell-virtualenv-path 'python-shell-virtualenv-root) ;; -root is emacs25+

(defconst bin-python-dir
  (if (eq system-type 'windows-nt) "Scripts/" "bin/")
  "root directory of the python executable based on emacs architecture")

(defun bin-python ()
    "path to the python executable based on emacs architecture"
    (concat bin-python-dir (if (eq system-type 'windows-nt)  
                               "python.exe" 
                             (concat "python" (format "%d" python-major-version)))))

(defun set-virtualenv (dir)
  "Must reopen buffers after you run this function.

   If auto-detect-virtualenv is 't, auto-detection will override this global setting. "
  (interactive "D")
  (setq python-shell-virtualenv-root (expand-file-name dir))
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
              (if (file-regular-p (concat (file-name-as-directory dir) (bin-python)))
                  dir nil))
                 subdirs)))
            (virtualenv-dir (car subdirs-that-have-bin/python))
            (virtualenv-dir (if virtualenv-dir
              ;; sometimes we're left with a relative path i.e. scripts/lib/..
              (file-name-as-directory (expand-file-name virtualenv-dir)) nil)))
       (if virtualenv-dir (progn
         (message (concat "Found " virtualenv-dir " as virtualenv for " filename))
         virtualenv-dir) nil)))) nil))

(defun virtualenv-hook ()
  "This should be run before any comints are run.  And re-run when opening a new file."
  (make-local-variable 'python-shell-virtualenv-root)
  (if auto-detect-virtualenv
      (let ((detected-virtualenv (detect-virtualenv (buffer-file-name))))
        (if detected-virtualenv
            (setq python-shell-virtualenv-root detected-virtualenv))))
  
  ;; finally, if we're using ipython, update the current value of python-shell-interpreter-args
  (if (eq python-inferior-shell-type 'ipython)
      (let ((ipython-script
             (expand-file-name "ipython-script.py"
                               (format "%s/%s" python-shell-virtualenv-root bin-python-dir))))
        (if (not (file-exists-p ipython-script))
            (message (concat "Warning:  inferior-shell-type is 'ipython but we can't find "
                             "ipython-script.py in the virtualenv.\nOn Windows make sure "
                             "you've installed it with pip install ipython --no-use-wheel."))
          ;; else call the interpreter with the ipython inside the virtualenv
          (setq python-shell-interpreter-args (concat "-u " ipython-script))))))


;; this is copied from built in.  Hard codes "bin" which doesn't work in Windows
(defun python-shell-calculate-exec-path ()
  "Calculate `exec-path'.
Prepends `python-shell-exec-path' and adds the binary directory
for virtualenv if `python-shell-virtualenv-root' is set.  If
`default-directory' points to a remote host, the returned value
appends `python-shell-remote-exec-path' instead of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       python-shell-remote-exec-path
                     exec-path))))
    (python-shell--add-to-path-with-priority
     new-path python-shell-exec-path)
    (if (not python-shell-virtualenv-root)
        new-path
      (python-shell--add-to-path-with-priority
       new-path
       (list (expand-file-name bin-python-dir python-shell-virtualenv-root)))
      new-path)))
