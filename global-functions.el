;; functions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defun python-goodies/get-or-start-completion-process ()
  "Get a dedicated process for the current buffer.

   Return (python-shell-get-process) if it's not nil.  This means
   that (run-python) repls will complete if we've started one for
   the current buffer.

   Otherwise, create a new internal process unique to the buffer
   or common to the virtualenv if we're in one.

   If there's a virtualenv, reuse an existing process.

   Implementation:
       Call run-python but override python-shell-make-comint to create an 'internal' buffer.
       Can't use run-python-internal because setup codes aren't sent."
  (let ((process (python-shell-get-process))
        (completion-process
         (if (boundp 'python-goodies/completion-process) python-goodies/completion-process nil)))
    (cond (process process)
          (completion-process completion-process)
          ('t
           (let* ((venv-path (if python-shell-virtualenv-root
                                 python-shell-virtualenv-root
                               (executable-find "python")))
                  (venv-hash (secure-hash 'md5 venv-path))
                  (venv-label (substring venv-hash 0 5)))
             (cl-letf* (( ;; first, override process-name to be unique per venv
                         (symbol-function 'python-shell-get-process-name)
                         (lambda (dedicated) (format "%s [%s]" python-shell-buffer-name venv-label)))
                     
                        ;; now override python-shell-make-comint to have "internal" be t
                        ((symbol-function 'psmc) (symbol-function 'python-shell-make-comint)) ;;original fn
                        ((symbol-function 'python-shell-make-comint)
                         (lambda (cmd proc-name &optional show internal)
                           (psmc cmd proc-name show 't) ;; call original fn but set internal 't
                           ))
                        (new-process (run-python (python-shell-calculate-command)
                                                 't ;; dedicated
                                                 nil ;; show
                                                 )))
               (setq-local python-goodies/completion-process new-process)
               new-process))))))

(defun python-goodies/switch-to-completion-process ()
  "For debugging"
  (interactive)
  (pop-to-buffer (process-buffer (python-goodies/get-or-start-completion-process))))

(defun check-for-virtualenv (process)
  "return 't if this process is a virtualenv."
  (if (not process) (progn (message "warning:  no process in check-for-virtualenv") nil)
    (let ((repl-out (python-shell-send-string-no-output "import sys;  hasattr(sys, 'real_prefix')" process)))
      (string= repl-out "True"))))

(defun check-for-readline (process)
  "return 't if we can import readline.  if we can't autocomplete will be silently broken"
  (if (not process) (progn (message "warning:  no process in check-for-readline") nil)
    (let ((repl-out (python-shell-send-string-no-output "import readline;''" process)))
      (if (string-match "^\\(>>> \\)*''" repl-out) 't nil))))

(defun ldf-compat (current-dir fn-or-subdir)
  "Partial replacement for locate-dominating-file.

  Starting from current-dir, walk up directory tree
until (fn-or-subdir current-dir) returns non nil or (concat
current-dir fn-or-subdir) exists.

  fn-or-subdir can be a filename (\"env\" for example) or a
function that takes a single argument "
  (defun parent-directory (dir)
    (unless (string-match "^\\([a-z]:\\)*/$" dir)
      (file-name-directory (directory-file-name dir))))
  (let* ((f (if (functionp fn-or-subdir)
              fn-or-subdir  
              (lambda (cd) (file-exists-p (concat cd fn-or-subdir)))))
         (cd (file-name-directory (expand-file-name current-dir)))
         (good-dir (funcall f current-dir))
         (parent (parent-directory cd)))
    (if good-dir good-dir
      (when parent
        (ldf-compat parent fn-or-subdir)))))

(defun send-package-directory (filename &optional process)
  "add the root project directory to sys.path This allows modules
deep in the project hierarchy to be sourced without relative
import errors"
  (let* ((detect-package-directory (lambda (filename)
            "return first parent directory that does not contain __init__.py"
            (ldf-compat (file-name-directory filename)
                        (lambda (cd)
                          (if (file-exists-p (concat cd "__init__.py"))
                              nil cd)))))
         (package-directory (funcall detect-package-directory filename)))
    (python-shell-send-string 
     (concat "import sys;\nif sys.path.count('" package-directory "') == 0:\n"
             "  sys.path.insert(0, '" package-directory  "')\n") process)))

(defun message-no-echo (string)
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max)) (insert string) (insert "\n"))))
