;; functions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defun python-goodies/get-or-start-completion-process ()
  "To change process name, override (python-shell-get-process-name dedicated)
   TODO - if virtualenv activated, use same process for all buffers (for efficiency)"
  (let ((process (python-shell-get-process)))
    (if (not process)
        (run-python (python-shell-calculate-command)
                    't  ;; dedicated
                    nil ;; show
                    )
      process)))

(defun python-goodies/switch-to-completion-process ()
  "For debugging"
  (interactive)
  (python-shell-switch-to-shell))

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
