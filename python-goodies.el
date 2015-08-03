(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python specific keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
  (define-key python-mode-map (kbd "C-M-<return>") 'python-goodies-python-send-buffer)
  (define-key python-mode-map (kbd "M-.") 'python-goodies-rope-goto-definition)
  (define-key python-mode-map (kbd "M-,") 'python-goodies-rope-go-backward)
  (define-key python-mode-map (kbd "M-i") 'python-goodies-python-shell-smart-switch)
  (define-key python-mode-map (kbd "C-c C-j") 'python-goodies-eval-line)
  (define-key python-mode-map (kbd "S-<f4>") 'python-goodies-restart-python-repl)
))

(add-hook 'inferior-python-mode-hook (lambda ()
  (define-key inferior-python-mode-map (kbd "M-i") 'python-goodies-python-shell-smart-switch)
  (define-key inferior-python-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key inferior-python-mode-map [up] 'comint-previous-matching-input-from-input)
  (define-key inferior-python-mode-map [f4] 'python-goodies-restart-python-repl)
))

(add-hook 'ropemacs-mode-hook (lambda ()
  (define-key ropemacs-local-keymap (kbd "M-?") 'ac-start)
  (define-key ropemacs-local-keymap (kbd "M-/") 'hippie-expand)
  ;;add menu item to Rope menu
  (define-key-after (lookup-key ropemacs-local-keymap [menu-bar Rope])
    [setup-virtualenv] '("Setup Virtualenv" . rope-set-virtualenv) 'rope-set-virtualenv)
))

;; can't use python-shell-extra-pythonpaths because these have to be set before we require 'pymacs
(setenv "PYTHONPATH" (concat
  (concat shared-externals "Pymacs" path-separator)
  (concat shared-externals "ropemacs" path-separator)
  (concat shared-externals "ropemode" path-separator)
  (concat shared-externals "rope" path-separator)
  (getenv "PYTHONPATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inferior Python shell setup variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom python-inferior-shell-type 'python
  "Customize inferior shells to be \"python\" or \"ipython\"."
  :type 'symbol
  :options '(python ipython)
  )


(defun python-shell-setup (shell-type)
  (cond ((eq shell-type 'ipython)
    (message "Changing python-shell-inferior variables to support ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "-i"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n" 
     ) 't)
        ((eq shell-type 'python)
     (message "Changing python-shell-inferior variables to support python")
     (setq
      python-shell-interpreter (eval (car (get 'python-shell-interpreter 'standard-value)))
      python-shell-interpreter-args (eval (car (get 'python-shell-interpreter-args 'standard-value)))
      python-shell-prompt-regexp (eval (car (get 'python-shell-prompt-regexp 'standard-value)))
      ;;sometimes prompts "build up" in the inferior processes so filter them out
      python-shell-prompt-output-regexp "\\(>>> \\)*" ;;(eval (car (get 'python-shell-prompt-output-regexp 'standard-value)))
      python-shell-completion-setup-code (eval (car (get 'python-shell-completion-setup-code 'standard-value)))
      python-shell-completion-module-string-code (eval (car (get 'python-shell-completion-module-string-code 'standard-value)))
      python-shell-completion-string-code  (eval (car (get 'python-shell-completion-string-code 'standard-value)))
      ) 't)
        ('t (error "python-shell-setup must be called with 'python or 'ipython"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pymacs)
(setq pymacs-auto-restart t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ac-python) ;; a source for python auto-complete that comes from the
                     ;; *Python* buffer or the unnamed "internal" process
;;swallow errors because sometimes start of expression gets a region of (nil xxxxx)
(defadvice ac-get-python-symbol-at-point (around ac-get-python-symbol-at-point-around activate)
  (let ((out (ignore-errors ad-do-it)))
    (if out out "")))

(defun python-symbol-completions (symbol)
  "Adapter to make ac-python work with gallina's python mode"
  (let* ((process (python-get-named-else-internal-process))
         (whole-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (input (replace-regexp-in-string "\\(^[[:space:]]*\\)" "" whole-line)) ;; have to eliminate leading tabs/spaces?
         (psc (python-shell-completion-get-completions process whole-line input)))
    (if psc psc "")))

(add-hook 'python-mode-hook (lambda () 
  ;; (add-to-list 'ac-sources 'ac-source-yasnippet)
))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
(defun flymake-pyflakes-init ()
  (if (and (executable-find "pyflakes")
           (executable-find "pep8"))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "cmd" (list "/c"
                          "pyflakes" local-file
                          "&"
                          "pep8" "--ignore=E124,E265" "--max-line-length=100" local-file
                          )))
    (progn (message "Warning:  pyflakes executable not found") nil)
    ))

(add-hook 'python-mode-hook (lambda ()
  ;;modify pyflakes' output
  ;; use \\| to separate multiple match criteria
  (setq flymake-warn-line-regexp "imported but unused\\|unable to detect undefined names\\|E[0-9]*")
  (setq flymake-info-line-regexp "is assigned to but never used")))

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks - loading a python file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook (lambda ()
  (set-variable 'python-indent-offset 4)
  (set-variable 'indent-tabs-mode nil)
  (python-shell-setup python-inferior-shell-type)
  (setq ropemacs-enable-autoimport t)   ;;TODO - make symbolName : packagea.packageb.packageC trigger an import statement

  ;; An Internal Process is created for each unique configuration.
  ;; Set up each file's virtualenv before calling python-just-source-file so that each virtualenv
  ;; will have a single internal process
  (virtualenv-hook)
  ;; source the file and then send our virtualenv and shell complete code to the internal process
  (python-just-source-file (buffer-file-name)
                           (python-shell-send-setup-code-to-process
                            (python-shell-internal-get-or-create-process)))
  (if (check-for-readline (python-get-named-else-internal-process)) 't
    (message "Warning:  readline not detected on system.  pip install pyreadline to set it up"))
  (if (check-for-virtualenv (python-get-named-else-internal-process))
      (message (concat "Virtualenv successfully activated in internal python process for " (buffer-file-name))))

  (python-goodies-turn-on-ropemacs) ;;something repeatedly calls pymacs-load "ropemacs" so you have to switch it back on
))

(add-hook 'inferior-python-mode-hook (lambda ()
  ;; jump to the bottom of the comint buffer if you start typing
  (make-local-variable 'comint-scroll-to-bottom-on-input) 
  (setq comint-scroll-to-bottom-on-input t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-shell-send-setup-code-to-process (process)
  "Gallina's python-shell-send-setup-code doesn't allow a buffer
argument"
  ;; (flet ((get-buffer-process (lambda (name) process)))
  (let* ((orig (symbol-function 'get-buffer-process))
         (fn (lambda (_) process)))
    (fset 'get-buffer-process fn)
    (ignore-errors (python-shell-send-setup-code))
    (fset 'get-buffer-process orig)
    process))

(defun check-for-readline (process)
  "return 't if we can import readline.  if we can't autocomplete will be silently broken"
  (let ((repl-out (python-shell-send-string-no-output "import readline;''" process)))
    (if (string-match "^\\(>>> \\)*''" repl-out) 't nil)))

(defun check-for-virtualenv (process)
  "return 't if this process is a virtualenv."
  (let ((repl-out (python-shell-send-string-no-output "import sys;  hasattr(sys, 'real_prefix')" process)))
    (string= repl-out "True")))

;; This equivalent function doesn't exist in Gallina's code
(defun python-get-named-else-internal-process ()
  "return the current global process if there is one.  Otherwise,
start an internal process and return that."
  (let ((process (or (python-shell-get-process)
                     (python-shell-internal-get-or-create-process))))                
    process))

(defun python-just-source-file (filename process)
  "Force process to evaluate filename but don't run __main__.
   Wraps Gallina's python-shell-send-buffer to let us specify both filename and process"
  (defadvice python-shell-send-string (around psss-adapter activate)
    "always pass in a second argument 'process' that's defined in the
     caller's environment"
        (if (boundp 'adv-process) (ad-set-arg 1 adv-process))
        ad-do-it)
  (with-temp-buffer
    (if (ignore-errors (insert-file-contents filename)) (progn
        (let ((adv-process process))
          (python-shell-send-string
           (concat "import sys; sys.path.append('" (detect-package-directory filename)  "')")
           process)
          ;;advice affects python-shell-send string inside this function
          (python-shell-send-buffer))
       ))
    ;;now clean up our advice
    (ad-unadvise 'python-shell-send-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPDB
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;from http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ 	]*import pdb; pdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenv support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom auto-detect-virtualenv nil
  "When loading a python file attempt to find its virtualenv using function detect-virtualenv.")

(defcustom current-virtualenv "/usr/bin/python"
  "Open python files using this virtualenv"
  :type 'file)

(defcustom ipython-use-with-virtualenv nil
  "Set up python-shell-interpreter-args-var correctly to
use ipython with the current virtualenv")


(defconst bin-python-dir
  (if (eq system-type 'windows-nt) "/Scripts/" "/bin/")
  "root directory of the python executable based on emacs architecture")

(defconst bin-python
  (concat bin-python-dir (if (eq system-type 'windows-nt)  "python.exe" "python"))
  "path to the python executable based on emacs architecture")

(defun set-current-virtualenv (dir)
  (interactive "D")
  (setq current-virtualenv (expand-file-name dir))
  't)

(defun reset-current-virtualenv ()
  (interactive)
  (setq current-virtualenv (default-value 'current-virtualenv))
  ;;TODO run virtualenv-hook here
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
              (if (file-regular-p (concat dir bin-python))
                  dir nil))
                 subdirs)))
            (virtualenv-dir (car subdirs-that-have-bin/python)))
       (if virtualenv-dir (progn
         (message (concat "Found " virtualenv-dir " as virtualenv for " filename))
         virtualenv-dir) nil)))) nil))

(defun detect-package-directory (filename)
  "return first parent directory that does not contain __init__.py"
  (ldf-compat (file-name-directory filename)
     (lambda (cd)
       (if (file-exists-p (concat cd "__init__.py"))
           nil cd))))

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

(defun virtualenv-hook ()
  "This should be run in python-mode-hook before any comints are
run"
  (let* ((used-virtualenv
          (cond (auto-detect-virtualenv
                 (detect-virtualenv (buffer-file-name)))
                (current-virtualenv current-virtualenv)
                ('t
                 python-shell-virtualenv-path))))
    (setq python-shell-virtualenv-path used-virtualenv)
    ;; doesn't work because pymacs has already been called at this point
    ;; (make-local-variable 'pymacs-python-command) (setq pymacs-python-command (concat used-virtualenv bin-python))
    (if (file-exists-p (concat used-virtualenv bin-python-dir "activate_this.py")) (progn
      (setq virtualenv-activate-command
            (concat "af = \"" used-virtualenv bin-python-dir "activate_this.py\"; execfile(af, dict(__file__=af))\n"))
      (add-to-list 'python-shell-setup-codes 'virtualenv-activate-command))
      ;;else
        (message "Defaulting to global python installation for pymacs/rope")
      (if ipython-use-with-virtualenv
          (setq python-shell-interpreter-args
              (concat "-u " (expand-file-name "ipython-script.py" (format "%s/%s" used-virtualenv virtualenv-bin-dir))))))))

(defadvice python-eldoc--get-doc-at-point (around python-eldoc--get-doc-at-point-around activate)
  (let ((force-process (python-get-named-else-internal-process)))
    ad-do-it))

(defun set-virtualenv-in-rope-config (rope-config-filename virtualenv-dir)
  (let* ((activate-script-name (concat virtualenv-dir bin-python-dir "activate_this.py"))
        (execfile-line (concat "    execfile(\"" activate-script-name
                               "\", dict(__file__=\"" activate-script-name "\"))")))
    (with-temp-buffer
      (insert-file-contents rope-config-filename)
      (goto-char (point-min))
      (if (re-search-forward execfile-line nil 't 1)
          'exists (progn
          (let ((is-rope-config-file (re-search-forward "^def set_prefs(prefs):" nil 't)))
            (if (not is-rope-config-file)
                (error (concat "file " rope-config-filename " isn't a rope project file"))))
          (next-line)
          (next-line)
          (move-beginning-of-line nil)
          (delete-region (point) (save-excursion (end-of-line) (point)))
          (insert execfile-line)
          (write-file rope-config-filename)
           'modified
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pymacs-reload-rope () 
    "Reload rope"
    (interactive)
    (flet ((yes-or-no-p (&optional arg) 't))
      (pymacs-terminate-services)
      (pymacs-load "ropemacs" "rope-")))

(defun rope-set-virtualenv ()
  "add virtualenv setup to rope project"
  (interactive)
  (defun find-rope-config-file ()
    "grab the argument to find-file by redefining it in rope-project-config's context"
    (let ((filename nil))
      (flet ((find-file (arg) (setq filename arg)))
        (rope-project-config))
      filename))
  (let ((set? (set-virtualenv-in-rope-config (find-rope-config-file) python-shell-virtualenv-path)))
    (if (eq set? 'modified) (progn
      (print (concat "virtualenv " python-shell-virtualenv-path " reset in rope project config, restarting pymacs."))
      (pymacs-reload-rope)))
    't))

(defun python-goodies-turn-on-ropemacs ()
  (interactive)
  (setq ropemacs-enable-shortcuts 'nil) ;;otherwise this overwrites M-/ and M-?
  (setq ropemacs-enable-autoimport 't)
  (setq ropemacs-autoimport-modules `("os" "shutil"))
  (if (not (boundp 'ropemacs-mode)) (pymacs-load "ropemacs" "rope-"))
  (if (and (boundp 'ropemacs-mode) (not ropemacs-mode)) (ropemacs-mode))
  ;; hook rope into auto-complete - this slows down everything so it's disabled for now
  ;; (ac-ropemacs-initialize)
  ;; (add-to-list 'ac-sources 'ac-source-ropemacs)
)

(defun python-goodies-rope-goto-definition ()(interactive) (push-current-location) (rope-goto-definition)) 
(defun python-goodies-rope-go-backward () (interactive) (pop-current-location))
(defun python-goodies-python-send-buffer ()
  (interactive)
  ;;refresh the internal process
  (python-shell-send-string
   (concat "import sys; sys.path.append('" (detect-package-directory (buffer-file-name))  "')")
   (python-shell-get-or-create-process))
  (python-just-source-file (buffer-file-name) (python-shell-internal-get-or-create-process))
  (python-shell-send-buffer)
  (python-goodies-python-shell-smart-switch))

(defun python-goodies-python-shell-smart-switch ()
  (interactive)
  (let ((saved-point (point))
	(saved-frame (selected-frame))
	(saved-window (selected-window)))
    (if (string= (python-shell-get-process-name t) "Python") (end-of-buffer) ;;we are in the inferior buffer
      (let ((display-buffer-reuse-frames t)) (python-shell-switch-to-shell) (end-of-buffer)))
    (if (and (eq saved-point (point))
             (eq saved-frame (selected-frame))
             (eq saved-window (selected-window))) ;;nothing moved - we're at the end of the inferior buffer
        (progn
          (raise-frame my-python-most-recent-frame)
          (select-window my-python-most-recent-window))
      (if (not (eq saved-window (selected-window))) ;;moved to a different window
          (progn (setq my-python-most-recent-frame saved-frame)
                 (setq my-python-most-recent-window saved-window)
                 )))))

(defun python-goodies-restart-python-repl () (interactive)
  (let ((process (python-shell-get-or-create-process))
        (in-repl (eq major-mode 'inferior-python-mode)))
    (if in-repl (other-window 1))
    (python-shell-send-string "quit()" process)
    (sleep-for 0.1)
    (python-shell-get-or-create-process)
    (if in-repl (sleep-for 0.1) (other-window 1) (end-of-buffer))
))

;; I never want run-python to ask me for a path
 (defun run-python (&optional a b) (interactive "ii")
   (python-shell-make-comint (python-shell-parse-command) (python-shell-get-process-name nil) t))

(defun python-goodies-eval-line ()
  "Evaluate the current Python line in the inferior Python process."
  (interactive) 
  (python-shell-send-string
   (buffer-substring-no-properties (point) (line-end-position))
   (python-get-named-else-internal-process)))

(defun ipython-eval-region (start end)
  "Send the region delimited by START and END to inferior ipython process."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (python-shell-send-string "%paste" nil t))



(provide 'python-goodies)
