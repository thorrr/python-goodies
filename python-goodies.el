(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python specific keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
  (define-key python-mode-map (kbd "C-M-<return>") 'my-python-send-buffer)
  (define-key python-mode-map (kbd "M-.") 'my-rope-goto-definition)
  (define-key python-mode-map (kbd "M-,") 'my-rope-go-backward)
  (define-key python-mode-map (kbd "M-i") 'my-python-shell-smart-switch)
  (define-key python-mode-map (kbd "C-c C-j") 'my-python-eval-line)
  (define-key python-mode-map (kbd "S-<f4>") 'my-restart-python)
))

(add-hook 'inferior-python-mode-hook (lambda ()
  (define-key inferior-python-mode-map (kbd "M-i") 'my-python-shell-smart-switch)
  (define-key inferior-python-mode-map [f9] 'my-python-show-graphs)
  (define-key inferior-python-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key inferior-python-mode-map [up] 'comint-previous-matching-input-from-input)
  (define-key inferior-python-mode-map [f4] 'my-restart-python)
))

(add-hook 'ropemacs-mode-hook (lambda ()
  (define-key ropemacs-local-keymap (kbd "M-?") 'ac-start)
  (define-key ropemacs-local-keymap (kbd "M-/") 'hippie-expand)
))

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
      python-shell-interpreter (car (get 'python-shell-interpreter 'standard-value))
      python-shell-interpreter-args (car (get 'python-shell-interpreter-args 'standard-value))
      python-shell-prompt-regexp (car (get 'python-shell-prompt-regexp 'standard-value))
      python-shell-prompt-output-regexp (car (get 'python-shell-prompt-output-regexp 'standard-value))
      python-shell-completion-setup-code (car (get 'python-shell-completion-setup-code 'standard-value))
      python-shell-completion-module-string-code (car (get 'python-shell-completion-module-string-code 'standard-value))
      python-shell-completion-string-code (car (get 'python-shell-completion-string-code 'standard-value))
      ) 't)
        ('t (error "python-shell-setup must be called with 'python or 'ipython"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pymacs)
(setq pymacs-auto-restart t)

(defun my-turn-on-ropemacs () (interactive)
  (setq ropemacs-enable-shortcuts 'nil) ;;otherwise this overwrites M-/ and M-?
  (setq ropemacs-enable-autoimport 't)
  (setq ropemacs-autoimport-modules `("os" "shutil"))
  (if (not (boundp 'ropemacs-mode)) (pymacs-load "ropemacs" "rope-"))
  (if (and (boundp 'ropemacs-mode) (not ropemacs-mode)) (ropemacs-mode))
)

;; regenerate the import cache whenever you open a project.  this can
;; be slow the first time
(defadvice rope-open-project (after rope-open-project-then-regenerate-import-cache activate)
  (rope-generate-autoimport-cache))

;; Autocomplete
(require 'ac-python) ;; a source for python auto-complete that comes from the
                     ;; *Python* buffer or the unnamed "internal" process
(ac-ropemacs-initialize) ;; hook rope into auto-complete

;; Flymake
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
(defun flymake-pyflakes-init ()
  (if (executable-find "pyflakes")
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                         'flymake-create-temp-inplace)) 
             (local-file (file-relative-name 
                          temp-file 
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))
    (progn (message "Warning:  pyflakes executable not found") nil)
    ))

(add-hook 'python-mode-hook (lambda ()
  ;;modify pyflakes' output
  ;; use \\| to separate multiple match criteria
  (setq flymake-warn-line-regexp "imported but unused\\|unable to detect undefined names")
  (setq flymake-info-line-regexp "is assigned to but never used")))

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks - loading a python file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook (lambda ()
  (set-variable 'python-indent-offset 4)
  (set-variable 'indent-tabs-mode nil)
  (setq ropemacs-enable-autoimport t)
  (add-to-list 'ac-sources 'ac-source-python)
  ;;TODO - make symbolName : packagea.packageb.packageC trigger an import statement
  (add-to-list 'ac-sources 'ac-source-ropemacs)
  ;; (add-to-list 'ac-sources 'ac-source-yasnippet)
  
  (python-shell-setup python-inferior-shell-type)
  ;; An Internal Process is created for each unique configuration.
  ;; Set up each file's virtualenv before calling python-just-source-file so that each virtualenv
  ;; will have a single internal process
  (virtualenv-hook)
  (python-just-source-file
   (buffer-file-name)
   (python-shell-send-setup-code-to-process (python-shell-internal-get-or-create-process)))
  (project-root-fetch)
  (setq ropemacs-guess-project (cdr project-details));;get all of the python subdirectories
  (local-set-key [S-f10] 'my-python-run-test-in-inferior-buffer)
  (local-set-key [f10] 'my-python-toggle-test-implementation)
  (my-turn-on-ropemacs) ;;something repeatedly calls pymacs-load "ropemacs" so you have to switch it back on
  (autopair-mode)
  (setq autopair-handle-action-fns '(autopair-default-handle-action
                                     autopair-dont-if-point-non-whitespace
                                     autopair-python-triple-quote-action))
))



(add-hook 'inferior-python-mode-hook (lambda ()
  ;; jump to the bottom of the comint buffer if you start typing
  (make-local-variable 'comint-scroll-to-bottom-on-input) 
  (setq comint-scroll-to-bottom-on-input t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;wrapper to make ac-python work with Gallina's python.el
(defun python-symbol-completions (symbol)
  (let* ((process (python-get-named-else-internal-process))
         ;;this breaks the abstraction in ac-python (it's defined without referencing the cursor position
         ;;but i don't feel like changing ac-python right now
         (current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         ;; have to eliminate leading tabs/spaces
         (curline (replace-regexp-in-string "\\(^[[:space:]]*\\)" "" current-line))
         (psc (python-shell-completion--get-completions curline process python-shell-completion-string-code)))
    psc))

;; This equivalent function doesn't exist in Gallina's code
(defun python-get-named-else-internal-process ()
  "return the current global process if there is one.  Otherwise,
start an internal process and return that."
      (if (< emacs-major-version 24)
      (defun process-live-p (process)
        "Returns non-nil if PROCESS is alive.
         A process is considered alive if its status is `run',
         `open',`listen', `connect' or `stop'. Value is nil if PROCESS is
          not a process."
        (and (processp process)
             (memq (process-status process)
                   '(run open listen connect stop)))))
      (let* ((global-process (python-shell-get-process))
             (internal-process-state (process-live-p (python-shell-internal-get-process-name)))
             (internal-process (if internal-process-state (get-process (python-shell-internal-get-process-name))
                                 nil))
             (existing-process (if global-process global-process internal-process))
             (process (if (not existing-process) (progn
                          (message "Starting inferior, unnamed Python process.")
                          (python-shell-send-setup-code-to-process (python-shell-internal-get-or-create-process)))
                        existing-process)))
        process))



(defun my-rope-goto-definition ()(interactive) (push-current-location) (rope-goto-definition)) 
(defun my-rope-go-backward () (interactive) (pop-current-location))
(defun my-python-send-buffer () (interactive) (python-shell-send-buffer) (my-python-shell-smart-switch))
(defun my-python-shell-smart-switch ()
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

(defun my-restart-python () (interactive)
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

(defun my-python-eval-line ()
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

;; (defun python-shell-send-region (start end)
;;   "Overridden.  Use the %paste IPython method to send copied regions to the inferior Python process."
;;   (interactive "r")
;;   (my-python-eval-region start end))

(defun python-just-source-file (filename &optional process)
  "Force process to evaluate filename but don't run __main__.
   Gallina has a similar technique for evaluating buffers in
   python-shell-send-buffer.  But his doesn't allow us to specify
   an internal Python process"
  (let ((command-string-1 "___oldName = __name__")
        (command-string-2 "__name__ = None")
        (command-string-3
         (concat "execfile( \"" filename "\", globals())"))
        (command-string-4 "__name__ = ___oldName"))
    (python-shell-send-string command-string-1 process)
    (python-shell-send-string command-string-2 process)
    (python-shell-send-string command-string-3 process)
    (python-shell-send-string command-string-4 process)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPDB
;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;from http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ 	]*import ipdb; ipdb.set_trace()"))
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

(defun set-current-virtualenv (dir)
  (interactive "D")
  (setq current-virtualenv (expand-file-name dir))
  't)

(defun reset-current-virtualenv ()
  (interactive)
  (setq current-virtualenv (default-value 'current-virtualenv))
  't)

(defun detect-virtualenv (filename)
  "resets variable current-virtualenv if it can detect this
  python file has a virtualenv in its path"
  (ldf-compat (file-name-directory filename)
   (lambda (cd)
     (let* ((dirlist `("." ".." "bin"  "include" "lib"))
            (ls (mapcar (lambda (f) (concat cd f)) (directory-files cd)))
            (subdir-p (lambda (dir) (if (file-directory-p dir) dir nil)))
            (subdirs (delq nil (mapcar subdir-p ls)))
            (subdir-ls (mapcar (lambda (dir) (ignore-errors (directory-files dir))) subdirs))
            (subdir-match (mapcar (lambda (subdir-ls-elem)
              (if (equal (sort subdir-ls-elem 'string=) (sort dirlist 'string=)) 't nil)) subdir-ls))
            (l (length (memq t subdir-match)))
            (vd (if (zerop l) nil (nth (- (length subdirs) l) subdirs)))
            (virtualenv-dir (if vd (file-name-as-directory vd) nil)))
       (if virtualenv-dir (progn
         (message (concat "Found " virtualenv-dir " as virtualenv for " filename))
         virtualenv-dir) nil)))))

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
                 python-shell-virtualenv-path)))
         (cygpath-output (shell-command-to-string (concat  "cygpath -u " used-virtualenv)))
         (cygwin-path (replace-regexp-in-string "\\\n" "" cygpath-output))
         )
    (setq python-shell-virtualenv-path used-virtualenv)
    (if (eq system-type 'windows-nt) (progn
        (setq-local python-shell-completion-setup-code
          (concat python-shell-completion-setup-code "\n"
                  "execfile(\"" used-virtualenv "bin/activate_this.py\", dict(__file__=\"" used-virtualenv "bin/activate_this.py\"" "))" ))
        ;;we can't use python-shell-extra-pythonpaths because path-separator breaks on cygwin python
        (setenv "PYTHONPATH" (concat cygwin-path "lib/python2.7/site-packages" path-separator (getenv "PYTHONPATH")))))
    (if ipython-use-with-virtualenv
        (setq python-shell-interpreter-args
              (concat "-u " (expand-file-name "ipython-script.py" (format "%s/%s" used-virtualenv virtualenv-bin-dir)))))))

(defadvice python-eldoc--get-doc-at-point (around python-eldoc--get-doc-at-point-around activate)
  (let ((force-process (python-get-named-else-internal-process)))
    ad-do-it))

(defun python-shell-send-setup-code-to-process (process)
  "Gallina's python-shell-send-setup-code doesn't allow a buffer
argument"
  (let* ((orig (symbol-function 'get-buffer-process))
         (fn (lambda (_) process)))
    (fset 'get-buffer-process fn)
    (python-shell-send-setup-code)
    (fset 'get-buffer-process orig)
    process))


(provide 'python-goodies)
