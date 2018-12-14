;;;;;;;;;;;;;
;; Pymacs
;;;;;;;;;;;;;

(defun pymacs-setup ()
  (if (not (boundp 'python-goodies/_pymacs-initiated))
      (setq python-goodies/_pymacs-initiated nil))
  (if (and pymacs-parent-dir
           (not python-goodies/_pymacs-initiated)) (progn
    ;; can't use python-shell-extra-pythonpaths because these have to be set before we require 'pymacs
    (setenv "PYTHONPATH" (concat
      (concat pymacs-parent-dir "Pymacs" path-separator)
      (concat pymacs-parent-dir "ropemacs" path-separator)
      (concat pymacs-parent-dir "ropemode" path-separator)
      (concat pymacs-parent-dir "rope")
      (getenv "PYTHONPATH")))
    (require 'pymacs)
    (setq pymacs-auto-restart 't)
    (setq python-goodies/_pymacs-initiated 't) ;;only have to do this once per emacs session
    )))

(defun set-virtualenv-in-rope-config (rope-config-filename virtualenv-dir)
  (let* ((activate-script-name (concat virtualenv-dir bin-python-dir "activate_this.py"))
         (execfile-line (concat "    exec(open(\"" activate-script-name
                                "\").read(), dict(__file__=\"" activate-script-name "\"))")))
    (if virtualenv-dir (with-temp-buffer
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
                    )))
      (message "Warning: virtualenv not set, not changing rope config"))))

(defun python-goodies/add-virtualenv-setup-to-rope-menu ()
    ;;add menu item to Rope menu
  (define-key-after (lookup-key ropemacs-local-keymap [menu-bar Rope])
    [setup-virtualenv] '("Setup Virtualenv" . rope-set-virtualenv) 'rope-set-virtualenv))

(defun pymacs-reload-rope () 
    "Reload rope"
    (interactive)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&optional _) 't)))
      (pymacs-terminate-services)
      (pymacs-load "ropemacs" "rope-")
      (python-goodies/add-virtualenv-setup-to-rope-menu)))

(defun rope-set-virtualenv ()
  "add virtualenv setup to rope project"
  (interactive)
  (virtualenv-hook)
  (let* ((find-rope-config-file (lambda ()
     ;; "grab the argument to find-file by redefining it in rope-project-config's context"
           (let ((filename nil))
             (cl-letf (((symbol-function 'find-file)
                        (lambda (find-file-filename) (setq filename find-file-filename))))
               (rope-project-config))
             (if (eq system-type 'cygwin)
                 (concat "/cygdrive/" (substring filename 0 1) (substring filename 2))
               filename))))
         (set? (set-virtualenv-in-rope-config (funcall find-rope-config-file) python-shell-virtualenv-root)))
    (if (eq set? 'modified) (progn
        (print (concat "virtualenv " python-shell-virtualenv-root 
                       " reset in rope project config, restarting pymacs."))
        (pymacs-reload-rope)))
    't))

(defun python-goodies/turn-on-ropemacs ()
  (interactive)
  (setq ropemacs-enable-shortcuts nil) ;;otherwise this overwrites M-/ and M-?

  ;; rope-auto-import works but M-x rope-generate-autoimport-cache is
  ;; extremely heavy (gigabytes of RAM for quite small projects).
  ;; Turn this off for now
  (setq ropemacs-enable-autoimport nil)
  (setq ropemacs-autoimport-modules `("os" "shutil"))

  (if (not (boundp 'ropemacs-mode)) (pymacs-load "ropemacs" "rope-"))
  (if (and (boundp 'ropemacs-mode) (not ropemacs-mode)) (ropemacs-mode))
  ;; hook rope into auto-complete - this slows down everything so it's disabled for now
  ;; (ac-ropemacs-initialize)
  ;; (add-to-list 'ac-sources 'ac-source-ropemacs)
)

(defun python-goodies/rope-goto-definition ()
  (interactive)
  (xref-push-marker-stack)
  (rope-goto-definition))

(defun python-goodies/rope-go-backward ()
  (interactive)
  (xref-pop-marker-stack))
