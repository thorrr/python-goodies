;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
;; run these once globally since they're slow.  Must restart emacs when you install a new
;; syntax checker.
(let* ((exe (if (eq system-type 'windows-nt) ".exe" "")))
  (setq python-goodies/pyflakes-exists (if (executable-find (concat "pyflakes" exe)) 't nil))
  (setq python-goodies/pep8-exists (or (executable-find (concat "pycodestyle" exe))
                                       (executable-find (concat "pep8" exe))))
  (setq python-goodies/pylint-exists (if (executable-find (concat "pylint" exe)) 't nil))

  (setq python-goodies/pep8-exe (cond
                  ;; prefer pycodestyle if it's installed
                  ((executable-find (concat "pycodestyle" exe)) "pycodestyle")
                  ((executable-find (concat "pep8" exe)) "pep8")
                  ('t nil)))

  (if (and python-use-pyflakes (not python-goodies/pyflakes-exists))
      (message "Warning:  pyflakes executable not found"))
  (if (and python-use-pep8 (not python-goodies/pep8-exists))
      (message "Warning:  pycodestyle or pep8 executable not found"))
  (if (and python-use-pylint (not python-goodies/pylint-exists))
      (message "Warning:  pylint executable not found")))

(defun python-goodies/filter-star-builtins (filename)
  "eliminate `from builtins import *` from python file so flymake
   isn't triggered by it"
  (with-temp-file filename
    (insert-file-contents filename)
    (while (re-search-forward "^from +builtins +import +\\*" nil 't)
      (replace-match "" nil nil)))
  filename)

(defun flymake-python-build-cmd-line ()
  ;; relies on global variables: pyflakes-exists, pep8-exists, pylint-exists
  (let ((use-pyflakes (and python-use-pyflakes python-goodies/pyflakes-exists))
        (use-pep8 (and python-use-pep8 python-goodies/pep8-exists))
        (use-pylint (and python-use-pylint python-goodies/pylint-exists)))
    (if (or use-pyflakes use-pep8 use-pylint)
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (temp-file (python-goodies/filter-star-builtins temp-file))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name)))
               (pep8-options-list (split-string python-pep8-options))
               (pylint-options-list (split-string python-pylint-options))
               
               (virtualenv-python (concat python-shell-virtualenv-root (bin-python)))
               ;; bind local var pylint-in-venv so we don't do call-process again and again
               (pylint-installed-in-virtualenv (or (bound-and-true-p python-goodies/_pylint-in-venv)
                   ;; python-shell-virtualenv-root will be non-nil if we're in a virtualenv
                     (if python-shell-virtualenv-root
                         ;; return our local variable if we've already checked
                         (if (boundp 'python-goodies/_pylint-in-venv) python-goodies/_pylint-in-venv
                           (if (zerop (call-process virtualenv-python nil nil nil python-goodies/pylint-script "-h"))
                               (setq-local python-goodies/_pylint-in-venv 't)
                             (if use-pylint (message (format
                                 "Warning:  pylint not installed in virtualenv %s; package imports won't be detected"
                                 python-shell-virtualenv-root)))
                             (setq-local python-goodies/_pylint-in-venv nil)
                             'not-installed-in-virtualenv)))))
               ;; use system python if pylint isn't in the virtualenv
               (pylint-exe (if (eq pylint-installed-in-virtualenv 't)
                               virtualenv-python
                             ;; else: windows doesn't have python2/3.exe, simply
                             ;; "python.exe"
                             (if (eq system-type 'windows-nt) "python"
                               (concat "python" (format "%d" python-major-version)))))
               ;; Finally, build a command that runs any combination of pyflakes, pep8 and
               ;; pylint.  First argument is the shell to run: bash or cmd.  Second
               ;; argument is a list of arguments to the shell.  For bash it _must_ have
               ;; exactly two elements: "-c" and a single string with the subcommand to
               ;; run.  Don't surround it in single quotes.  The resulting process is
               ;; equivalent to doing the following on the command line:
               ;;
               ;; bash -c ' ( pyflakes common_flymake.py ; pep8 common_flymake.py ) '
               (shell (if (eq system-type 'windows-nt) "cmd" "bash"))
               (cmd-switch (if (eq system-type 'windows-nt) "/c" "-c"))
               (cmd-sep (if (eq system-type 'windows-nt) "&" ";"))
               (rv (list shell
                 `(,cmd-switch
                   ;; use mapconcat to build a string with spaces in between arguments
                   ,(mapconcat 'identity `(
                     ,@(if (not (eq system-type 'windows-nt)) '("( "))
                     ;; pyflakes command
                     ,@(if use-pyflakes `("pyflakes" ,local-file))
                     ;; separate if there was a previous command
                     ,@(if (and use-pep8 use-pyflakes) `(,cmd-sep))
                     ;; pep8 command
                     ,@(if use-pep8 `(,python-goodies/pep8-exe ,@pep8-options-list
                                      ,(concat "--max-line-length=" (format "%d" python-column-width)) ,local-file))
                     ;; separate again - check for any previous command
                     ,@(if (and use-pylint (or use-pyflakes use-pep8)) `(,cmd-sep))
                     ;; pylint command - virtualenv friendly
                     ,@(if use-pylint `(;; equivalent to 'python $(where pylint)' inside virtualenv
                                        ,pylint-exe ,python-goodies/pylint-script
                                                    ,(concat " --max-line-length=" (format "%d" (+ 1 python-column-width)))
                                                    ,@pylint-options-list ,local-file))
                     ;; properly wrap the combined command
                     ,@(if (not (eq system-type 'windows-nt)) '(" ; )"))
                     ) " ")))))
          rv)
      (progn (message
              "Warning:  flymake won't run because neither pyflakes nor pep8 nor pylint were selected") nil))))

;; create pylint file to run inside virtualenv - I can't figure out how to escape this
;; under 'cmd /c python -c'
(setq python-goodies/pylint-script (make-temp-file "pylint"))
(with-temp-file python-goodies/pylint-script
  (insert "from pylint import run_pylint;\
               import sys;\
               sys.exit(run_pylint())"))

(add-hook 'python-mode-hook (lambda ()
  ;;modify pyflakes' output
  ;; use \\| to separate multiple match criteria
  (setq flymake-warn-line-regexp "imported but unused\\|unable to detect undefined names\\|E[0-9]+")
  (setq flymake-info-line-regexp "is assigned to but never used\\|W[0-9]+")))

(add-hook 'python-mode-hook
          (lambda ()
            (if (or python-goodies/pyflakes-exists
                    python-goodies/pep8-exists
                    python-goodies/pylint-exists)
                (add-to-list 'flymake-allowed-file-name-masks
                             '("\\.py\\'" flymake-python-build-cmd-line)))))
