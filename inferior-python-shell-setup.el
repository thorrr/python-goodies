;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inferior Python shell setup variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom python-inferior-shell-type 'python
  "Customize inferior shells to be \"python\" or \"ipython\"."
  :type 'symbol
  :options '(python ipython)
  )

(defun python-shell-setup (shell-type)
  (let ((ipython-str (concat "ipython" (format "%d" python-major-version))))
    (if (not (or (eq shell-type 'python) (eq shell-type 'ipython)))
        (error "python-shell-setup must be called with 'python or 'ipython"))
    (if (and (eq shell-type 'ipython) (not (executable-find ipython-str)))
        (progn
          (message "Warning - ipython not installed in python base installation.  Changing shell-type back to 'python")
          (setq python-inferior-shell-type 'python)))

    (if (and (eq shell-type 'ipython) (executable-find ipython-str)) (progn
        (message "Changing python-shell-inferior variables to support ipython")
        (setq python-shell-interpreter ipython-str
              python-shell-interpreter-args "")
        ;; don't need the rest of these for new emacs
        (if (< emacs-major-version 25)
            (setq
             python-shell-prompt-regexp "In \\[[0-9]+\\]: "
             python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
             python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
             python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
             ;; unicode literal gets printed now so wrap the completion in a "print":  from
             ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-07/msg01451.html
             python-shell-completion-string-code (concat
                                                  "print("
                                                  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
                                                  ")\n")
             )))
      ;; else: set regular python mode with some helpful messages if the user wants ipython mode
      (message "Changing python-shell-inferior variables to support python")
      (setq python-shell-interpreter (concat 
                                      (eval (car (get 'python-shell-interpreter 'standard-value)))
                                      (format "%d" python-major-version))
            python-shell-interpreter-args (eval (car (get 'python-shell-interpreter-args 'standard-value))))
      ;; don't need the rest of these for new emacs
      (if (< emacs-major-version 25)
          (setq
           python-shell-prompt-regexp (eval (car (get 'python-shell-prompt-regexp 'standard-value)))
           ;;sometimes prompts "build up" in the inferior processes so filter them out
           python-shell-prompt-output-regexp "\\(>>> \\)*" ;;(eval (car (get 'python-shell-prompt-output-regexp 'standard-value)))
           python-shell-completion-setup-code (eval (car (get 'python-shell-completion-setup-code 'standard-value)))
           python-shell-completion-module-string-code (eval (car (get 'python-shell-completion-module-string-code 'standard-value)))
           python-shell-completion-string-code
           ;; unicode literal gets printed now so wrap the completion in a "print":  from
           ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-07/msg01451.html
           (concat "print("
                   (eval (car (get 'python-shell-completion-string-code 'standard-value)))
                   ")\n"))
        ))))
