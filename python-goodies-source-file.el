;;; source-file.el --- functions to read python files into the live process  -*- lexical-binding: t -*-

(defun python-destroy-side-effects-in-buffer ()
  "In the current buffer get rid of any code that potentially can
lead to side effects.  Only top level keywords plus the following
scope or MyClass = namedtuple(...) are allowed."
  (let ((keywords
         ;; must have spaces after keyword so
         ;; defaultString = "foo"
         ;; isn't picked up
         '("def " "class " "from " "import " "@"
           ;; allow classes to be defined using namedtuple:
           ;; Point = namedtuple('Point', ['x', 'y'])
           "\\([[:alpha:]]\\|_\\)\\([[:alnum:]]\\|_\\)* *= *\\(collections\\.\\)?namedtuple *(")))
    (let ((more-lines 't)
          (side-effect-start nil))
      (goto-char (point-min))
      ;; first, get rid of if __name__ == "__main__":
      (save-excursion
        (when (python-nav-if-name-main)
          (delete-region (point) (progn
            (python-nav-forward-sexp-safe)
            (forward-line 1)
            (point)))))
      ;; now, iterate through the buffer line-by-line
      (while more-lines
        (let ((is-start-of-line-token (looking-at "[[:alpha:]]"))
              (is-keyword (member 't (mapcar (lambda (keyword) (looking-at keyword)) keywords))))
          ;; detect the state of the current line and act
          (cond ;; start deleting - we're seeing a side effect
                ((and is-start-of-line-token (not is-keyword) (not side-effect-start))
                 (setq side-effect-start (point)))
                ;; stop deleting - we're inside a side effect but we've hit the next keyword
                ((and is-start-of-line-token is-keyword side-effect-start)
                 (delete-region side-effect-start (point))
                 (setq side-effect-start nil))))
        ;;now move cursor past any triple quotes that start on this line
        (if (looking-at ".*?\"\"\"") (progn
              (re-search-forward ".*?\"\"\"" nil)
              (re-search-forward ".*?\"\"\"" nil 'eof)))
        (if (looking-at ".*?'''") (progn
              (re-search-forward ".*?'''" nil)
              (re-search-forward ".*?'''" nil 'eof)))
        ;;then bump to next line
        (setq more-lines (= 0 (forward-line 1))))
      ;;finally, delete the last region if we saw something before EOF
      (if side-effect-start
          (delete-region side-effect-start (point-max)))
      )))

(defun python-just-source-file (filename process)
  "Force process to evaluate filename but don't run __main__ or any other code that can have side effects.
   Wraps Gallina's python-shell-send-buffer to let us specify
   both filename and process"
  (if (not process) (message (concat "warning:  internal process doesn't exist for" filename "; not sourcing"))
    (if (not (file-exists-p filename))
        (message (concat "INFO:  not sourcing " filename " because it hasn't been saved yet."))
      (progn
        (message-no-echo (format "Sourcing %s into %s" filename process))
        (send-package-directory filename process)
        (let ((prog-string
               (with-temp-buffer (progn
                 (insert-file-contents filename)
                 (python-destroy-side-effects-in-buffer)
                 (buffer-string)))))
          (python-shell-send-string prog-string process)))))
  't)

(defun python-source-file-to-completion-process (filename)
  ;; our "get process" function must be called from the buffer itself
  (with-current-buffer (get-file-buffer filename)
    "send a file to the internal process with the proper directory setup code"
    (let ((completion-process (python-goodies/get-or-start-completion-process))
          ;; nasty hack that's not needed on emacs 25 anymore
          (python-shell-send-setup-code-to-process (lambda (process)
            "Gallina's python-shell-send-setup-code doesn't allow a process argument"
            (cl-letf* (((symbol-function 'get-buffer-process) (lambda (_) process)))
              (python-shell-send-setup-code)
              process))))
      (if (not completion-process) (message (format "Warning - completion process is nil for %s" filename))
        (if (< emacs-major-version 25)
            (funcall python-shell-send-setup-code-to-process completion-process))
        (send-package-directory filename completion-process)
        ;; now send the actual code inside filename
        (python-just-source-file filename completion-process)))))

(add-hook 'python-mode-hook (lambda ()
  ;; source on a timer
  ;; (if auto-python-just-source-file
  ;;     (run-with-idle-timer 2 't 'python-source-file-to-completion-process (buffer-file-name)))
  ;;  or after saves
  (if auto-python-just-source-file
      (add-hook 'after-save-hook (lambda () (python-source-file-to-completion-process (buffer-file-name)))
                nil 't))))
  

(provide 'python-goodies-source-file)
