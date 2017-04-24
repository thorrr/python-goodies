;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-switch-to-internal-process ()
  (interactive)
  (switch-to-buffer (concat " *" (python-shell-internal-get-process-name) "*")))

(defun python-goodies/python-send-buffer ()
  (interactive)
  (run-python)
  ;; add the top level package to sys.path
  (send-package-directory (buffer-file-name))
  ;; now source the entire file verbatim into the visible repl
  (python-shell-send-buffer)
  (python-goodies/python-shell-smart-switch))

(defun python-goodies/python-shell-smart-switch ()
  (interactive)
  (let ((saved-point (point))
	(saved-frame (selected-frame))
	(saved-window (selected-window)))
    (if (string= (python-shell-get-process-name t) "Python") (end-of-buffer) ;;we are in the inferior buffer
      (let ((display-buffer-reuse-frames t))
        ;; the following isequivalent to (python-shell-switch-to-shell) but ACTION=nil so
        ;; we don't split into a new buffer, which is annoying
        (pop-to-buffer (process-buffer (python-shell-get-or-create-process)))
        ;;and set the cursor at the first repl line
        (end-of-buffer)))
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

(defun python-goodies/restart-python-repl ()
  (interactive)
  (let ((process (python-shell-get-or-create-process))
        (in-repl (eq major-mode 'inferior-python-mode)))
    (if in-repl (other-window 1))
    (python-shell-send-string "quit()" process)
    (sleep-for 0.1)
    (python-shell-get-or-create-process)
    (if in-repl (sleep-for 0.1) (other-window 1) (end-of-buffer))
))

(defun python-goodies/eval-line ()
  "Evaluate the current Python line in the inferior Python process."
  (interactive)
  (python-shell-send-string
   (buffer-substring-no-properties (point) (line-end-position))
   (run-python)))

(defun ipython-eval-region (start end)
  "Send the region delimited by START and END to inferior ipython process."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (python-shell-send-string "%paste" nil t))
