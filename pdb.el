;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDB
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adapted from http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  ;; TODO:  autodetect ipdb and change this line accordingly
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ 	]*import i?pdb; i?pdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
