;;; hide-show.el --- apply Git diffs  -*- lexical-binding: t -*-
;; add the 'Hide All defs' menu item if we're in hide-show mode
(defun hide-all-defs ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((end-found nil))
      (while (not end-found)
        (setq end-found (not (search-forward-regexp "^ *def" nil 't)))
        (if (not end-found) (progn
          (beginning-of-line)
          (hs-hide-block)
          (forward-line)))))))

(add-hook 'python-mode-hook (lambda ()
  (if (boundp 'hs-minor-mode)
      (define-key-after (lookup-key hs-minor-mode-map [menu-bar Hide/Show])
        [hide-all-defs] '("Hide All defs" . hide-all-defs) 'hide-all-defs))))

(provide 'python-goodies-hide-show)
