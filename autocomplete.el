;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ac-python-async:get-completion-process 'python-goodies/get-or-start-completion-process)

(require 'ac-python-async) ;; a source for python auto-complete that comes from the
                           ;; *Python* buffer or the unnamed "internal" process


(add-hook 'python-mode-hook (lambda ()
  ;; by default emacs uses the dictionary and other buffers as
  ;; completion sources which is super lame in practice.  Turn this off.
  (setq ac-sources (delete 'ac-source-abbrev ac-sources))
  (setq ac-sources (delete 'ac-source-dictionary ac-sources))
  (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
  (if (not (boundp 'python-goodies/_yasnippet-started)) (progn
    (setq python-goodies/_yasnippet-started nil)
    (require 'yasnippet)
    (yas-reload-all)))
  (yas-minor-mode)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
))
