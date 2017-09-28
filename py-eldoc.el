;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
;; turn on eldoc properly using the internal process
(defadvice python-eldoc--get-doc-at-point (around python-eldoc--get-doc-at-point-around activate)
  (let ((force-process (python-goodies/get-or-start-completion-process)))
    ad-do-it))

