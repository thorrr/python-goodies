;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python specific keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook (lambda ()
  (define-key python-mode-map (kbd "C-M-<return>") 'python-goodies/python-send-buffer)
  (if pymacs-parent-dir (progn
    (define-key python-mode-map (kbd "M-.") 'python-goodies/rope-goto-definition)
    (define-key python-mode-map (kbd "M-,") 'python-goodies/rope-go-backward)))
  (define-key python-mode-map (kbd "M-i") 'python-goodies/python-shell-smart-switch)
  (define-key python-mode-map (kbd "C-c C-j") 'python-goodies/eval-line)
  (define-key python-mode-map (kbd "S-<f4>") 'python-goodies/restart-python-repl)
))

(add-hook 'inferior-python-mode-hook (lambda ()
  (define-key inferior-python-mode-map (kbd "M-i") 'python-goodies/python-shell-smart-switch)
  (define-key inferior-python-mode-map [down] 'comint-next-matching-input-from-input)
  (define-key inferior-python-mode-map [up] 'comint-previous-matching-input-from-input)
  (define-key inferior-python-mode-map [f4] 'python-goodies/restart-python-repl)
))

(add-hook 'ropemacs-mode-hook (lambda ()
  (define-key ropemacs-local-keymap (kbd "M-?") 'ac-start)
  (define-key ropemacs-local-keymap (kbd "M-/") 'hippie-expand)
  ;;add menu item to Rope menu
  (define-key-after (lookup-key ropemacs-local-keymap [menu-bar Rope])
    [setup-virtualenv] '("Setup Virtualenv" . rope-set-virtualenv) 'rope-set-virtualenv)
))
