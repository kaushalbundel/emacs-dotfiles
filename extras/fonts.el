(set-face-attribute 'default nil :family "JetBrains Mono" :weight 'Regular)

(use-package nerd-icons
  :ensure t
  :defer 5)

(use-package nerd-icons-completion
  :ensure t
  :defer 5)
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1)

(use-package nerd-icons-corfu
  :ensure t
  :defer 5)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(use-package nerd-icons-dired
  :ensure t
  :defer 5)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;;doom mode line

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t))


