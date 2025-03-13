(set-face-attribute 'default nil :family "Iosevka Comfy Motion Fixed" :weight 'Regular)

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

;; package doom modelline to emhance the mode line 
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
