;; setting fonts for mac and linux
(cond
 ((equal system-type 'darwin)
  (set-face-attribute 'default nil :family "Aporetic Sans Mono" :weight 'Regular :height 100)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Serif Mono" :weight 'Regular :height 100))
 ((equal system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 110)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Serif Mono" :height 100))
 ((equal system-type 'windows-nt)
  (set-face-attribute 'default nil :family "JetBrains Mono NL" :weight 'Regular :height 90)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Motion Fixed" :weight 'Regular :height 90)))

(use-package nerd-icons
  :ensure t
  :defer 5)

(use-package nerd-icons-completion
  :ensure t
  :defer 5)
(nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode 1)

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
