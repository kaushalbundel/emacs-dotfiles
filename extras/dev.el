;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (rust-mode . rust-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;; automatically install treesitter grammar
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eglot
  :ensure nil ;; Built-in in Emacs 29+
  :hook ((python-ts-mode . eglot-ensure)
         (tsx-ts-mode    . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-ts-mode     . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format))
  :config
  ;; Performance boost: don't log every JSON-RPC event
  (fset #'jsonrpc--log-event #'ignore) 
  
  ;; Tell Eglot to use the servers you already have installed
  (add-to-list 'eglot-server-programs 
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(js-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))
  )

;; auto-format on save
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;;; setting dart
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;; (use-package lsp-dart
;;   :defer t
;;   :hook (dart-mode . lsp-deferred))

(use-package dart-mode
  :defer t
  :mode ("\\.dart\\'" . dart-mode))

;; (use-package hover
;;   :defer t
;;   :after dart)

(use-package flutter
  :defer t
  :after dart-mode)

;; (use-package ob-dart
;;   :ensure t
;;   :defer t)

;; setting up flutter/drt sdk
(setq lsp-dart-sdk-dir "/home/kaushalbundel/dev/flutter/bin/dart"
      lsp-dart-flutter-sdk-dir "/home/kaushalbundel/dev/flutter/bin/flutter")
;;TODO:  check if the variable flutter-sdk-path should be defined or not?

;; (require 'ob-dart)			
;;(add-to-list 'org-babel-load-languages '(dart . t))

(use-package web-mode
  :ensure t
  ;; Use the :mode keyword for a cleaner setup
  :mode (("\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;;("\\.css\\'" . web-mode)
         ;;("\\.jsx?\\'" . web-mode) ; .js and .jsx are removed so that direct js mode can work instead of web-mode
         ("\\.json\\'" . web-mode))
  :config
  ;; Enable both auto-closing and auto-pairing for a fluid experience
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t) 
  (setq web-mode-enable-auto-quoting t)
  
  ;; Your Django setting is fine
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'"))))

(setopt org-confirm-babel-evaluate nil) ;; no (y/n) before code execution in source block

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippets
;;A snippet manager for fast work
;;
;;
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode text-mode org-mode) . yas-minor-mode))

;; doom snippets contains useful snippets
(use-package doom-snippets
  :load-path "~/.emacs.d/snippets/"
  :after yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP-Mode configuration
;;
;;
;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred
;; Basic LSP Mode configuration
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   ;; General settings
;;   (lsp-keymap-prefix "C-c l")              ;; Set prefix for LSP key bindings
;;   (lsp-enable-which-key-integration t)     ;; Enable which-key integration
;;   (lsp-headerline-breadcrumb-enable t)     ;; Enable breadcrumb on headerline
;;   (lsp-idle-delay 0.5)                     ;; Delay for optimizing performance
;;   (lsp-log-io nil)                         ;; Don't log everything = better performance
  
;;   ;; Performance optimization
;;   (lsp-completion-provider :none)          ;; Use completion-at-point-functions
;;   (lsp-auto-completion nil)
;;   (lsp-keep-workspace-alive nil)           ;; Auto-kill LSP server
;;   (lsp-signature-auto-activate nil)        ;; Disable automatic signatures
  
;;   ;; UI customization
;;   (lsp-lens-enable nil)                    ;; Disable code lens (performance)
;;   (lsp-modeline-diagnostics-enable t)      ;; Show errors in modeline
;;   (lsp-modeline-code-actions-enable t)     ;; Show code actions in modeline
  
;;   ;; Configure hooks for common programming modes
;;   :hook ((python-mode . lsp-deferred)
;;          (js-mode . lsp-deferred)
;;          (typescript-mode . lsp-deferred)
;;          (web-mode . lsp-deferred)
;;          (html-mode . lsp-deferred)
;;          (css-mode . lsp-deferred)
;;          ;; Add other modes as needed
;;          (python-ts-mode . lsp-deferred)   ;;python ts mode provide more features than python mode
;;          ;; For dart, which you have in your config
;;          (dart-mode . lsp-deferred)
         
;;          ;; Enables lsp automatically after the LSP server is started
;;          (lsp-mode . lsp-enable-which-key-integration))

;;   ;; Commands will be added to autoload list automatically
;;   :config
;;   ;; LSP UI configurations
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  
;;   ;; Python specific settings
;;   (setq lsp-pyright-venv-path (expand-file-name "~/.pyenv/versions"))
;;   (setq lsp-pyright-use-library-code-for-types t))

;; ;; LSP UI for richer experience
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-enable t)                  ;; Enable documentation popups
;;   (lsp-ui-doc-position 'at-point)        ;; Show docs at point
;;   (lsp-ui-doc-delay 0.5)                 ;; Delay before showing docs
;;   (lsp-ui-sideline-enable t)             ;; Enable sideline info
;;   (lsp-ui-sideline-show-diagnostics t)   ;; Show diagnostics in sideline
;;   (lsp-ui-sideline-show-code-actions t)  ;; Show code actions in sideline
;;   (lsp-ui-sideline-ignore-duplicate t)   ;; Don't show duplicate info
;;   :config
;;   ;; Key bindings for UI components
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; ;; LSP Treemacs integration for project symbols
;; (use-package lsp-treemacs
;;   :ensure t
;;   :after (lsp-mode treemacs)
;;   :commands lsp-treemacs-errors-list
;;   :bind (:map lsp-mode-map
;;               ("C-c l t e" . lsp-treemacs-errors-list)
;;               ("C-c l t s" . lsp-treemacs-symbols)))

;; ;; Specific configuration for Python
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp-deferred)))
;;   :custom
;;   (lsp-pyright-multi-root nil))

;; Optional: which-key for better key binding discovery
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; uv-mode for python environment
(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))

;; code formatting on save
(use-package apheleia
  :defer t
  :config
  (apheleia-global-mode +1))

;; emmet mode for ease of tag insertion
(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode-hook . emmet-mode)
         (css-mode-hook . emmet-mode)
         (web-mode . emmet-mode)))

;; indent bar mode (Does not work on mac)
(cond
 ((equal system-type 'darwin)
  (use-package highlight-indent-guides
    :ensure t
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setopt highlight-indent-guides-method 'character
            highlight-indent-guides-responsive 'top)))
 ((equal system-type 'gnu/linux)
  (use-package indent-bars
    :ensure t
    :hook (prog-mode . indent-bars-mode)
    :config
    (setq
    indent-bars-color '(highlight :face-bg t :blend 0.15)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
    indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
    indent-bars-display-on-blank-lines nil))
  ))

;;python environment setup tool
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; vue file editing in emacs

;; adding vue file in web mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; adding hook to enable lsp mode as web-mode is started for vue files
(setq web-mode-engines-alist
      '(("vue" . "\\.vue\\'")))

;; racket mode (for SICP)
(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode))
  :hook ((racket-mode . eglot-ensure)))

;; Configuring go for development

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . subword-mode))
  :config
  ;; Optional: Auto-format on save using Eglot/gopls
  (add-hook 'before-save-hook
            (lambda ()
              (when (derived-mode-p 'go-ts-mode)
                (eglot-format-buffer)))))

;; Organizing go imports
(add-hook 'before-save-hook
    (lambda ()
        (call-interactively 'eglot-code-action-organize-imports))
    nil t)

;; rust mode for rust programming

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t))

;; Use cargo-mode instead of the outdated cargo package
(use-package cargo-mode
  :ensure t
  :defer t
  :hook ((rust-mode . cargo-minor-mode)
         (rust-ts-mode . cargo-minor-mode)))

;; Map the cargo-mode commands directly to your custom layout in rust-ts-mode
(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-c C-u") 'cargo-mode-build)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-k") 'cargo-mode-execute-task) ; Prompts for any command (like check)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-t") 'cargo-mode-test)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-r") 'cargo-mode-run)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-l") 'cargo-mode-last-command))
