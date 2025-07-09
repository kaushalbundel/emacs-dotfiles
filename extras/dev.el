;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

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
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

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

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

;; (use-package eglot
;;   ;; no :ensure t here because it's built-in

;; ;;  Configure hooks to automatically turn-on eglot for selected modes
;;   :hook ((python-mode . eglot-ensure))

;;   :custom
;;   (eglot-send-changes-idle-time 0.1)
;;   (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

;;   :config
;;   (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
;;   ;; Sometimes you need to tell Eglot where to find the language server
;;   (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))


;;; setting dart
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(use-package lsp-dart
  :defer t
  :after dart)

(use-package dart-mode
  :defer t
  :after dart)

(use-package hover
  :defer t
  :after dart)

(use-package flutter
  :defer t
  :after dart)

(use-package ob-dart
  :ensure t
  :defer t)

;; (require 'ob-dart)			
;;(add-to-list 'org-babel-load-languages '(dart . t))

;; setting for web development
;;(copied from https://cestlaz.github.io/posts/using-emacs-21-web-mode/)
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python 
;;
;;
;;
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
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; General settings
  (lsp-keymap-prefix "C-c l")              ;; Set prefix for LSP key bindings
  (lsp-enable-which-key-integration t)     ;; Enable which-key integration
  (lsp-headerline-breadcrumb-enable t)     ;; Enable breadcrumb on headerline
  (lsp-idle-delay 0.5)                     ;; Delay for optimizing performance
  (lsp-log-io nil)                         ;; Don't log everything = better performance
  
  ;; Performance optimization
  (lsp-completion-provider :capf)          ;; Use completion-at-point-functions
  (lsp-auto-completion nil)
  (lsp-keep-workspace-alive nil)           ;; Auto-kill LSP server
  (lsp-signature-auto-activate nil)        ;; Disable automatic signatures
  
  ;; UI customization
  (lsp-lens-enable nil)                    ;; Disable code lens (performance)
  (lsp-modeline-diagnostics-enable t)      ;; Show errors in modeline
  (lsp-modeline-code-actions-enable t)     ;; Show code actions in modeline
  
  ;; Configure hooks for common programming modes
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         ;; Add other modes as needed
         (python-ts-mode . lsp-deferred)   ;;python ts mode provide more features than python mode
         ;; For dart, which you have in your config
         (dart-mode . lsp-deferred)
         
         ;; Enables lsp automatically after the LSP server is started
         (lsp-mode . lsp-enable-which-key-integration))

  ;; Commands will be added to autoload list automatically
  :config
  ;; LSP UI configurations
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  
  ;; Python specific settings
  (setq lsp-pyright-venv-path (expand-file-name "~/.pyenv/versions"))
  (setq lsp-pyright-use-library-code-for-types t))

;; LSP UI for richer experience
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)                  ;; Enable documentation popups
  (lsp-ui-doc-position 'at-point)        ;; Show docs at point
  (lsp-ui-doc-delay 0.5)                 ;; Delay before showing docs
  (lsp-ui-sideline-enable t)             ;; Enable sideline info
  (lsp-ui-sideline-show-diagnostics t)   ;; Show diagnostics in sideline
  (lsp-ui-sideline-show-code-actions t)  ;; Show code actions in sideline
  (lsp-ui-sideline-ignore-duplicate t)   ;; Don't show duplicate info
  :config
  ;; Key bindings for UI components
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; LSP Treemacs integration for project symbols
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("C-c l t e" . lsp-treemacs-errors-list)
              ("C-c l t s" . lsp-treemacs-symbols)))

;; Specific configuration for Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom
  (lsp-pyright-multi-root nil))

;; Yasnippet for code templates
(use-package yasnippet
  :ensure t
  :hook (lsp-mode . yas-minor-mode))

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
         (css-mode-hook . emmet-mode)))

;; indent bar mode (Does not work on mac)
(cond
 ((equal system-type 'darwin)
  (use-package highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setopt highlight-indent-guides-method 'character
            highlight-indent-guides-responsive 'top)))
 ((equal system-type 'gnu/linux)
(use-package indent-bars
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
    indent-bars-display-on-blank-lines t))
  ))
