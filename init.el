; Inspired from Emacs Bedrock (https://codeberg.org/ashton314/emacs-bedrock)
;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Guardrail

;; (when (< emacs-major-version 29)
;;   (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
;; You can simply uncomment the following if you'd like to get started with
;; MELPA packages quickly:
;;
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setopt use-package-always-ensure t)

;; Auto-update package for automatically updating packages
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;; If you want to turn off the welcome screen, uncomment this
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most
(setq ring-bell-function 'ignore) ;; turning off the annoying bell on windows
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; backup files
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; adding unicode support
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Old Modifiers
(setq undo-limit 80000000)    ;;raise undo limit
(setq evil-want-fine-undo t)  ;;more granular undo
(setq scroll-margin 2)        ;;maintaining a little margin while scrolling
(setq save-interprogram-paste-before-kill t) ;;integrates system clipboard to kill ring

;;windows specific change
(when (equal system-type 'windows-nt)
  (setopt default-directory "C:\\Users\\kaush"))

;;mac-specific settings
(when (equal system-type 'darwin)
 (setq mac-option-modifier        'meta
      mac-right-option-modifier  'meta
      mac-command-modifier       'control))

;; mac-specifc settings for disabling gpg key for package verification
(when (equal system-type 'darwin)
  (setq package-check-signature nil))

;;pricision mode is not working on macos
(when (equal system-type 'darwin)
  (setq pixel-scroll-precision-mode nil))
;; deleted files moving to Trash
(setq delete-by-moving-to-trash t)

;; changing yes and no to y and n
(setopt use-short-answers t)

;; asking emacs before closing
(setq confirm-kill-emacs #'y-or-n-p)

;;elfeed links opening in eww
(setq browse-url-browser-function 'eww-browse-url)

;; If a text is selected and delete is pressed then delete that text
(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

;; useful dired enhancements
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; settiing a margin in emacs for better visibility (It does look weird)
;;(set-window-margins nil 1 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
;;(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; for debugging
;; TODO: Remove this
;; (setq debug-on-error t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode 1)                                ; Steady cursor

;; Use common keystrokes by default
;;(cua-mode)

;; Display line numbers in programming mode
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; disabling due to global changes below
(setopt display-line-numbers-width 3)           ; Set a minimum width
;; enabling global line number mode
(global-display-line-numbers-mode 1)
;; disable line numbers in specific modes
(dolist (mode '(org-mode-hook
                term--mode-hook
                shell-mode-hook
                eshell-mode-hook
                eww-mode-hook
                olivetti-mode-hook
                elfeed-show-mode-hook
                elfeed-search-mode-hook))
  (add-hook mode (lambda ()(display-line-numbers-mode 0))))


;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package dashboard
;; ;; :disabled
;;   :ensure t
;;   :init
;;     (dashboard-setup-startup-hook)
;;   :config
;;   (setq dashboard-startup-banner 'logo
;;         dashboard-show-shortcuts nil
;;         dashboard-center-content t
;;         dashboard-items '((recents  . 5)
;;                           (projects . 6)
;;                           (bookmarks . 5)
;;                           (agenda . 5)
;;                           ;;(registers . 5)
;;                           )))
;; ;; important for getting the dashboard buffer
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package rainbow-mode
  :commands rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keyboard Shortcuts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent file access
(global-set-key (kbd "C-c f r") 'recentf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package emacs
;;   :config
;;   (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi

;; doom emacs
 (use-package doom-themes
   :ensure t
   :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled

;; modus theme (additional customization: https://protesilaos.com/emacs/modus-themes#h:1af85373-7f81-4c35-af25-afcef490c111)
(use-package modus-themes
  :ensure t
  :defer t
  :config
  ;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `italic', `WEIGHT'
      modus-themes-prompts '(italic bold)

      ;; The `modus-themes-completions' is an alist that reads two
      ;; keys: `matches', `selection'.  Each accepts a nil value (or
      ;; empty list) or a list of properties that can include any of
      ;; the following (for WEIGHT read further below):
      ;;
      ;; `matches'   :: `underline', `italic', `WEIGHT'
      ;; `selection' :: `underline', `italic', `WEIGHT'
      ;; modus-themes-completions
      ;; '((matches . (extrabold))
      ;;   (selection . (semibold italic text-also)))
      ;; modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      ;; The `modus-themes-headings' is an alist: read the manual's
      ;; node about it or its doc string.  Basically, it supports
      ;; per-level configurations for the optional use of
      ;; `variable-pitch' typography, a height value as a multiple of
      ;; the base font size (e.g. 1.5), and a `WEIGHT'.
      modus-themes-headings
      '((1 . (variable-pitch 1.2))
        (2 . (1.1))
        (agenda-date . (1.1))
        (agenda-structure . (variable-pitch light 1.2))
        (t . (1.1)))))
(load-theme 'modus-operandi-tinted :no-confirm)
  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;; changing buffer while splitting
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;;closing windows (like help windows)
(winner-mode 1)
;; good to have keybindings
(use-package crux
  :ensure t
  :defer t
  :bind ("C-k" . 'crux-smart-kill-line)
  ("<C-S-return>" . 'crux-smart-open-line-above)
  ("<C-return>" . 'crux-smart-open-line)
  ("C-c P" . 'crux-kill-buffer-truename)
  ("M-o" . 'crux-other-window-or-switch-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory)) 

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
(load-file (expand-file-name "extras/org.el" user-emacs-directory))


;; beautification and related things (fonts, icons etc)
(load-file (expand-file-name "extras/fonts.el" user-emacs-directory))

;; misc programs like denote and gptel etc
(load-file (expand-file-name "extras/misc.el" user-emacs-directory))

;; elfeed
(load-file (expand-file-name "extras/elfeed.el" user-emacs-directory))


;; custom config
(setq custom-file "~/.emacs.d/extras/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.
(setq enable-local-variables :safe)


;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))
(put 'downcase-region 'disabled nil)
