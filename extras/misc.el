;;to encode secret API key 
;;https://www.reddit.com/r/emacs/comments/kxxkg9/how_do_guys_include_secrets_in_their_initel/
;; (defun my-auth (key)
;;   (with-temp-buffer
;;     (insert-file-contents-literally "~/.my-auth")
;;     (alist-get key (read (current-buffer)))))

;; denote package
(use-package denote
  :ensure t
  :defer 5
  :bind (("C-c n l" . denote-link-or-create)
         ("C-c n n" . denote)))
;; Denote Configuration

(setq denote-directory (expand-file-name "C:\\Users\\kaush\\OneDrive\\09-Notes"))      ;creating Denote directory
(setq denote-known-keywords '("work" "personal" "health" "article" "course" "video" "audio"))           ;setting the keywords
(setq denote-infer-keywords t)                                                                          ;if any new keywords are added in Denote will add them to the list of keywords
(setq denote-sort-keywords t)                                                                           ;keyword sorting
(denote-rename-buffer-mode 1)                                                                           ;rename buffers as denote buffers

;; consult denote package provides mini-buffer preview of denote files and easy search with preview.
(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;;denote org-specific
(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))


;; GPTEL (flavor of llm in emacs)
;;TODO: encase the gptel key in a function 
;; (use-package gptel
;;   :ensure t
;;   :defer 5
;;   :config
;;   (setq gptel-api-key (my-auth 'key1))
;;   :bind ("<f1>" . gptel-send))

;; conformatble padding
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 2
           :tab-width 2
           :right-divider-width 24
           :scroll-bar-width 8))
  (spacious-padding-mode 1))

;; pdf-tool install
;; (use-package pdf-tools
;;   :defer 10
;;   :init   (system-packages-ensure "pdf-tools")
;;   :init (pdf-loader-install))

;; olivetti mode
(use-package olivetti
  :ensure t
  :defer 5
  :custom
  (olivetti-body-width 130)
  :bind
  ("<f1>" . olivetti-mode)
  :config
  (add-hook 'olivetti-mode-hook (lambda ()(if olivetti-mode (toggle-frame-fullscreen)(toggle-frame-fullscreen)))))


;; using profiler
(use-package esup
  :ensure t
  :commands(esup))

;; ox-pandoc for org mode file conversion
;; (use-package ox-pandoc
;;   :ensure t
;;   :after org)

;; misc functions

;; copy the path of directory in which the buffer resides
(defun kaushal/copy-buffer-directory-path ()
  "copy buffer directory to clipboard"
  (interactive)
  (kill-new (string-trim-left (pwd) "Directory ")))
(global-set-key (kbd "C-c y d") 'kaushal/copy-buffer-directory-path)

;; making C-g useful (DWIM) (Take out from prot config)
(defun kaushal/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'kaushal/keyboard-quit-dwim)

;; writeroom mode
;; TODO: Evaluate against olivetti mode

(use-package writeroom-mode
  :ensure t
  :defer 5
  :config
  ;; Basic writeroom settings
  (setq writeroom-width 120
        writeroom-fringes-outside-margins nil
        writeroom-center-text t
        writeroom-extra-line-spacing 4
        writeroom-mode-line nil
        writeroom-alpha 0.95)

  ;; Global effects for writeroom
  (setq writeroom-global-effects
        '(writeroom-set-fullscreen
          writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width))

  ;; Writeroom mode configuration
  :hook (writeroom-mode . my-writeroom-setup))

(defvar-local my-writeroom-face-cookie nil
  "Store face remapping cookie for writeroom mode.")

(defun my-writeroom-calculate-margins ()
  "Calculate appropriate top and bottom margins for vertical centering."
  (max 10 (/ (- (window-height) 40) 3)))

(defun my-writeroom-enter ()
  "Configure settings when entering writeroom mode."
  ;; Visual settings
  (display-line-numbers-mode -1)
  (hl-line-mode -1)
  (variable-pitch-mode 1)
  
  ;; Cursor and text appearance
  (setq cursor-type 'bar)
  (setq-local my-writeroom-face-cookie
              (face-remap-add-relative 'default :height 0.9))
  
  ;; Margin settings
  (setq left-margin-width 0
        right-margin-width 0)
  
  ;; Visual fill column configuration
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1))
  
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t
        visual-fill-column-extra-text-width '(0 . 0))
  
  ;; Vertical centering margins
  (let ((margin-size (my-writeroom-calculate-margins)))
    (setq-local writeroom-top-margin-size margin-size
                writeroom-bottom-margin-size margin-size))
  
  (visual-fill-column-mode 1))

(defun my-writeroom-exit ()
  "Restore settings when exiting writeroom mode."
  ;; Restore visual settings
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  
  ;; Restore cursor and text
  (setq cursor-type 'box)
  (text-scale-set 0)
  
  ;; Clean up face remapping
  (when my-writeroom-face-cookie
    (face-remap-remove-relative my-writeroom-face-cookie)
    (setq my-writeroom-face-cookie nil))
  
  ;; Disable visual fill column
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1)))

(defun my-writeroom-setup ()
  "Main function to handle writeroom mode toggle."
  (if writeroom-mode
      (my-writeroom-enter)
    (my-writeroom-exit)))
