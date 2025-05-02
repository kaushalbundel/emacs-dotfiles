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

(setq denote-directory (expand-file-name "~/Insync/kaushalbundel@outlook.com/OneDrive/09-Notes/"))      ;creating Denote directory
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
(use-package pdf-tools
  :defer 10
  :init   (system-packages-ensure "pdf-tools")
  :init (pdf-loader-install))

;; olivetti mode
(use-package olivetti
  :ensure t
  :defer 5
  :custom
  (olivetti-body-width 130))


;; using profiler
(use-package esup
  :ensure t
  :commands(esup))

;; ox-pandoc for org mode file conversion
(use-package ox-pandoc
  :ensure t
  :after org)

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
