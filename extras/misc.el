;;to encode secret API key 
;;https://www.reddit.com/r/emacs/comments/kxxkg9/how_do_guys_include_secrets_in_their_initel/
(defun my-auth (key)
  (with-temp-buffer
    (insert-file-contents-literally "~/.my-auth")
    (alist-get key (read (current-buffer)))))

;; denote package
(use-package denote
  :ensure t
  :defer 5)
;; Denote Configuration

(setq denote-directory (expand-file-name "~/Insync/kaushalbundel@outlook.com/OneDrive/09-Notes/"))      ;creating Denote directory
(setq denote-known-keywords '("work" "personal" "health" "article" "course" "video" "audio"))           ;setting the keywords
(setq denote-infer-keywords t)                                                                          ;if any new keywords are added in Denote will add them to the list of keywords
(setq denote-sort-keywords t)                                                                           ;keyword sorting
(denote-rename-buffer-mode 1)                                                                           ;rename buffers as denote buffers

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
(use-package gptel
  :ensure t
  :defer 5
  :config
  (setq gptel-api-key (my-auth 'key1))
  :bind ("<f1>" . gptel-send))

;; conformatble padding
(use-package spacious-padding
  :ensure t
  :defer 5
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 16
         :header-line-width 4
         ;; :mode-line-width 2
         :tab-width 2
         :right-divider-width 24
         :scroll-bar-width 8)))

;; pdf-tool install
(use-package pdf-tools
  :defer 10
  ; :init   (system-packages-ensure "pdf-tools")
  :init (pdf-loader-install))

;; olivetti mode
(use-package olivetti
  :ensure t
  :defer 5
  :custom
  (olivetti-body-width 130))

