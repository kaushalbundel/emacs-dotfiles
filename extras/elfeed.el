;; installing elfeed
(use-package elfeed
  :ensure t
  :bind
  ("C-c e f" . elfeed-db)
  :custom
    (elfeed-db-directory
     (expand-file-name "elfeed" user-emacs-directory))
     (elfeed-show-entry-switch 'display-buffer))
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
;; elfeed org
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files '("~/.emacs.d/extras/elfeed.org")))

;; ;;elfeed goodies
(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))


;; elfeed-tube
(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t ;; or :straight t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(setq +ligatures-in-modes '(not special-mode comint-mode eshell-mode term-mode
                                vterm-mode Info-mode elfeed-search-mode elfeed-show-mode))
