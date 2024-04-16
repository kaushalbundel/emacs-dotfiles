;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables

;;; Phase 2 variables

;; Agenda variables

(setq org-directory "~/Insync/kaushalbundel@outlook.com/OneDrive/09-Notes")
(setq org-agenda-files '("~/Insync/kaushalbundel@outlook.com/OneDrive/01-Vision-Plan/02-Plan/02-habit"
                         "~/Insync/kaushalbundel@outlook.com/OneDrive/09-Notes"
                         "~/Insync/kaushalbundel@outlook.com/OneDrive/07-Programming"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;;; Phase 3 variables

;; Org-roam variables
;; (setq org-roam-directory "~/Documents/org-roam/")
;; (setq org-roam-index-file "~/Documents/org-roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
;; (setq org-link-abbrev-alist
;;       '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "Dropped(o@)" "Rescheduled")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "todo.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "todo.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))
  (setopt org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)  

  (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org")))
	     ("u" "Unscheduled" alltodo "")))))

;; Move the TODO after the current headline and the content instead of the a new headline
(setq org-insert-heading-respect-content t)

;; org-habit
(add-to-list 'org-modules 'org-habit t)
(setopt org-log-into-drawer t
	org-habit-following-days 3
        org-habit-preceding-days 7
        org-habit-show-all-today t)

(setq org-habit-graph-column 80)
;; setting org-bullets
;; org-bullets is now archieved, using org-superstar now

(use-package org-superstar
  :ensure t
  :after (org)
  :config
  (setopt org-hide-leading-stars t
	  org-superstar-leading-bullet ?\s
	  org-superstar-special-todo-items t))


;; org-noter
(use-package org-noter
  :ensure t
  :defer
  :config
  (setopt org-noter-always-create-frame nil
          org-noter-kill-frame-at-session-end nil))

;; org-download
(use-package org-download
  :ensure t
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  ;; (setq org-download-backend (if IS-LINUX "curl" "url-retrieve"))
  (setq org-download-heading-lvl nil)
  (setq org-download-backend "curl")
  (setq-default org-download-image-dir "./images")
  (setq org-download-image-attr-list
        '("#+attr_html: :width 40% :align center"
          "#+attr_latex: :width 0.5\\textwidth")))

;; installing ox-hugo for blogging workflow
(use-package ox-hugo
  :ensure t   
  :after ox)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  org-agenda
;;
;;

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (let ((org-super-agenda-groups
	 '((:auto-group t))))
    (org-agenda-list)))
  
(global-set-key (kbd "C-c o a") 'org-agenda)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;org-babel
;;
;;loading languages to be used with org-babel
;;add other languages in brackets

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((ipython . t)
;;    (python . t)
;;    (dart . t)
;;    ;;(add language)
;;    ))
