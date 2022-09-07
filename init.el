(setq inhibit-startup-message t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-v") 'clipboard-yank)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "M-q") 'evil-quit-all)
(menu-bar-mode t)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(setq make-backup-files nil)
(setq global-auto-revert-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq make-backup-files nil)
(global-auto-revert-mode t) ;; hot reload files
(setq-default line-spacing 0.3)

(require 'package)

(setq package-archives '(
			 ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
			 ;; ("melpa" . "https://melpa.org/packages")
                         ("Org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package quelpa)
(use-package quelpa-use-package)

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))

(defun skril/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    ;; (setq dashboard-startup-banner "~/.emacs.d/img/avatar.png")
    (setq dashboard-startup-banner 2)
    ;; (setq dashboard-banner-logo-title "Hi!")
    (setq dashboard-items '((recents  . 5)
			    ;; (agenda . 5)
			    (bookmarks . 5)))
    (setq dashboard-center-content t)
    )

;; (use-package async
;;   :ensure t
;;   :init (dired-async-mode 1))

(use-package undo-tree)

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    ;; :hook (evil-mode . skril/evil-hook)
    :config
    (evil-mode 1)
    (evil-set-undo-system 'undo-tree)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-visual-state-map (kbd "gc") 'comment-or-uncomment-region)
    (define-key evil-normal-state-map (kbd "gcc") 'comment-line)
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    )

(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

; TODO setup increment/decrement like in vim
;(use-package evil-numbers
  ;:config
    ;(define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
    ;(define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(ivy-mode t)

(use-package counsel)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 1)))

(set-face-attribute 'default nil :height 170 :font "Fira Mono")
;; (set-frame-parameter (selected-frame) 'alpha 93)
;; (add-to-list 'default-frame-alist '(alpha  93))
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(column-number-mode)
(global-display-line-numbers-mode t)
;(setq display-line-numbers 'relative)
;(display-line-numbers-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  ;; :init 
  :diminish which-key-mode
  :config
    (which-key-mode)
  (setq whick0key-idele-delay 0.3))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer skril/leader-keys
	:states '(normal insert visual emacs)
	:prefix "SPC"
	:global-prefix "s-SPC")
	(skril/leader-keys
		"a" 'org-agenda
		"b" 'counsel-ibuffer
		"c" 'org-capture
		"t" 'org-insert-structure-template
		"u" 'org-agenda-bulk-action
		"lb" 'counsel-bookmark
		"m" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
		"o" 'counsel-find-file
		;; "f" '(lambda () (interactive) (org-ql-search 'org-agenda-files))
		"s" 'swiper ))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) 


(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(defun skril/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
    :commands (org-capture org-agenda)
    :hook (org-mode . skril/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")

    (setq org-archive-location "~/Personal Library/Org/Archive.org::Archived entries from file %s")
    (setq org-capture-templates
	'(("i" "Inbox" entry (file "~/Personal Library/Org/Desktop.org")
	    	  "* INBOX %^{Inbox item}\n:PROPERTIES:\n:CREATED :%U\n:END:\n %^{Additional information}")
	("l" "Inbox with link" entry (file"~/Personal Library/Org/Desktop.org")
	    	  "* INBOX %?\n :PROPERTIES:\n :CREATED: %U\n :END:\n %A")
	    ))
    (add-to-list 'org-structure-template-alist
	 '("i" . "* INBOX %^{Inbox item}\n:PROPERTIES:\n:CREATED :%U\n:END:\n %^{Additional information}")
	 )
    (setq org-agenda-files
	    (list "~/Personal Library/Org/Desktop.org"
		"~/Personal Library/Org/Phone.org"
		"~/Personal Library/Org/Projects.org"
		"~/Personal Library/Org/Matepad.org"
		"~/Personal Library/Org/Birthdays.org"
		))
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-todo-keywords
	    '((sequence "INBOX(i)" "PROJECTS(p)" "SOMEDAY(s)" "TODO(t)" "WAITING(w)" "CALENDAR(c)" "|" "DONE(d)" "CANCELLED(x)")))
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-todo-keyword-faces
	  '(("INBOX" . "white smoke")
	    ("PROJECTS" . "yellow green")
	    ("SOMEDAY" . "coral")
	    ("TODO" . "gold")
	    ("WAITING" . "dark violet")
	    ("CALENDAR" . "light sea green")
	    ("DONE" . "slate gray")
	    ("CANCELLED" . "slate gray")))

  )



(setq org-agenda-custom-commands
      '(("m" "Music entries"
         ((org-ql-block '(and (todo "TODO")
                              (and(tags "music") (tags "download")))
                        ((org-ql-block-header "TODO music download")))
	  (org-ql-block '(and (todo "SOMEDAY")
			      (and(tags "music") (tags "download")))
			((org-ql-block-header "SOMEDAY music download")))
          ))
	("f" "FILMS"
	 ((org-ql-block '(and (tags "film") (not (or (todo "DONE") (todo "CANCELLED"))))
			    ((org-ql-block-header "Films")))
          ))
	("b" "BOOKS"
	 ((org-ql-block '(and (tags "book") (not (or (todo "DONE") (todo "CANCELLED"))))
			    ((org-ql-block-header "Books")))
          ))
	("c" "CONTENT"
	 ((org-ql-block '(and (tags "content") (not (or (todo "DONE") (todo "CANCELLED"))))
			    ((org-ql-block-header "Inbox")))
          ))
	("s" "SOMEDAY without music"
	    ((org-ql-block '(and(todo "SOMEDAY") (not (and (tags "music") (tags "download")) ))
			    ((org-ql-block-header "SOMEDAY without music")))
          ))
	("i" "Inbox"
	    ((org-ql-block '(todo "INBOX")
			    ((org-ql-block-header "Inbox")))
          ))
	("x" "Todo entries"
	 (
	    (org-ql-block '(and (priority "A")
				(todo "TODO"))
				((org-ql-block-header "Important")))
	    (org-ql-block '(and (todo "TODO")
				(tags "study"))
				((org-ql-block-header "Study")))
	    (org-ql-block '(and (todo "TODO")
				(tags "work"))
			    ((org-ql-block-header "Work")))
	    (org-ql-block '(and (todo "TODO")
				(tags "note"))
			    ((org-ql-block-header "Notes")))
	    (org-ql-block '(and (todo "TODO")
			    (and (or (tags "research") (tags "think")) (not (tags "study")) (not (tags "work")) (not (tags "note"))))
			    ((org-ql-block-header "Study, research or think")))
	    (org-ql-block '(and (todo "TODO")
				(not (or
				 (and (tags "music") (tags "download"))
				 (tags "research")
				 (tags "think")
				 (tags "study")
				 (tags "work")
				 (tags "note"))))
			    ((org-ql-block-header "Other")))
	    (org-ql-block '(and (todo "TODO")
				(and(tags "music") (tags "download")))
			    ((org-ql-block-header "Music download")))
	    ))))
      


; Auto-save all org-files after changing them
(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-capture-after-finilize-hook 'org-save-all-buffers)

(with-eval-after-load 'org
    (eval-after-load 'org-agenda
    '(progn
	(evil-set-initial-state 'org-agenda-mode 'normal)
	(evil-define-key 'normal org-agenda-mode-map
	(kbd "<RET>") 'org-agenda-switch-to
	(kbd "\t") 'org-agenda-goto

	"+" 'org-agenda-priority-up
	"," 'org-agenda-priority
	"-" 'org-agenda-priority-down
	"." 'org-agenda-goto-today
	"0" 'evil-digit-argument-or-evil-beginning-of-line
	":" 'org-agenda-set-tags
	";" 'org-timer-set-timer
	"<" 'org-agenda-filter-by-category
	">" 'org-agenda-date-prompt
	"a" 'org-agenda-toggle-archive-tag
	"A" 'org-agenda-archive
	"D" 'org-agenda-deadline
	"F" 'org-agenda-follow-mode
	"H" 'org-agenda-holidays
	"I" 'helm-org-task-file-headings
	"J" 'org-agenda-next-date-line
	"K" 'org-agenda-previous-date-line
	"L" 'org-agenda-recenter
	"O" 'org-agenda-clock-out-avy
	"P" 'org-agenda-show-priority
	"R" 'org-agenda-clockreport-mode
	"S" 'org-save-all-org-buffers
	"T" 'org-agenda-show-tags
	"X" 'org-agenda-clock-cancel
	"Z" 'org-agenda-sunrise-sunset
	"[" 'org-agenda-manipulate-query-add
	"b" 'org-agenda-earlier
	"c" 'helm-org-capture-templates
	"e" 'org-agenda-set-effort
	"f" 'org-agenda-later
	"g/" 'org-agenda-filter-by-tag
	"gJ" 'org-agenda-clock-goto
	"g\\" 'org-agenda-filter-by-tag-refine
	"gh" 'org-agenda-holiday
	"gj" 'org-agenda-goto-date
	"go" 'org-agenda-open-link
	"gv" 'org-agenda-view-mode-dispatch
	"i" 'org-agenda-clock-in-avy
	"j"  'org-agenda-next-line
	"k"  'org-agenda-previous-line
	"m" 'org-agenda-bulk-mark
	"n" 'org-agenda-add-note
	"n" nil  ; evil-search-next
	"o" 'delete-other-windows
	"q" 'org-agenda-quit
	"r" 'org-agenda-redo
	"s" 'org-agenda-schedule
	"t" 'org-agenda-todo
	"C-u" 'org-agenda-bulk-unmark
	"u" 'org-agenda-undo
	"va" 'org-agenda-archives-mode
	"vc" 'org-agenda-show-clocking-issues
	"vd" 'org-agenda-day-view
	"vl" 'org-agenda-log-mode
	"vt" 'org-agenda-toggle-time-grid
	"vw" 'org-agenda-week-view
	"x" 'org-agenda-exit
	"y" 'org-agenda-todo-yesterday
	"{" 'org-agenda-manipulate-query-add-re
	"}" 'org-agenda-manipulate-query-subtract-re
	"]" 'org-agenda-manipulate-query-subtract)))
)
;; (dolist (face '(
;; 		 (org-level-1 . 1.2)
;; 		(org-level-2 . 1.1)
;; 		(org-level-3 . 1.05)
;; 		(org-level-4 . 1.0)
;; 		(org-level-5 . 1.1)
;; 		(org-level-6 . 1.1)
;; 		(org-level-7 . 1.1)
;; 		(org-level-8 . 1.1)))
;; 		(set-face-attribute (car face) nil :font "Fira Mono" :weight 'bold :height (cdr face)))

(defun skril/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

;; Set up different location for custom configuration file
;; that emacs rewrites automatically
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; (add-hook 'emacs-startup-hook #'skril/display-startup-time)

