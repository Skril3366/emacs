; Hook to show start up time
(defun skril/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'skril/display-startup-time)



; ----------------------- General settings ------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p) ; use y-n instead of yes-no
(global-auto-revert-mode t) ;; hot reload files
(setq make-backup-files nil) ; remove feature of making backups

; UI tweaks
(setq-default line-spacing 0.3) ; space between lines
(menu-bar-mode nil) ; remove menu bar
(tool-bar-mode -1) ; remove toolbar
(tooltip-mode -1) ; remove hover on tips in tool bar
(scroll-bar-mode -1) ; remove scroll bar
(set-fringe-mode -1) ; paddings on the sides
(setq inhibit-startup-message t) ; remove startup message
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode) ; add cursor line

;; Set up different location for custom configuration file that emacs rewrites
;; automatically
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(add-hook 'text-mode-hook 'visual-line-mode)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


; ---------------------- Package management -----------------------------------
(require 'package)
; (setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(unless package-archive-contents
          (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; ------------------------ Key bindings ---------------------------------------

;; (use-package company
;;   :after ???
;;   :hoop ())


; Evil mode - emulation of vim
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
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
)

; ; (defun skril/evil-hook ()
; ;   (dolist (mode
; ;             '(custom-mod
; ;               eshell-mode
; ;               git-rebase-mode
; ;               erc-mode
; ;               term-mode
; ;               )
; ;             )
; ;     (add-to-list 'evil-emacs-state-modes mode)))
;
; ; Collection of useful keybindings for different modes, that are not handled by
; ; evil package by itself
 (use-package evil-collection
   :after evil
   :config
       (evil-collection-init)
   )

(use-package general
  :after evil
  :config
  (general-define-key
        "<escape>" 'keyboard-escape-quit
        "M-v" 'clipboard-yank
        "M-u" 'universal-argument
        "M-q" 'evil-quit-all
    )
  (general-define-key
    :keymaps 'evil-insert-state-map
        "C-g" 'evil-normal-state
    )
  (general-define-key
    :keymaps 'evil-visual-state-map
        "gc" 'comment-or-uncomment-region
    )
  (general-define-key
    :keymaps 'evil-normal-state-map
        "gcc" 'comment-line
    )
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
		"o" 'counsel-fin-file
		"ll" 'org-open-at-point-global
		;; "f" '(lambda () (interactive) (org-ql-search 'org-agenda-files))
		"s" 'swiper )
)

; --------------------------------- General UX --------------------------------

; Show helpful menu at the bottom with all the keybindings available under typed
; prefix
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
    (which-key-mode)
  (setq whick0key-idele-delay 0.3))

; An interactive interface for completion in Emacs
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

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

; TODO: Understand what is it
(use-package counsel
    :bind
        (("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer)
        ("C-x C-f" . counsel-find-file)
    :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history))
    :config
        (setq ivy-initial-inputs-alist nil)
        (push '(counsel-ag . "--file-search-regex '' -- ") ivy-initial-inputs-alist)
        (push '(counsel-rg . "--glob '**' -- ") ivy-initial-inputs-alist)
  )

; Dealing with parentheses
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

; Make parenthesis colorful
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; Full undo tree history like in vim
(use-package undo-tree
    :after evil
    :config
        (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
)

; ----------------------------------- Org mode --------------------------------

(use-package org
    :commands
        (org-capture org-agenda)
    :config
        (org-indent-mode)
        (visual-line-mode 1)
        (setq evil-auto-indent nil)
        (setq org-ellipsis " ▾")
        (setq org-agenda-start-with-log-mode t)
        (setq org-log-done 'time)
        (setq org-log-into-drawer t)
        (skril/org-mode-setup)
)

(defun skril/org-mode-setup ()
    (skril/org-tags-setup)
    (skril/org-headings-setup)
    (skril/org-templates-setup)
    (skril/org-files-setup)
    (skril/org-todo-setup)
    (skril/org-views-setup)
    (skril/org-autosave-setup)
)

(defun skril/org-tags-setup ()
    (setq org-tag-alist '(
	 ; Types of content
	(:startgroup)
	    ("music" . ?m)
	    ("film" . ?f)
	    ("book" . ?b)
	    ("game" . ?g)
	    ("software" . ?s)
	    ("habit" . ?h)
	    ("article" . ?a)
	    ("funny_videos" . ?v)
	    ("places" . ?p)
	(:endgroup)
	(:startgroup)
	    ("idea" . ?I) ; something complete
	    ("thought" . ?T) ; to further think and develop
	(:endgroup)
	; Context
	(:startgroup)
	    ("@work" . ?W)
	    ("@room" . ?R)
	    ("@kazan" . ?k)
	    ("quick" . ?q) ; less than 10 minutes
	    ("dude" . ?D) ; from, a hard task, can be done in the morning
	    ("launchpad" . ?l)
	(:endgroup)
	 ; Actions
	(:startgroup)
	    ("study" . ?S)
	    ("research" . ?r)
	    ("think" . ?t)
	    ("buy" . ?B)
	    ("fix" . ?F)
	    ("download" . ?d)
	(:endgroup)
	; Spheres of life
	(:startgroup)
	    ("cloth" . ?c)
	    ("finance" . ?M)
	    ("psychology" . ?P)
	    ("health" . ?H)
	    ("goal" . ?G)
	(:endgroup)
	; Eisenhower matrix
	(:startgroup)
	    ("important" . ?i)
	    ; ("not_important" . ?ni)
	    ("urgent" . ?u)
	    ; ("not_urgent" . ?nu)
	(:endgroup)
    ))
)

(defun skril/org-templates-setup ()
    (setq org-capture-templates
          '(
            ("i" "Inbox" entry (file "~/Personal Library/Org/Desktop.org")
             "* INBOX %?\n:PROPERTIES:\n:CREATED :%U\n:END:\n")
            )
          )
)

(defun skril/org-headings-setup()
    (dolist (face '(
        (org-level-1 . 1.2)
        (org-level-2 . 1.1)
        (org-level-3 . 1.05)
        (org-level-4 . 1.0)
        (org-level-5 . 1.1)
        (org-level-6 . 1.1)
        (org-level-7 . 1.1)
        (org-level-8 . 1.1)
        ))
        (set-face-attribute (car face) nil
            :font "Fira Mono"
            :weight 'bold
            :height (cdr face)
        )
    )
)

(defun skril/org-files-setup ()
    (setq org-archive-location
          "~/Personal Library/Org/Archive.org::Archived entries from file %s")
    (setq org-agenda-files
        (list
            "~/Personal Library/Org/Desktop.org"
            "~/Personal Library/Org/Phone.org"
            "~/Personal Library/Org/Projects.org"
            "~/Personal Library/Org/Matepad.org"
            "~/Personal Library/Org/Birthdays.org"
            )
        )
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
)

(defun skril/org-todo-setup ()
    (setq org-todo-keywords '(
	(sequence
	    "INBOX(i)"
	    "RELEVANT(r)"
	    "SOMEDAY(s)"
	    "NOTES(n)"
	    "CONTENT(c)"
	    "WAITING(w)"
	    "PROJECTS(p)"
	    "TODO(t)"
	    "|"
	    "DONE(d)"
	    "TRASH(x)")
    ))
    (setq skril/colors-white "white smoke")
    (setq skril/colors-yellow "gold")
    (setq skril/colors-purple "dark violet")
    (setq skril/colors-gray "slate gray")
    (setq skril/colors-green "forest green")
    (setq skril/colors-orange "light coral")

    (setq org-todo-keyword-faces '(
        ("INBOX" . "white smoke")
	("RELEVANT" ."dark violet")
        ("SOMEDAY" . "light coral")
	("NOTES" . "forest green")
	("CONTENT" . "light coral")
        ("WAITING" . "dark violet")
        ("PROJECTS" . "forest green")
        ("TODO" . "gold")
        ("DONE" . "slate gray")
        ("TRASH" . "slate gray")
        )
    )
)

(defun skril/org-views-setup ()
  (setq org-agenda-custom-commands
	'(
	  ("mf" "Music fix"
	   (
	    (org-ql-block
	     '(and
	       (tags "music")
	       (tags "fix")
	       (not (done)))
	     ((org-ql-block-header "Fix")))
	   ))
	  ("md" "Music download"
	   (
	   (org-ql-block
	     '(and
	       (tags "music")
	       (tags "download")
	       (todo "TODO"))
	     ((org-ql-block-header "TODO")))
	   (org-ql-block
	     '(and
	       (tags "music")
	       (tags "download")
	       (todo "RELEVANT"))
	     ((org-ql-block-header "Relevant")))
	   (org-ql-block
	     '(and
	       (tags "music")
	       (tags "download")
	       (todo "CONTENT"))
	     ((org-ql-block-header "Other")))
	   ))
	  ("mm" "Music entries"
	   (
	   (org-ql-block
	     '(and
	       (tags "music")
	       (not (tags "download"))
	       (not (tags "fix"))
	       (todo "TODO"))
	     ((org-ql-block-header "TODO")))
	   (org-ql-block
	     '(and
	       (tags "music")
	       (not (tags "download"))
	       (not (tags "fix"))
	       (todo "RELEVANT"))
	     ((org-ql-block-header "Relevant")))
	   (org-ql-block
	     '(and
	       (tags "music")
	       (not (done))
	       (not (tags "download"))
	       (not (tags "fix"))
	       (not (todo "TODO"))
	       (not (todo "RELEVANT")))
	     ((org-ql-block-header "Other")))
	   ))
        ("cf" "Films"
         (
	  (org-ql-block
	   '(and
	     (todo "CONTENT")
	     (tags "film"))
	   ((org-ql-block-header "Films")))
	  ))
        ("cb" "Books"
         (
	  (org-ql-block
	   '(and
	     (todo "CONTENT")
	     (tags "book"))
	   ((org-ql-block-header "Books")))
	  ))
        ("cm" "Music"
         (
	  (org-ql-block
	   '(and
	     (todo "CONTENT")
	     (tags "music"))
	   ((org-ql-block-header "Music")))
	  ))
        ("cs" "Software"
         (
	  (org-ql-block
	   '(and
	     (todo "CONTENT")
	     (tags "software"))
	   ((org-ql-block-header "Software")))
	  ))
        ("i" "Inbox"
         (
	  (org-ql-block
	   '(todo "INBOX")
	   ((org-ql-block-header "Inbox")))
	  ))
        ("p" "Projects"
         (
	  (org-ql-block
	   '(todo "PROJECTS")
	   ((org-ql-block-header "Projects")))
	  ))
        ("r" "Relevant"
         (
	  (org-ql-block
	   '(todo "RELEVANT")
	   ((org-ql-block-header "Relevant")))
	  ))
        ("s" "Someday"
         (
	  (org-ql-block
	   '(todo "SOMEDAY")
	   ((org-ql-block-header "Someday")))
	  ))
        ("n" "Notes"
         (
	  (org-ql-block
	   '(todo "NOTES")
	   ((org-ql-block-header "Notes")))
	  ))
	  ("w" "Waiting"
	   (
	    (org-ql-block
	     '(todo "WAITING")
	     ((org-ql-block-header "Other")))
	    ))
	  ("x" "Dashboard"
	   (
	    (org-ql-block
	     '(and
	       (not (done))
	       (todo "TODO")
	       (priority "A"))
	     ((org-ql-block-header "Important")))
	    (org-ql-block
	     '(and
	       (not (done))
	       (todo "TODO")
	       (priority "B"))
	     ((org-ql-block-header "Should Do")))
	    (org-ql-block
	     '(and
	       (not (done))
	       (todo "TODO")
	       (priority "C"))
	     ((org-ql-block-header "May skip")))
	    (org-ql-block
	     '(and
	       (not (or
		     (done)
		     (and (tags "music") (tags "download"))
		     (priority "A")
		     (priority "B")
		     (priority "C")
		     ))
	       (todo "TODO"))
	     ((org-ql-block-header "Other")))
	    ))
	  )))


; Auto-save all org-files after changing them
(defun skril/org-autosave-setup ()
    (add-hook 'org-agenda-todo :after 'org-save-all-buffers)
    (add-hook 'org-agenda-priority :after 'org-save-all-buffers)
    (add-hook 'org-agenda-priority-up :after 'org-save-all-buffers)
    (add-hook 'org-agenda-priority-down :after 'org-save-all-buffers)
    (add-hook 'org-agenda-set-tags :after 'org-save-all-buffers)
    (add-hook 'org-agenda-deadline :after 'org-save-all-buffers)
    (add-hook 'org-agenda-schedule :after 'org-save-all-buffers)
    (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (add-hook 'org-capture-after-finilize-hook :after 'org-save-all-buffers)
)

(with-eval-after-load 'org
    (eval-after-load 'org-agenda
    '(progn
        (evil-set-initial-state 'org-agenda-mode 'normal)
        (evil-define-key 'normal org-agenda-mode-map
            (kbd "<RET>") 'org-agenda-switch-to
                (kbd "\t") 'org-agenda-goto
                    "+" 'org-agenda-priority-up
                    "-" 'org-agenda-priority-down

                    "." 'org-agenda-goto-today
                    "0" 'evil-digit-argument-or-evil-beginning-of-line
                    ":" 'org-agenda-set-tags

                    "<" 'org-agenda-filter-by-category
                    ">" 'org-agenda-date-prompt

                    "a" 'org-agenda-toggle-archive-tag
                    "A" 'org-agenda-archive

                    "D" 'org-agenda-deadline
                    "s" 'org-agenda-schedule

                    "F" 'org-agenda-follow-mode
                    "J" 'org-agenda-next-date-line
                    "K" 'org-agenda-previous-date-line
                    "L" 'org-agenda-recenter ; Show current heading in the file in the center of the screen
                    "S" 'org-save-all-org-buffers
                    "go" 'org-agenda-open-link

                    "g/" 'org-agenda-filter-by-tag
                    "g\\" 'org-agenda-filter-by-tag-refine

                    "t" 'org-agenda-todo

                    "j"  'org-agenda-next-line
                    "k"  'org-agenda-previous-line
                    "m" 'org-agenda-bulk-mark
                    "C-U" 'org-agenda-bulk-unmark

                    "r" 'org-agenda-redo
                    "u" 'org-agenda-undo
                    "q" 'org-agenda-quit
                    "n" nil  ; evil-search-next

                    ;; "H" 'org-agenda-holidays
                    ;; "I" 'helm-org-task-file-headings
                    ;; "O" 'org-agenda-clock-out-avy
                    ;; "P" 'org-agenda-show-priority
                    ;; "R" 'org-agenda-clockreport-mode
                    ;; "T" 'org-agenda-show-tags
                    ;; "X" 'org-agenda-clock-cancel
                    ;; "Z" 'org-agenda-sunrise-sunset
                    ;; "[" 'org-agenda-manipulate-query-add
                    ;; "b" 'org-agenda-earlier
                    ;; "c" 'helm-org-capture-templates
                    ;; "e" 'org-agenda-set-effort
                    ;; "f" 'org-agenda-later
                    ;; "gJ" 'org-agenda-clock-goto
                    ;; "gh" 'org-agenda-holiday
                    ;; "gj" 'org-agenda-goto-date
                    ;; "gv" 'org-agenda-view-mode-dispatch
                    ;; "i" 'org-agenda-clock-in-avy
                    ;; "n" 'org-agenda-add-note
                    ;; "o" 'delete-other-windows
                    ;; "va" 'org-agenda-archives-mode
                    ;; "vc" 'org-agenda-show-clocking-issues
                    ;; "vd" 'org-agenda-day-view
                    ;; "vl" 'org-agenda-log-mode
                    ;; "vt" 'org-agenda-toggle-time-grid
                    ;; "vw" 'org-agenda-week-view
                    ;; "x" 'org-agenda-exit
                    ;; "y" 'org-agenda-todo-yesterday
                    ;; "{" 'org-agenda-manipulate-query-add-re
                    ;; "}" 'org-agenda-manipulate-query-subtract-re
                    ;; "]" 'org-agenda-manipulate-query-subtract)
		    )
        )
    )
)

;; (use-package quelpa)
;; (use-package quelpa-use-package)

;; (use-package org-ql
;;  :defer
;;   :quelpa (org-ql
;;             :fetcher github
;;             :repo "alphapapa/org-ql"
;;             :files (:defaults (:exclude "helm-org-ql.el"))
;;             )
;;   )

;; (use-package org-sidebar
;;   :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar"))

; TODO: understand how it works
;; (defun skril/org-sidebar ()
;;   "Display my Org Sidebar."
;;   (interactive)
;;   (org-sidebar
;;    :sidebars (make-org-sidebar
;;               :name "My Sidebar"
;;               :description "My sidebar items"
;;               :items (org-ql (org-agenda-files)
;;                        (and (not (done))
;;                             (or (deadline auto)
;;                                 (scheduled :on today)))
;;                        :action element-with-markers))))

; ----------------------------------- UI --------------------------------------


(set-face-attribute 'default nil :height 170 :font "Fira Mono")
;; (set-frame-parameter (selected-frame) 'alpha 93)
;; (add-to-list 'default-frame-alist '(alpha  93))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	)
  ;; (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'doom-city-lights t))
  (load-theme 'doom-horizon t))

(use-package doom-modeline
  :ensure t
  :init
    (setq doom-modeline-height 20)
  :hook (after-init . '(doom-modeline-mode t)))

;; (use-package doom-modeline
;;   :ensure t
;;   ;; :init
;;   ;; :custom ((doom-modeline-height 1))
;;   )

; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner 2)
    (setq dashboard-items '((recents  . 5)
			    (agenda . 5)
			    (bookmarks . 5)))
    (setq dashboard-center-content t))

(use-package all-the-icons)

; TODO setup increment/decrement like in vim

;(use-package evil-numbers
  ;:config
    ;(define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
    ;(define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt))

; Code completion

(use-package bug-hunter)
(use-package auto-complete
  :config (ac-config-default))
(use-package relint)
