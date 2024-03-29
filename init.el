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

; ------------------------ Useful functions -----------------------------------

(defun add-list-to-list (dst src)
        (mapcar '(lambda (e) (add-to-list dst e)) src))

; ------------------------- Config Settings -----------------------------------

(defvar settings-org-folder "~/Vault/Personal/Org/" "Location of all the org files")
(defvar settings-capture-file-name "Work.org" "Name of the file in org folder to which inbox entries are written")
(defvar settings-archive-file-name "Archive.org" "Name of the file in org folder to which to archive")


(setq settings-capture-file (concat settings-org-folder settings-capture-file-name))
(setq settings-archive-file (concat settings-org-folder settings-archive-file-name))
; (setq settings-archive-file '(concat settings-org-folder "Archive.org"))


; ---------------------- Package management -----------------------------------
(require 'package)

(add-list-to-list 'package-archives
      (list
        '("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
        '("melpa" . "https://melpa.org/packages")
        '("org" . "https://orgmode.org/elpa/")
        '("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
          (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; ------------------------ Key bindings ---------------------------------------

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
	    ("videos" . ?v)
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
	    ("@laptop" . ?l)
	    ("quick" . ?q) ; less than 10 minutes
	    ("dude" . ?D) ; dude = frog,  a hard task, can be done in the morning
	    ("launchpad" . ?L)
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
	(:endgroup))))

(defun skril/org-templates-setup ()
    (setq org-capture-templates
          '(
            ("i" "Inbox" entry (file settings-capture-file)
             "* INBOX %?\n:PROPERTIES:\n:CREATED :%U\n:END:\n"))))

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
            :height (cdr face))))

(defun skril/org-files-setup ()
    (setq org-archive-location (concat settings-archive-file "::Archived entries from file %s"))
    (add-to-list 'org-agenda-files (expand-file-name settings-org-folder))
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3))))

(defun skril/org-todo-setup ()
    (setq org-todo-keywords '(
      (sequence
          "INBOX(i)"
          "RELEVANT(r)"
          "SOMEDAY(s)"
          "NOTES(n)"
          "LIST(l)"
          "WAITING(w)"
          "PROJECTS(p)"
          "TODO(t)"
          "|"
          "DONE(d)"
          "TRASH(x)")))
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
        ("LIST" . "royal blue")
        ("WAITING" . "dark violet")
        ("PROJECTS" . "forest green")
        ("TODO" . "gold")
        ("DONE" . "slate gray")
        ("TRASH" . "slate gray"))))

(defun skril/org-views-setup ()
  (setq org-agenda-custom-commands
	'(
	  ("mf" "Music fix"
	   ((tags-todo "+music+fix"
		       ((org-agenda-overriding-header "Fix")))))
	  ("md" "Music download"
	   ((tags-todo "+music+download/+TODO"
		((org-agenda-overriding-header "TODO")))
	    (tags-todo "+music+download/+RELEVANT"
		((org-agenda-overriding-header "Relevant")))
	    (tags-todo "+music+download/+LIST"
		       ((org-agenda-overriding-header "Other")))
	    ))
	  ("mm" "Music entries"
	   ((tags-todo "+music-download-fix"
		((org-agenda-overriding-header "Music entries")))
	    ))
	  ("lf" "Films"
	   ((tags-todo "+film/+LIST"
		((org-agenda-overriding-header "LIST")))
	   (tags-todo "+film/-LIST"
		      ((org-agenda-overriding-header "Other")))
	   ))
	  ("lb" "Books"
	   ((tags-todo "+book/+LIST"
		((org-agenda-overriding-header "LIST")))
	   (tags-todo "+book/-LIST"
		      ((org-agenda-overriding-header "Other")))
	   ))
	  ("i" "Inbox"
	   ((tags-todo "/+INBOX"
		((org-agenda-overriding-header "Inbox")))
	   ))
	  ("p" "Projects"
	   ((tags-todo "/+PROJECTS"
		((org-agenda-overriding-header "Projects")))
	   ))
	  ("r" "Relevant"
	   ((tags-todo "/+RELEVANT"
		((org-agenda-overriding-header "Relevant")))
	   ))
	  ("s" "Someday"
	   ((tags-todo "/+SOMEDAY"
		((org-agenda-overriding-header "SOMEDAY")))
	   ))
	  ("n" "Notes"
	   ((tags-todo "/+NOTES"
		((org-agenda-overriding-header "Notes")))
	   ))
	  ("w" "Waiting"
	   ((tags-todo "/+WAITING"
		((org-agenda-overriding-header "Waiting")))
	   ))
	  ("x" "Dashboard"
	   (
	    (tags-todo "+PRIORITY=\"A\"/+TODO"
		((org-agenda-overriding-header "Important")))
	    (tags-todo "+PRIORITY=\"B\"/+TODO"
		((org-agenda-overriding-header "Should Do")))
	    (tags-todo "+PRIORITY=\"C\"/+TODO"
		((org-agenda-overriding-header "May skip")))
	    (tags-todo "/+PROJECTS"
		((org-agenda-overriding-header "Projects")))
	    (agenda "" ((org-deadline-warning-days 7)))
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
    (add-hook 'org-capture-after-finilize-hook :after 'org-save-all-buffers))

(defun skril/org-mode-setup ()
    (skril/org-tags-setup)
    (skril/org-headings-setup)
    (skril/org-templates-setup)
    (skril/org-files-setup)
    (skril/org-todo-setup)
    (skril/org-views-setup)
    (skril/org-autosave-setup))

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
		    ))))

; ----------------------------------- UI --------------------------------------


(set-face-attribute 'default nil :height 170 :font "Fira Mono")

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	)
  ;; (load-theme 'doom-tokyo-night t))
  ;; (load-theme 'doom-city-lights t))
  (load-theme 'doom-horizon t))

(use-package doom-modeline
  :ensure t
  :init
    (setq doom-modeline-height 20)
  :hook (after-init . '(doom-modeline-mode t)))

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
(use-package lispy
  :config (add-hook 'emacs-lisp-mode-hook #'lispy-mode))
(use-package auto-complete
  :config (ac-config-default))
(use-package relint)
