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

; 
(add-hook 'text-mode-hook 'visual-line-mode)

; ---------------------- Package management -----------------------------------
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
    '(
      ("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
      ("melpa" . "https://melpa.org/packages")
      ("Org" . "https://orgmode.org/elpa/")
      ("elpa" . "https://elpa.gnu.org/packages/")
      )
    )

(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t)
    )

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

; (defun skril/evil-hook ()
;   (dolist (mode
;             '(custom-mode
;               eshell-mode
;               git-rebase-mode
;               erc-mode
;               term-mode
;               )
;             )
;     (add-to-list 'evil-emacs-state-modes mode)))

; Collection of useful keybindings for different modes, that are not handled by
; evil package by itself
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
		"o" 'counsel-find-file
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
        ; Content
        ("music" . ?m)
        ("film" . ?f)
        ("book" . ?b)
        ("software" . ?S)
        ("game" . ?G)
        ("cloth" . ?c)
        ; Music actions
        ("fix" . ?F)
        ("download" . ?d)
        ; Areas
        ("work" . ?w)
        ("study" . ?s)
        ("people" . ?p)
        ; Actions
        ("think" . ?t)
        ("goal" . ?g)
        ("buy" . ?B)
        ("note" . ?n)
        ; Other
        ("relevant" . ?r)
        ("project" . ?P)
    ))
)

(defun skril/org-templates-setup ()
    (setq org-capture-templates
          '(
            ("i" "Inbox" entry (file "~/Personal Library/Org/Desktop.org")
             "* INBOX %?\n:PROPERTIES:\n:CREATED :%U\n:END:\n")
            )
          )
    ;; (add-to-list 'org-structure-template-alist
    ;;     '("i" . "* INBOX %^{Inbox item}\n:PROPERTIES:\n:CREATED :%U\n:END:\n %^{Additional information}"))
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
        (sequence "INBOX(i)" "PROJECTS(p)" "SOMEDAY(s)" "TODO(t)" "WAITING(w)" "CALENDAR(c)" "|" "DONE(d)" "CANCELLED(x)")
    ))
    (setq org-todo-keyword-faces '(
        ("INBOX" . "white smoke")
        ("PROJECTS" . "yellow green")
        ("SOMEDAY" . "coral")
        ("TODO" . "gold")
        ("WAITING" . "dark violet")
        ("CALENDAR" . "light sea green")
        ("DONE" . "slate gray")
        ("CANCELLED" . "slate gray")
        )
    )
)

(defun skril/org-views-setup ()
    (setq org-agenda-custom-commands '(
        ("m" "Music entries" (
             (org-ql-block
                '(and(tags "music") (tags "fix"))
                ((org-ql-block-header "Fix"))
                )
              (org-ql-block
                 '(and (todo "TODO")
                      (and (tags "music") (tags "download"))
                 )
                 ((org-ql-block-header "TODO music download"))
                )
             (org-ql-block
                '(and (todo "SOMEDAY")
                     (and (tags "music") (tags "download"))
                     )
                  ((org-ql-block-header "SOMEDAY music download"))
                  )
              )
         )
        ("f" "FILMS" (
                  (org-ql-block
                    '(and (tags "film")
                          (not (or (todo "DONE") (todo "CANCELLED"))))
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
                    (tags "people"))
                    ((org-ql-block-header "People")))
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
                     (tags "people")
                     (tags "note"))))
                    ((org-ql-block-header "Other")))
            (org-ql-block '(and (todo "TODO")
                    (and(tags "music") (tags "download")))
                    ((org-ql-block-header "Music download")))
            ))
        ))
  )

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
                    ;; "n" 'org-agenda-add-note
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
                    "]" 'org-agenda-manipulate-query-subtract)
        )
    )
)

; Uncomment for updating it, but it makes startup time slower
; (use-package quelpa)
; (use-package quelpa-use-package)
; (use-package org-ql
;  :defer
;   :quelpa (org-ql
;             :fetcher github
;             :repo "alphapapa/org-ql"
;             :files (:defaults (:exclude "helm-org-ql.el"))
;             )
;   )

; ----------------------------------- UI --------------------------------------


(set-face-attribute 'default nil :height 150 :font "Fira Mono")
;; (set-frame-parameter (selected-frame) 'alpha 93)
;; (add-to-list 'default-frame-alist '(alpha  93))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-tokyo-night t)
  (load-theme 'doom-Iosvkem t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 1)))

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
