
;; -*- lexical-binding: t; -*-

(message "Start reading ~/.emacs.d/init.el ...")

;; --------------------------------------------------------------------------------------------
;; STARTUP SPEEDUP
;; --------------------------------------------------------------------------------------------

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq read-process-output-max (* 1024 1024))

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; --------------------------------------------------------------------------------------------
;; INIT CONFIG
;; --------------------------------------------------------------------------------------------

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)

;; Enable / disable displaying LR/CR characters
;; (global-whitespace-mode nil)

;; Enable mouse support in terminal Emacs
(xterm-mouse-mode 1)

;; Enable auto revert mode globally, so that all buffers will be in sync with whats actually on disk.
;; If you are sure that the file will only change by growing at the end, use Auto Revert Tail mode instead, as
;; it is more efficient for this.
(global-auto-revert-mode t)

;; How to get colors in terminal Emacs ?
;; https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY

;; Mouse behaviour
(setq mouse-wheel-progressive-speed nil)

;; Enable full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Do not create backup files (with ~ suffix)
(setq make-backup-files nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

(setq-default show-trailing-whitespace nil)
(setq-default explicit-shell-file-name "/bin/bash")

;; --------------------------------------------------------------------------------------------
;; CONFIGURATION FILES CONFIG
;; --------------------------------------------------------------------------------------------

;; Do not use `init.el` for `custom-*` code (generated by 'M-x customize' menu) - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Use default Emacs bookmarks localisation (for now)
(setq bookmark-default-file "~/.emacs.d/bookmarks")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

;; Load custom Emacs Lisp files (libs, functions, etc.)
(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'custom-functions)

;; --------------------------------------------------------------------------------------------
;; SETUP PROXY SERVICES
;; --------------------------------------------------------------------------------------------
; (setq url-proxy-services
;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;      ("http" . "10.158.100.2:8080")
;      ("https" . "10.158.100.2:8080")))

; (setq url-proxy-services nil)

;; --------------------------------------------------------------------------------------------
;; SETUP PACKAGE REPOSITORIES
;; --------------------------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/")     t)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")        t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load Emacs Lisp packages, and activate them - variable ‘package-load-list’ controls which packages to load.
(package-initialize)

;; Update list of available packages - sth like 'git fetch'
;; doing it together with 'unless' reduces emacs startup time significantly
(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
        (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; --------------------------------------------------------------------------------------------
;; THEME
;; --------------------------------------------------------------------------------------------

(use-package spacemacs-theme
  :ensure t
  :defer t
  :custom
        (setq spacemacs-theme-comment-bg nil)
        (setq spacemacs-theme-comment-italic t)
  :init (load-theme 'spacemacs-dark t))

;; --------------------------------------------------------------------------------------------
;; DASHBOARD
;; --------------------------------------------------------------------------------------------

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((projects  . 5)
			  (recents . 10)))
  (dashboard-setup-startup-hook))

;; --------------------------------------------------------------------------------------------
;; PACKAGES
;; --------------------------------------------------------------------------------------------

(use-package command-log-mode
  :ensure t
  :bind (("C-c c t" . clm/toggle-command-log-buffer)
	 ("C-c c o" . clm/open-command-log-buffer)
	 ("C-c c x" . clm/close-command-log-buffer)
	 ("C-c c c" . clm/command-log-clear)
	 ("C-c c s" . clm/save-command-log)
	 )
  :custom
  ;; disable default keybinding "C-c o" that opens command-log-buffer
  (command-log-mode-key-binding-open-log nil)
  :config
  ;; Enable command-log-mode globally by default
  (global-command-log-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package idle-highlight-mode
  :ensure t
  :custom
  (idle-highlight-idle-time 0.1)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method (quote character)))

(use-package drag-stuff
  :ensure t)

(use-package dumb-jump
  :ensure t)

;; Load Silver Searcher
(use-package ag
  :ensure t)

;; Load ripgrep
(use-package rg
  :ensure t)

(use-package xref
  :ensure t)

(use-package all-the-icons
  :ensure t)

(unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))

(use-package doom-modeline
  :after (all-the-icons)
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'auto
        doom-modeline-height 40)
  :custom
  (display-battery-mode t))

;; (use-package keycast
;;   :config
;;   ;; This works with doom-modeline, inspired by this comment:
;;   ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
;;   (define-minor-mode keycast-mode
;;     "Show current command and its key binding in the mode line."
;;     :global t
;;     (if keycast-mode
;; 	(add-hook 'pre-command-hook 'keycast--update t)
;;       (remove-hook 'pre-command-hook 'keycast--update)))
;;   (add-to-list 'global-mode-string '("" mode-line-keycast " "))
;;   (keycast-mode nil))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)

(use-package dired
  :ensure nil ;; dires is a built-in emacs package, so don't look for it in package repositories
  :commands (dired dired-jump) ;; defer this config until one of this commands is executed
  :bind (("C-x j" . dired-jump)
         ;; those bindings will only be valid if dired-mode is active
         :map dired-mode-map
         ;; change this from ^ which is not convenient
         ("<C-backspace>" . dired-up-directory)
         ;; this one is a default keybinding, keep it here as an information tough
         ("v" . dired-view-file))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  )

;; Thanks to this package, the directories that we've visited won't be existing as opened buffers.
;; Instead, all these buffers will be closed automatically.
(use-package dired-single
  :after (dired)
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("<C-return>" . dired-single-up-directory)
              ("<return>"   . dired-single-buffer)))

;; This package has been replaced with "treemacs-icons-dired"
;; (use-package all-the-icons-dired)

;;  This package allow us to set a program different than Emacs, that we want to open given files with
(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "gwenview")
                                ("jpg" . "gwenview"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("h" . dired-hide-dotfiles-mode)))


(use-package magit
  :ensure t
  :custom
  (magit-status-buffer-switch-function 'switch-to-buffer)
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . magit-fetch)
         ("C-c g b" . magit-blame)
         ("C-c g r" . magit-branch)
         ("C-c g c" . magit-checkout)))

(use-package projectile
  :ensure t
  :bind (
         ("<f7>"  . projectile-add-known-project)
         )
  :init
  (projectile-mode 1))

(use-package treemacs
  :ensure t
  :bind (
         ("<C-f5>" . treemacs)
         ("<C-f6>" . treemacs-add-project-to-workspace)
         )
  :config
  (treemacs-follow-mode t)
  (treemacs-git-mode 'deferred)
  ;; Make tremacs display nice indents in files hierarchy
  ;; (treemacs-indent-guide-mode 't)
  ;; (treemacs-indent-guide-style 'line)
  :custom
  ;; This fixes problem with helm buffers (e.g. helm-M-x)
  ;; ocupying the entire widow
  (treemacs-display-in-side-window nil)
  ;; Set default treemacs width, and unlock the
  ;; drag-and-drop resize option
  (treemacs-width 50)
  (treemacs-width-is-initially-locked nil)
  ;; Disable test wrapping in treemacs window, when widnow is to narrow
  (treemacs-wrap-around nil)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))
  :ensure t

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package helm
  :ensure t
  :bind  (("M-x"     . helm-M-x)
          ("M-y"     . helm-show-kill-ring)
          ("C-x C-f" . helm-find-files)
          ("C-b"     . helm-buffers-list)
          ("C-c h o" . helm-occur)
          ("C-c h b" . helm-filtered-bookmarks)
          )
  :custom
  (helm-position 'bottom)
  ;; This fixes problem with helm buffers (e.g. helm-M-x)
  ;; ocupying the entire widow.
  ;; Although "helm-split-window-in-side-p" is deprecated
  ;; and superseeded "helm-split-window-inside-p", both
  ;; variables have to be set to t.
  (helm-split-window-in-side-p t)
  (helm-split-window-inside-p t)
  :init
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-swoop
  :after helm
  :ensure t
  :bind
  (("M-s"     . helm-swoop))
  :custom
  ;; This decreases helm swoop speed but in favour of colorded results
  (helm-swoop-speed-or-color t)
  ;; This fixes problem with helm-swoop appearing in another window,
  ;; when using multiple windows in one frame (treemacs / minimap)
  (helm-swoop-split-with-multiple-windows t)
  )

(use-package helm-projectile
  :after (helm projectile)
  :ensure t
  :bind(
        ("C-p"   . helm-projectile-find-file)
        ("C-l"   . helm-projectile-recentf)
        ("<f8>"  . helm-projectile-switch-project)
        ))

(use-package helm-icons
  :after helm
  :ensure t
  :config
  (helm-icons-enable))

(use-package helm-ag
  :after helm
  :ensure t)

(use-package helm-xref
  :after helm
  :ensure t
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package minimap
  :ensure t
  :custom
  (minimap-always-recenter nil)
  (minimap-hide-fringes t)
  (minimap-hide-scroll-bar nil)
  (minimap-highlight-line nil)
  (minimap-minimum-width 20)
  (minimap-recenter-type (quote relative))
  (minimap-recreate-window t)
  (minimap-update-delay 0)
  (minimap-width-fraction 0.06)
  (minimap-window-location (quote right))
  :custom-face
  (minimap-active-region-background ((((background dark)) (:background "#3c3c3c" :extend t)) (t (:background "#C847D8FEFFFF" :extend t))))
  (minimap-font-face ((t (:weight bold :height 15 :width normal :family "DejaVu Sans Mono"))))
  :config
  (minimap-mode -1))

 (use-package verilog-mode
   :ensure t
   :custom
   (verilog-align-ifelse t)
   (verilog-auto-delete-trailing-whitespace t)
   (verilog-auto-indent-on-newline t)
   (verilog-auto-newline nil)
   (verilog-highlight-grouping-keywords t)
   (verilog-highlight-modules t)
   (verilog-indent-level 2)
   (verilog-indent-level-behavioral 2)
   (verilog-indent-level-declaration 2)
   (verilog-indent-level-directive 0)
   (verilog-indent-level-module 2))

(use-package python-mode
  :ensure t
  :hook
  (python-mode . lsp-deferred)
  (python-mode . (lambda ()
                   (setq indent-tabs-mode nil)
                   (setq tab-width 4)
                   (setq python-indent-offset 4)))
  :custom
  (python-shell-interpreter "python3"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  ;; Unlike python-mode, this mode follows the Emacs convention of not
  ;; binding the ENTER key to `newline-and-indent'.  To get this
  ;; behavior, add the key definition to `yaml-mode-hook'
  :hook (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; (use-package c-mode
  ;; :ensure t)

 (use-package c++-mode
   :ensure nil
   ;; open .tpp files with c++-mode
   :mode ("\\.tpp\\'" . c++-mode))

(use-package docstr
  :ensure t
  :hook
  (c++-mode . docstr-mode)
  :config
  (setq docstr-key-support t))

(use-package groovy-mode
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

;; --------------------------------------------------------------------------------------------
;; KEY BINDINGS
;; --------------------------------------------------------------------------------------------

(global-set-key (kbd "M-v")        #'my/scroll-half-page-down)
(global-set-key (kbd "C-v")        #'my/scroll-half-page-up)

(global-set-key (kbd "<f5>")       #'my/revert-buffer)
(global-set-key (kbd "<f6>")       #'my/kill-asterisk-buffers)
(global-set-key (kbd "<f9>")       #'minimap-mode)
(global-set-key (kbd "<f12>")      #'xref-find-definitions)

(global-set-key (kbd "<prior>")    #'drag-stuff-up)
(global-set-key (kbd "<next>")     #'drag-stuff-down)

(global-set-key (kbd "C-x 0")      #'kill-buffer-and-window)
(global-set-key (kbd "C-c d")      #'my/duplicate-current-line-or-region)
(global-set-key (kbd "C-c k")      #'kill-whole-line)
(global-set-key (kbd "C-c l")      #'my/kill-word-at-point)
(global-set-key (kbd "C-c s")      #'my/kill-sentence-at-point)
(global-set-key (kbd "C-c x")      #'delete-trailing-whitespace)
(global-set-key (kbd "C-c w")      #'my/toggle-highlight-trailing-whitespaces)
(global-set-key (kbd "C-c e")      #'global-whitespace-mode)
(global-set-key (kbd "C-c h")      #'my/toggle-idle-highlight-mode)
(global-set-key (kbd "C-c C-e")    #'eval-region)
(global-set-key (kbd "C-c t")      #'my/untabify-entire-buffer)

(global-set-key (kbd "C-c o i")    #'my/open-init-file)
(global-set-key (kbd "C-c o f")    #'my/open-custom-functions-file)
(global-set-key (kbd "C-c o c")    #'my/open-customization-file)

(global-set-key (kbd "C-c p r")    #'helm-projectile-recentf)
(global-set-key (kbd "C-c p R")    #'projectile-replace)
(global-set-key (kbd "C-c p x")    #'projectile-replace-regexp)
(global-set-key (kbd "C-,")        #'helm-projectile-grep)
(global-set-key (kbd "C-.")        #'helm-projectile-ag)

(define-key helm-map (kbd "TAB")   #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   #'helm-select-action)

(global-set-key (kbd "C-,")        #'helm-projectile-grep)
(global-set-key (kbd "C-.")        #'helm-projectile-ag)

(eval-after-load 'verilog-mode
  '(define-key verilog-mode-map (kbd "C-{") 'verilog-beg-of-defun))

(eval-after-load 'verilog-mode
  '(define-key verilog-mode-map (kbd "C-}") 'verilog-end-of-defun))

;; --------------------------------------------------------------------------------------------
;; HOOKS
;;
;; List of emacs hooks:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;; --------------------------------------------------------------------------------------------

;; PROG
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; XREF
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; MINIBUFFER
(defun my/minibuffer-setup ()
  "Function sets font size in the minibuffer"
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.3))))

(add-hook 'minibuffer-setup-hook 'my/minibuffer-setup)

;; --------------------------------------------------------------------------------------------
;; OTHER SETTINGS
;; --------------------------------------------------------------------------------------------

(message "... finished reading ~/.emacs.d/init.el")

