(setq inhibit-startpage-message t)

;;(server-start)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(set-frame-parameter (selected-frame) 'alpha '(85))
(add-to-list 'default-frame-alist '(alpha 85))

(menu-bar-mode -1)

(setq visible-bell nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil
                      :font "Terminus"
                      :height 80)

;  (setq default-frame-alist '((font . "Terminus")))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(require 'package)

(setq package-archives '(("melpa" ."https://melpa.org/packages/")
                         ("org" . "https:/orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

      (package-initialize)
      (unless package-archive-contents
        (package-refresh-contents))

      (unless (package-installed-p 'use-package)
        (package-install 'use-package))

      (require 'use-package)
      (setq use-package-always-ensure t)

(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   '("88c59500c520bdba6c6a164deb767b3b14c6a9686a24781adb3b9a1a883b9e50" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "f8925b6e0b5efdefece2eff53597a746cd47f4aa097942db2ebda82b7b9b3670" default))
 '(elfeed-feeds '("https://reddit.com/r/emacs.rss"))
 '(fci-rule-color "#383a42")
 '(ivy-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files '("~/Documents/org/org-agenda.org"))
 '(package-selected-packages
   '(marginalia vertico lsp-latex typescript-mode lsp-haskell lsp-javacomp lsp-java company-rtags ivy-rtags rtags cmake-ide treemacs-evil ranger elfeed-org elfeed-web go-mode exwm-x exwm-firefox-evil exwm-surf dmenu poly-R ess-view ess-r-insert-obj ess-R-data-view ess nix-mode nixos-options autothemer company-c-headers company-lua company-plsense company-php company-shell company-web company-auctex minions emojify markdown-preview-eww ghub+ ghub mingus kaolin-themes poet-theme company lsp-ivy lsp-treemacs no-littering w3m yaml-mode elfeed latex-preview-pane auto-complete-auctex auctex dired-open all-the-icons-dired eterm-256color epc w3 webkit rust-mode haskell-mode dashboard evil-magit magit projectile hydra sorcery-theme almost-mono-themes evil-collection undo-tree evil-mode evil-tutor evil general doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline command-log-mode use-package))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'doom-gruvbox)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))

  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package general
  :config
(general-create-definer basqs/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(basqs/leader-keys
 "c" '(kill-buffer :which-key "kill buffer")
 "TAB" '(counsel-switch-buffer :which-key "change buffer")
 "." '(counsel-find-file :which-key "find-file")
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "chose theme")))

(defun basqs/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(use-package undo-tree
      :init
      (global-undo-tree-mode 1))

    (use-package evil
      :init
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-C-i-jump nil)
      (setq evil-respect-visual-line-mode t)
      (setq evil-undo-system 'undo-tree)
      :config
      (add-hook 'evil-mode-hook 'basqs/evil-hook)
      (evil-mode 1)
      (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
      (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

      ;; Use visual line motions even outside of visual-line-mode buffers
      (evil-global-set-key 'motion "j" 'evil-next-visual-line)
      (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

      (evil-set-initial-state 'messages-buffer-mode 'normal)
      (evil-set-initial-state 'dashboard-mode 'normal))

    (use-package evil-collection
      :after evil
      :init
      (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
      :custom
      (evil-collection-outline-bind-tab-p nil)
      :config
      (setq evil-collection-mode-list
            (remove 'lispy evil-collection-mode-list))
      (evil-collection-init))

;;  (use-package evil-nerd-commenter)

(use-package command-log-mode)

  (use-package ivy
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

  (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

  (use-package no-littering)

  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


  ;; minimize garbage collection during startup
  (setq gc-cons-threshold most-positive-fixnum)

  ;; lower threshold back to 8 mib (default is 800kb)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (expt 2 23))))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key mode
  :config
(setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpfullkey))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  ;;(setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell))

(basqs/leader-keys
  "e" 'eshell)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(basqs/leader-keys
  "w"  '(:ignore t :which-key "windows")
  "wc" 'evil-window-delete
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "l"  'evil-window-next
  "h"  'evil-window-prev)

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

(setq dashboard-set-navigator t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts t)

(setq dashboard-items '((recents  . 5)
			    (bookmarks . 5)
;;                      (projects . 5)
                        (agenda . 10)))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode)
    (display-battery-mode))
(set-face-attribute 'mode-line nil :family "terminus" :height 80)
(setq doom-modeline-height 14)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-modal-icon t)
(setq doom-modeline-mu4e t)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-themes)

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 2)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished :exit t"))

(basqs/leader-keys
"ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'basqs/switch-project-action))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(basqs/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(defun basqs/org-mode-setup ()
    (org-indent-mode)
    (variable-putch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq evil-auto-indent nil))

  (use-package org
    :hook (org-mode . basqs/org-mode-setup)
    :config
    (setq org-ellipsis " ▾"
                                          ;	org-hide-emphasis-markers t
          ))

  (basqs/leader-keys
    "o"   '(:ignore t :which-key "org")
    "od"  'org-toggle-checkbox
    "ot"  'org-todo
    "oa"  'org-agenda
    "os"  'org-schedule
    "on"  'org-agenda-file-to-front
    "ob"  '(:ignore b :which-key "babel")
    "obt" 'org-babel-tangle
    "ol"  'org-insert-link)

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp .t)
     (shell . t)))

(setq org-confirm-babel-evaluate nil)

(use-package org-auto-tangle)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(use-package latex-preview-pane)

(load "auctex.el" nil t t)
    (require 'tex-mik)

   (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)

    (setq TeX-PDF-mode t)

(use-package pandoc)
(use-package ox-pandoc)

(use-package lsp-ivy
        :commands lsp-ivy-workspace-symbol)

    (setq company-format-margin-function nil)
      (add-hook 'after-init-hook 'global-company-mode)

    (use-package autothemer
        :ensure t)

      (font-lock-add-keywords
       'latex-mode
             '(("\\\\quad" 0 my-new-face prepend)
               ("\\\\label" 0 my-another-new-face prepend)))

    ;; (setq ess-ask-about-transfile t)

    (require 'rtags)
    (require 'company-rtags)

    (setq rtags-completions-enabled t)
    (eval-after-load 'company
        '(add-to-list
              'company-backends 'company-rtags))
    (setq rtags-autostart-diagnostics t)
          (rtags-enable-standard-keybindings)


    (use-package lsp-mode
        :commands (lsp lsp-deferred)
        :init
        (setq lsp-keymap-prefix "C-c l")
        :config
        (lsp-enable-which-key-integration t))

    (require 'lsp)

    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast

(use-package geiser)

(use-package geiser-mit)

(use-package ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; Set our nickname & real-name as constant variables
  (setq
   erc-nick "basqs"     ; Our IRC nick
   erc-user-full-name "basqs") ; Our /whois name

  ;; Define a function to connect to a server
  (defun some-serv ()
    (lambda ()
      (interactive)
    (erc :server "irc.libera.chat"
         :port   "6697")))

  ;; Or assign it to a keybinding
  ;; This example is also using erc's TLS capabilities:
;  (global-set-key "\C-cen"
;                  (lambda ()
;                    (interactive)
;                    (erc-tls :server ""
;                             :port   "6697")))

(use-package jabber)

(setq 
 special-display-regexps 
 '(("jabber-chat" 
     (width . 80)
    (scroll-bar-width . 16)
    (height . 15)
    (tool-bar-lines . 0)
    (menu-bar-lines 0)
    (font . "Terminus")
    (left . 80))))

(delete 'mu4e evil-collection-mode-list)
(delete 'mu4e-conversation evil-collection-mode-list)

(use-package mu4e
  :ensure nil
;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
;; :defer 20 ; Wait until 20 seconds after startup
  :config

;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail/")

  (setq mu4e-drafts-folder "/Gmail/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/Gmail/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/Gmail/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/Gmail/[Gmail]/Trash")

  (setq mu4e-maildir-shortcuts
    '((:maildir "/Gmail/Inbox"    :key ?i)
      (:maildir "/Gmail/[Gmail]/Sent Mail" :key ?s)
      (:maildir "/Gmail/[Gmail]/Trash"     :key ?t)
      (:maildir "/Gmail/[Gmail]/Drafts"    :key ?d)
      (:maildir "/Gmail/[Gmail]/All Mail"  :key ?a))))
