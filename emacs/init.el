(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startpage-message t)

;;(server-start)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(set-frame-parameter (selected-frame) 'alpha '(85))
(add-to-list 'default-frame-alist '(alpha 85))

(menu-bar-mode -1)

                                        ;     (setq display-graphic-p t)

(setq visible-bell nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-face-bold-p 'bold nil)
(set-face-attribute 'default nil
                    :font "firacode:antialias=true"
                    :height 80)

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
 '(custom-safe-themes
   '("88c59500c520bdba6c6a164deb767b3b14c6a9686a24781adb3b9a1a883b9e50" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "f8925b6e0b5efdefece2eff53597a746cd47f4aa097942db2ebda82b7b9b3670" default))
 '(elfeed-feeds '("https://reddit.com/r/emacs.rss") t)
 '(ivy-mode t)
 '(org-agenda-files '("~/Documents/org/org-agenda.org"))
 '(package-selected-packages
   '(material-theme zig-mode which-key weechat vertico use-package undo-tree typescript-mode twittering-mode treemacs-evil symon spotify smart-mode-line rust-mode reddigg ranger rainbow-delimiters projectile-ripgrep powerline-evil poly-R pandoc ox-pandoc org-super-agenda org-evil org-bullets org-auto-tangle nordless-theme nord-theme no-littering nnreddit nixos-options nix-mode mpdmacs minions mastodon marginalia magithub lsp-ui lsp-latex lsp-javacomp lsp-java lsp-ivy lsp-haskell latex-preview-pane jabber ivy-rtags ivy-rich ivy-mpdel horizon-theme helpful handoff gruvbox-theme go-mode general geiser-mit fira-code-mode exwm-x exwm-surf exwm-firefox-evil ewal-doom-themes evil-tutor evil-nerd-commenter evil-collection eterm-256color ess-r-insert-obj ess-R-data-view emojify elfeed-web elfeed-org doom-modeline doom dmenu dired-toggle dashboard counsel-projectile company-rtags company-auctex command-log-mode color-theme-x cmake-ide base16-theme auto-org-md atom-one-dark-theme all-the-icons-gnus ac-geiser)))

(load-theme 'doom-gruvbox)

(global-display-line-numbers-mode t)

(use-package general
  :config
  (general-create-definer basqs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (basqs/leader-keys
    "c" '(kill-buffer :which-key "kill buffer")
    "TAB" '(counsel-switch-buffer :which-key "change buffer")
    "t" '(:ignore t :which-key "toggles")
    "tl" '(toggle-truncate-lines :which-key "truncate lines")
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

(basqs/leader-keys
  "w"  '(:ignore t :which-key "windows")
  "wc" 'evil-window-delete
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "l"  'evil-window-next
  "h"  'evil-window-prev)

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

(defun efs/configure-eshell ()
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

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

(use-package dired-toggle)

(basqs/leader-keys
  "." '(counsel-find-file :which-key "find-file")
  )

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))

                                        ;     (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

(setq dashboard-set-navigator t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts t)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        ;;(projects . 5)
                        (agenda . 10)))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode)
  (display-battery-mode))
(set-face-attribute 'mode-line nil :family "firacode" :height 80)
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

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Documents/git"))

(defun basqs/org-mode-setup ()
  (org-indent-mode)
  (variable-putch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq truncate-lines t)
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

(use-package org-evil)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("md" . "src markdown"))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
