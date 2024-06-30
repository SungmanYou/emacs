;;; package --- Summary
;;; Commentary:
;;; Code:

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure use-package is always used
(require 'use-package)
(setq use-package-always-ensure t)

;; Set default font and font size
(set-face-attribute 'default nil
                    :family "Fira Code Retina" ;; Replace with your preferred font
                    :height 130) ;; Font size in 1/10pt, so 120 means 12pt

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Disable toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Enable column numbers
(column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Backup files configuration
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave files configuration
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-saves/" t)))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Better handling of text wrapping
(global-visual-line-mode t)

;; Enable clipboard integration
(setq select-enable-clipboard t)

;; Auto-revert buffers of changed files
(global-auto-revert-mode t)

;; Ivy, Counsel, and Swiper
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Which-key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; Company
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode 1))

;; Python (with elpy)
(use-package elpy
  :init
  (elpy-enable))

;; JavaScript, TypeScript, HTML, and CSS
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq js2-basic-offset 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))

;; Prettier
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

;; Emmet mode for HTML and CSS
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; LSP (Language Server Protocol) configuration
(use-package lsp-mode
  :hook ((js2-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; Org Mode Enhancements
(use-package org
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-ellipsis " ▼ "))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-icon-type 'all-the-icons)
(add-to-list 'dashboard-items '(agenda) t)
(setq dashboard-week-agenda t)
(setq dashboard-projects-backend 'projectile)
(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
(setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
			(registers . 5)))

;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc.)
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Unbind S-SPC (shift + space) to disable toggle-korean-input-method
(global-unset-key (kbd "S-SPC"))



;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
   '(lsp-ivy lsp-treemacs lsp-ui lsp-mode emmet-mode prettier-js typescript-mode which-key web-mode vue-mode visual-fill-column treesit-auto svelte-mode rainbow-delimiters org-bullets magit js2-mode ivy-rich helpful flycheck elpy doom-themes doom-modeline dashboard counsel-projectile command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
