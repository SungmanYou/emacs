(setq inhibit-startup-message t)

(scroll-bar-mode -1)	; Disable visible scrollbar
(tool-bar-mode -1)	; Disable the toolbar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 5)	; Give some breathing room
(menu-bar-mode -1)	; Disable the menu bar


;; Set up the visible bell
(setq visible-bell t)


;; Fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)


;; Theme
(load-theme 'wombat)


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents(package-refresh-contents))
(unless (package-installed-p 'use-package)(package-install 'use-package)) ;; Initialize use-package on non-Linux platforms


(require 'use-package)
(setq use-package-always-ensure t)


(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook  eshell-mode-hook)) ;; Disable line numbers for some modes
  (add-hook mode(lambda() (display-line-numbers-mode 0))))


(use-package command-log-mode)


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


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(use-package doom-themes)

;; (use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idel-delay 0.3))


(use-package ivy-rich
  :init (ivy-rich-mode 1))


(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" default))
 '(package-selected-packages
   '(doom-themes helpful counsel ivy-rich which-key rainbow-delimiters swiper doom-modeline ivy command-log-mode monokai-theme expand-region)))
