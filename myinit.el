(defun load-if-exists-and-readable (f)
  "Load the elisp file only if it exists & readable"
  "(load-if-exists-and-readable 'some/path/to/load/the/file/foo.el')"
  (if (file-readable-p f)
	(load-file f)
  )
)

(use-package atom-one-dark-theme
  :ensure t
)
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/atom-one-dark-theme-20170803.916/")
;; (load-theme 'atom-one-dark t)

(use-package zenburn-theme
  :ensure t
)
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/zenburn-theme-20170511.1337/")
(load-theme 'zenburn t)

(setq inhibit-startup-message t)

(tool-bar-mode -1)

(toggle-frame-maximized)

(global-linum-mode t)

(require 'linum)
(defun linum-update-window-scale-fix (win)
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(setq default-directory "/Users/sungman.you/Desktop/coding/projects/")

(defalias 'list-buffers 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)

;; (global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "S-SPC") (lambda() (interactive) (push-mark nil nil 1)))

(defun auto-indent ()
    (save-excursion
      (indent-region (point-min) (point-max))
  )
)
(global-set-key (kbd "C-M-\\") (lambda() (interactive) (auto-indent)))

;; Make it permanent
(show-paren-mode 1)
;; Deactivate delay
(setq show-paren-delay 0)
;; Change the color / face
(require 'paren)
(set-face-background 'show-paren-match "#000000")
(set-face-foreground 'show-paren-match "#ff4040")
(set-face-attribute 'show-paren-match nil :weight 'ultra-bold)

;; enable 'Octave Mode' automatically for all .m files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; to turn on the abbrevs, auto-fill and font-lock features automatically
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

;; (use-package try
;;   :ensure t
;; )

;; (use-package which-key
;;   :ensure t
;;   :config (which-key-mode)
;; )

(use-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-background nil)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 1.5)))
      )
    )
  )
)

(use-package counsel
  :ensure t
  :bind (("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map
	 ("M-y" . ivy-next-line))
)

(use-package swiper
  :ensure t
  :bind (
	 ("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 )
  :config (progn
	    (ivy-mode 1)
	    (setq ivy-use-virtual-buffers t)
	    (setq ivy-display-style 'fancy)
	    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
	    )
  )

(use-package auto-complete
  :ensure t
  :init (progn
          (ac-config-default)
          (global-auto-complete-mode t)
        )
)

;; (use-package ox-reveal
;;   :ensure ox-reveal
;; )
;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;; (setq org-reveal-mathjax t)

;; ;; For highlighting syntax in presentation
;; (use-package htmlize
;;   :ensure t
;; )

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
)

;; (use-package undo-tree
;;   :ensure t
;;   :init (global-undo-tree-mode)
;; )

;; (use-package aggresive-indent
;;   :ensure t
;;   :config (global-aggresive-indent-mode 1)
;; )

(use-package magit
  :ensure t
)
;; Key bindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Enable global magit file mode
(global-magit-file-mode t)

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode)
)

(use-package exec-path-from-shell
  :ensure t
)
(exec-path-from-shell-initialize)

(use-package js2-mode
:ensure t
)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(use-package typescript-mode
:ensure t
)
;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(use-package ng2-mode
:ensure t
)
(require 'ng2-mode)
(add-to-list 'auto-mode-alist '("\\.module.ts\\'" . ng2-ts-mode))

;; (use-package shell-pop
;; :ensure t
;; )
;; (require 'shell-pop)

(use-package smartparens
:ensure t
:config (require 'smartparens-config)
:init (smartparens-global-mode t))

;; (load "nameses")
;; (require 'desktop)
;; (require 'nameses)
;; (global-set-key (kbd "<f9>")     'nameses-load)
;; (global-set-key (kbd "C-<f9>")   'nameses-prev)
;; (global-set-key (kbd "C-S-<f9>") 'nameses-save)

(use-package expand-region
      :ensure t
      :config
      (global-set-key (kbd "C-=") 'er/expand-region))
(setq shift-select-mode nil) ;; When you expand region then C-g for some reason has conflict with transient-mark-mode

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)

;; (use-package flycheck-color-mode-line
;;   :ensure t)
;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; (use-package flycheck-pos-tip
;;   :ensure t)
;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))
