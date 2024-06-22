(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Use ibuffer in place of the default list-buffers command
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)


;; (setq default-directory "C:/Users/USER/code/")
(tool-bar-mode -1)
(toggle-frame-maximized)
(global-display-line-numbers-mode 1)
(global-hl-line-mode t)
(setq inhibit-startup-message t)


;; Monokai Customization
(load-theme 'monokai t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :foundry "outline" :slant normal :weight regular :height 158 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(expand-region markdown-mode monokai-theme)))
