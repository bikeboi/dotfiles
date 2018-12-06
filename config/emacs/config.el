(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

(use-package evil
  :ensure t
  :config (evil-mode 1))

(setq evil-move-beyond-eol t)

(use-package spaceline
  :ensure t
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-toggle-global-on) ;; Set EVERYTHING on
  (spaceline-spacemacs-theme))

(use-package smart-hungry-delete
  :ensure t
  :bind ("<backspace>" . smart-hungry-delete-backward-char)
  :defer nil ;; So we can do what's below
  :config (smart-hungry-delete-add-default-hooks))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :pin melpa-stable
  :config (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commmands)
	 ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package all-the-icons :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-molokai t)
  (doom-themes-org-config))

(set-face-attribute 'default nil
  :family "Source Code Pro"
  :height 100
  :width 'condensed
  :foundry "SRC" ;; Not sure if this is necessary
  :weight 'semi-bold)

;; Little tweaks
(setq org-src-tabs-natively t)
(setq org-startup-truncated nil)

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package company :ensure t)

(use-package haskell-mode :ensure t) ;; Haskell
(use-package rjsx-mode :ensure t :mode "\\.js\\'")
(use-package yaml-mode :ensure t)
(use-package elpy :ensure t)
(use-package cider :ensure t)

(use-package latex-preview-pane :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(ido-mode t)

(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)
