;; General miscellaneous tweaks before actual config
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ido-mode t)
(setq x-underline-at-descent-line t)
(package-initialize)
(package-refresh-contents)

(setq inhibit-startup-message t)

(setq
 package-archives
 '(("melpa" . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("elpa" . "https://elpa.gnu.org/packages/")))

;; Config
;; Modal editing. Papa bless
(use-package boon
  :ensure t
  :init (use-package boon-qwerty)
  :config (boon-mode))

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  ;; Load dark theme in the evening, light theme in the day
  (let ((picked-theme (if (or (> (caddr (decode-time)) 18)
			      (< (caddr (decode-time)) 7))
                          'solarized-dark
                          'solarized-light)))
    (load-theme picked-theme t)))

;; Minibuffer autocompletion
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

;; More efficient backspace
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char))
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))

;; Git porcelain. Not much going on here
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :pin melpa-stable
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))

;; Mah modeline
(use-package telephone-line
  :ensure t
  :init
  ;; Boon Mode Segment
  (telephone-line-defsegment* telephone-line-boon-tag-segment ()
    "Displays current boon mode."
    (when (bound-and-true-p boon-mode)
      (if (string= (boon-state-string) "INS")
	  "INSERT"
          "COMMAND")))

  ;; Display minor modes without boon
  (telephone-line-defsegment* telephone-line-custom-minor-mode-segment ()
    (telephone-line-raw (assq-delete-all 'boon-local-mode minor-mode-alist) t)) 

  (setq telephone-line-primary-right-separator 'telephone-line-abs-right)
  (setq telephone-line-secondary-right-separator 'telephone-line-abs-hollow-right)
  (setq telephone-line-height 18)
  (setq telephone-line-lhs
	'((accent . (telephone-line-boon-tag-segment
                     telephone-line-custom-minor-mode-segment))
          (nil    . (telephone-line-vc-segment
		     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
	'((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  :config (telephone-line-mode 1))

;; Documents and Latex
(use-package latex-preview-pane
  :ensure t)

;; Language stuff
;; I will defer actual language configurations for when I get a hold of my setup again.
;; For now, just most used packages

;; Ubiquitous
(use-package company
  :ensure t)

;; Langauge specific packages
;; Haskell
(use-package haskell-mode
  :ensure t)

;; JS
(use-package rjsx-mode ;; JS and JSX syntax highligting
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Misc.
(setq active-user-alias "Kareithi")
