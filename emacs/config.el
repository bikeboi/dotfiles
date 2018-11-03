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
  (let ((picked-theme (if (> (caddr (decode-time)) 18) 'solarized-dark 'solarized-light)))
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
  :pin melpa-stable)

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

;; Language stuff

;; Haskell
(use-package haskell-mode
  :ensure t)

;; JS
(use-package rsjx-mode
  :ensure t)
