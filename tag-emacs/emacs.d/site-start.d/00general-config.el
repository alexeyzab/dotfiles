;; Expand region binding
(eval-after-load 'expand-region '(require 'expand-region))
(global-set-key (kbd "C-=") 'er/expand-region)

;; Ace bindings
(require 'ace-isearch)
(global-ace-isearch-mode +1)

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)
(global-set-key (kbd "C-c c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-line-mode)

(custom-set-variables
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-function 'avy-goto-char)
 '(ace-isearch-use-jump 'printing-char))

(define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch)

;; Simpleclip
(eval-after-load 'simpleclip '(require 'simpleclip))
(require 'simpleclip)
(simpleclip-mode 1)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))
(global-set-key "\t" 'company-complete-common)
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-interactive-mode-hook 'company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
(setq company-tooltip-limit 20)
(setq company-idle-dealy 0)
(setq company-echo-delay 0)

;; Magit for Git
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Buffer Selection
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Shel-pop
(require 'shell-pop)
(global-set-key (kbd "C-t") 'shell-pop-universal-key)

;; Helm
(autoload 'dired "~/.emacs.d/site-start.d/emacs-async/dired-async.el" nil t)
(dired-async-mode 1)

(require 'helm-config)

;; Eyebrowse
(eyebrowse-mode t)

;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Ido mode
(require 'ido)
(ido-mode t)

;; Misc keybindigns
(global-set-key (kbd "C-c g") 'align-regexp)
