;; Haskell-mode
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(global-set-key (kbd "C-c C-.") 'haskell-mode-format-imports)

;; Hindent
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
(define-key haskell-mode-map (kbd "C-c C-f") 'hindent-reformat-buffer)

;; Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)
