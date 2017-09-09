;; Haskell-mode
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  :bind
  ("C-c C-." . haskell-mode-format-imports))

;; Disable haskell-mode indentation.
(haskell-indentation-mode -1)

;; Hindent
(use-package hindent
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; Intero
;; (package-install 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; Fix ghci bug: https://github.com/haskell/haskell-mode/issues/1455
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))

;; lsp-haskell
(use-package lsp-haskell
  :config
  (add-to-list 'load-path "/Users/alexeyzab/code/lsp-haskell"))

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)

;; rainbow-delimeters for haskell-mode
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
