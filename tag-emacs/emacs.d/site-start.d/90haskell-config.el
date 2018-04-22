;; Haskell-mode
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (unbind-key "C-c C-s" haskell-mode-map)
  :bind
  ("C-c C-." . haskell-mode-format-imports)
  ("C-c C-s" . haskell-mode-stylish-buffer))

;; Disable haskell-mode indentation.
(haskell-indentation-mode -1)

;; Hindent
(use-package hindent
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; Intero
(use-package intero
 :config
 (add-hook 'haskell-mode-hook 'intero-mode))

;; Fix ghci bug: https://github.com/haskell/haskell-mode/issues/1455
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)

;; rainbow-delimeters for haskell-mode
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
