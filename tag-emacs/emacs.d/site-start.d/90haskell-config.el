;; Haskell-mode
(use-package haskell-mode
  :config
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

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)

;; rainbow-delimeters for haskell-mode
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

;; flycheck
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

;; autocompletion
(use-package auto-complete)
(use-package ac-haskell-process
  :config
  (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
  (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'haskell-interactive-mode)))

(setq-default flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc))

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))
