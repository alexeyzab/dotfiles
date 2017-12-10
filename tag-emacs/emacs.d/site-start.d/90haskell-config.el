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

;; Fix ghci bug: https://github.com/haskell/haskell-mode/issues/1455
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))

;; Dante
;; (use-package dante
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))
;;   (add-hook 'dante-mode-hook
;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                 '(warning . haskell-hlint))))

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)

;; rainbow-delimeters for haskell-mode
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
