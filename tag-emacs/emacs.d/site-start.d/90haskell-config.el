;; Haskell-mode
(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (unbind-key "C-c C-s" haskell-mode-map)
  :bind
  ("C-c C-." . haskell-mode-format-imports))
;; ("C-c C-s" . haskell-mode-stylish-buffer))

(autoload 'hhp-init "hhp" nil t)
(autoload 'hhp-debug "hhp" nil t)
(add-hook 'haskell-mode-hook (lambda () (hhp-init)))

;; Disable haskell-mode indentation.
(haskell-indentation-mode -1)

;; Hindent
(use-package hindent
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; Intero
;; (use-package intero
;;  :config
;;  (add-hook 'haskell-mode-hook 'intero-mode))

;; Autocompletion
;; (use-package company-ghc
;;   :config
;;   (add-to-list 'company-backends 'company-ghc))
;; (use-package ghc-mod
;;   :config
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

;; Fix ghci bug: https://github.com/haskell/haskell-mode/issues/1455
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))

;; Misc
(add-hook 'haskell-mode-hook 'subword-mode)

;; rainbow-delimeters for haskell-mode
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

;; structured-haskell-mode
;; (use-package shm)

;; autocompletion
(use-package auto-complete
  :defer t)
(use-package ac-haskell-process
  :defer t
  :config
  (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
  (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'haskell-interactive-mode)))

;; (setq haskell-process-args-ghci
;;       '("-ferror-spans" "-fshow-loaded-modules"))

;; (setq haskell-process-args-cabal-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

;; (setq haskell-process-args-stack-ghci
;;       '("--stack-yaml=/home/alexeyzab/code/work/lanehoney/stack-backend.yaml"
;;         "--ghci-options=-ferror-spans -fshow-loaded-modules"
;; 	"--no-build" "--no-load"))

;; (setq haskell-process-args-cabal-new-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
