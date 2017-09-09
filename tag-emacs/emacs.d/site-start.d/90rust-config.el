;; rust-mode
(use-package rust-mode)

;; toml-mode
(use-package toml-mode)

;; racer
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; completion
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; minor cargo-mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; rustfmt
(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>")
						    #'rust-format-buffer)))

;; flycheck
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rainbow-delimeters for haskell-mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)

;; lsp-rust
(use-package lsp-rust
  :config
  (add-to-list 'load-path "/Users/alexeyzab/code/lsp-rust"))
