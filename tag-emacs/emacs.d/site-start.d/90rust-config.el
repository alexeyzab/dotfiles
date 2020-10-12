;; rustic
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))

;; rustfmt
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; toml-mode
(use-package toml-mode)

;; rainbow-delimeters for rust-mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
