;; rust-mode
(use-package rust-mode)

;; toml-mode
(use-package toml-mode)

;; racer
(use-package racer)
(use-package company-racer)
(with-eval-after-load 'company
      (add-to-list 'company-backends 'company-racer))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; cargo
(use-package cargo
  :bind
  ("C-c C-c C-b" . cargo-process-build)
  ("C-c C-c C-r" . cargo-process-run)
  ("C-c C-c C-t" . cargo-process-test)
  ("C-c C-c C-e" . cargo-process-bench)
  ("C-c C-c C-l" . cargo-process-clean)
  ("C-c C-c C-d" . cargo-process-doc)
  ("C-c C-c C-v" . cargo-process-doc-open)
  ("C-c C-c C-n" . cargo-process-new)
  ("C-c C-c C-i" . cargo-process-init)
  ("C-c C-c C-x" . cargo-process-run-example)
  ("C-c C-c C-s" . cargo-process-search)
  ("C-c C-c C-u" . cargo-process-update)
  ("C-c C-c C-c" . cargo-process-repeat)
  ("C-c C-c C-f" . cargo-process-current-test)
  ("C-c C-c C-o" . cargo-process-current-file-tests)
  ("C-c C-c C-m" . cargo-process-fmt)
  ("C-c C-c C-k" . cargo-process-check)
  ("C-c C-c C-S-k" . cargo-process-clippy)
)

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
(use-package rustfmt)
(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>")
						    #'rust-format-buffer)))

;; flycheck
(use-package flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rainbow-delimeters for haskell-mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)

;; lsp-rust
(use-package lsp-rust
  :config
  (add-to-list 'load-path "/Users/alexeyzab/code/lsp-rust"))
