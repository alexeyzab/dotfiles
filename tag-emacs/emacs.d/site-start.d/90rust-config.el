;; rust-mode
;; (use-package rust-mode)

;; rustic
(use-package rustic
  :config
  (setq rustic-rls-pkg 'elgot)
  (autoload 'rustic-mode "rustic-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

;; rustfmt
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; toml-mode
(use-package toml-mode
  :defer t)

;; racer
(use-package racer
  :defer t
  :config
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "/home/alexeyzab/code/rust/src") ;; Rust source code PATH
  :bind
  ("M-." . racer-find-definition))
(use-package company-racer
  :defer t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; cargo
(use-package cargo
  :defer t
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

;; flycheck
(use-package flycheck-rust
  :defer t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rainbow-delimeters for rust-mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
