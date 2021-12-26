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

;; LSP
;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook 'eglot-ensure)
;;   ;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
;;   (add-hook 'eglot--managed-mode-hook 'sanityinc/eglot-prefer-flycheck)
;;   (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
;;   (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))

;; (defvar-local flycheck-eglot-current-errors nil)

;; (defun flycheck-eglot-report-fn (diags &rest _)
;;   (setq flycheck-eglot-current-errors
;; 	(mapcar (lambda (diag)
;; 		  (save-excursion
;; 		    (goto-char (flymake--diag-beg diag))
;; 		    (flycheck-error-new-at (line-number-at-pos)
;; 					   (1+ (- (point) (line-beginning-position)))
;; 					   (pcase (flymake--diag-type diag)
;; 					     ('eglot-error 'error)
;; 					     ('eglot-warning 'warning)
;; 					     ('eglot-note 'info)
;; 					     (_ (error "Unknown diag type, %S" diag)))
;; 					   (flymake--diag-text diag)
;; 					   :checker 'eglot)))
;; 		diags))
;;   (flycheck-buffer))

;; (defun flycheck-eglot--start (checker callback)
;;   (funcall callback 'finished flycheck-eglot-current-errors))

;; (defun flycheck-eglot--available-p ()
;;   (bound-and-true-p eglot--managed-mode))

;; (flycheck-define-generic-checker 'eglot
;;   "Report `eglot' diagnostics using `flycheck'."
;;   :start #'flycheck-eglot--start
;;   :predicate #'flycheck-eglot--available-p
;;   :modes '(prog-mode text-mode))

;; (push 'eglot flycheck-checkers)

;; (defun sanityinc/eglot-prefer-flycheck ()
;;   (when eglot--managed-mode
;;     (flycheck-add-mode 'eglot major-mode)
;;     (flycheck-select-checker 'eglot)
;;     (flycheck-mode)
;;     (flymake-mode -1)
;;     (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))

;; (use-package lsp-haskell
;;  :ensure t
;;  :config
;;  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  ;;(setq lsp-log-io t)
;; )

;; (require 'lsp)
;; (require 'lsp-haskell)
;; (add-hook 'haskell-mode-hook #'lsp)

(setq-default flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc))

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (haskell-mode . lsp)
;;          (haskell-literate-mode-hook . lsp)
;;          ;; if you want which-key integration
;;          ;; (lsp-mode . lsp-enable-which-key-integration)
;; 	 )
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; (load "persistent-mode")
