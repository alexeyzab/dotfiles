;; Who am I? Where am I?
(setq user-full-name "Alexey Zabelin"
      user-mail-address "hello@alexeyzabelin.com"
      calendar-location-name "New York, NY")

;; Rename file
(defun az/rename-file (new-name)
  (interactive "New name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
            (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

;; I don't usually use the menu or scroll bar, and they take up useful space.
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

;; The default frame title isn't useful. This binds it to the name of the current
;; project:
(setq frame-title-format '((:eval (projectile-project-name))))

;; Use nicer lambdas.
(global-prettify-symbols-mode t)

;; Theme setup
;; Doom theme.
(use-package doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Solaire-mode.
(use-package solaire-mode)
(solaire-mode-swap-bg)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

;; =sensible-defaults= replaces the audible bell with a visual one, but I really
;; don't even want that (and my Emacs/Mac pair renders it poorly). This disables
;; the bell altogether.
(setq ring-bell-function 'ignore)

;; The standard =text-scale-= functions just resize the text in the current buffer;
;; I'd generally like to resize the text in /every/ buffer, and I usually want to
;; change the size of the modeline, too (this is especially helpful when
;; presenting). These functions and bindings let me resize everything all together!

;; Note that this overrides the default font-related keybindings from
;; =sensible-defaults=.
(setq az/default-font "Fantasque Sans Mono")
(setq az/default-font-size 12)
(setq az/current-font-size az/default-font-size)

(setq az/font-change-increment 1.1)

(defun az/font-code ()
  "Return a string representing the current font (like \"Fantasque Sans Mono-13\")."
  (concat az/default-font "-" (number-to-string az/current-font-size)))

(defun az/set-font-size ()
  "Set the font to `az/default-font' at `az/current-font-size'.
  Set that for the current frame, and also make it the default for
  other, future frames."
  (let ((font-code (az/font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun az/reset-font-size ()
  "Change font size back to `az/default-font-size'."
  (interactive)
  (setq az/current-font-size az/default-font-size)
  (az/set-font-size))

(defun az/increase-font-size ()
  "Increase current font size by a factor of `az/font-change-increment'."
  (interactive)
  (setq az/current-font-size
        (ceiling (* az/current-font-size az/font-change-increment)))
  (az/set-font-size))

(defun az/decrease-font-size ()
  "Decrease current font size by a factor of `az/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq az/current-font-size
        (max 1
             (floor (/ az/current-font-size az/font-change-increment))))
  (az/set-font-size))

(define-key global-map (kbd "C-)") 'az/reset-font-size)
(define-key global-map (kbd "C-+") 'az/increase-font-size)
(define-key global-map (kbd "C-=") 'az/increase-font-size)
(define-key global-map (kbd "C-_") 'az/decrease-font-size)
(define-key global-map (kbd "C--") 'az/decrease-font-size)

(az/reset-font-size)

;; =global-hl-line-mode= softly highlights the background color of the line
;; containing point. It makes it a bit easier to find point, and it's useful when
;; pairing or presenting code.
(when window-system
  (global-hl-line-mode))

;; Flycheck.
(use-package flycheck)
(use-package flycheck-package)

;; I'd rather have only a few necessary mode identifiers on my modeline. This
;; either hides or "renames" a variety of major or minor modes using the =diminish=
;; package.
(use-package diminish)
(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yard-mode 'yard-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'rust-mode-hook "rs")

;; Show line numbers everywhere
(setq global-linum-mode t)

;; Magit settings
;; The default behavior of =magit= is to ask before pushing. I haven't had any
;; problems with accidentally pushing, so I'd rather not confirm that every time.
(setq magit-push-always-verify nil)

;; Enable spellchecking when writing commit messages:
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

;; Try out packages without installing them
(use-package try)

;; Show keybingings for commands
(use-package which-key
  :config
  (which-key-mode))

;; Swiper setup
(use-package counsel)

(use-package swiper
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  :bind
  ("\C-s" . counsel-grep-or-swiper)
  ("C-c C-r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-rg)
  ("C-x l" . counsel-locate))

;; Dumb-jump
(use-package dumb-jump)

;; Auto-complete setup with company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Expand region binding
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Simpleclip
(use-package simpleclip
  :config
  (simpleclip-mode 1))

;; Magit for Git
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

;; Buffer Selection
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Switch to ZSH
(setq explicit-shell-file-name "/usr/local/bin/zsh")

;; Shell-pop
(use-package shell-pop
  :bind
  ("C-t" . shell-pop-universal-key))

;; Eyebrowse
(use-package eyebrowse)
(eyebrowse-mode t)

;; Indent buffer
(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

;; Linewrapping
(global-visual-line-mode 0)
(setq-default fill-column 99999)

;; Sane copy-paste
(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))
            ))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
  (cond
   ((and (display-graphic-p) x-select-enable-clipboard)
    (insert (x-get-selection 'CLIPBOARD)))
   (t (shell-command
       (cond
        (*cygwin* "getclip")
        (*is-a-mac* "pbpaste")
        (t "xsel -ob"))
       1))
   ))

(global-set-key (kbd "C-x C-y") 'copy-to-x-clipboard)
(global-set-key (kbd "C-x C-p") 'paste-from-x-clipboard)

;; LSP
;; (add-to-list 'load-path "/Users/alexeyzab/code/lsp-mode")
;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-flycheck))
;; (require 'lsp-mode)
;; (add-hook 'haskell-mode #'lsp-mode)
;; (add-hook 'rust-mode #'lsp-mode)

;; undo-tree
(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; Eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; Create directory if it doesn't exist
;; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
;;   "Create parent directory if not exists while visiting file."
;;   (unless (file-exists-p filename)
;;     (let ((dir (file-name-directory filename)))
;;       (unless (file-exists-p dir)
;; 	(make-directory dir)))))

;; Make sure $PATH is available to Emacs.
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Customzie exec-path
(setq exec-path (append exec-path '("~/.cargo/bin")))

;; Make sure $PATH is available to Emacs
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; This binds C-c e to quickly open my
;; Emacs configuration file.
(defun az/visit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/site-start.d/00general-config.el"))

(global-set-key (kbd "C-c e") 'az/visit-emacs-config)

;; Assume that I always want to kill the current buffer when hitting =C-x k=.
(global-set-key (kbd "C-x k") 'az/kill-current-buffer)

;; When splitting a window, I invariably want to switch to the new window. This
;; makes that automatic.

(defun az/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun az/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'az/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'az/split-window-right-and-switch)

;; Projectile
(use-package projectile)
(use-package counsel-projectile)
(counsel-projectile-on)

;; Use ripgrep for projectile.
(use-package ripgrep)
(use-package projectile-ripgrep)

;; I'd like to /always/ be able to recursively fuzzy-search for files, not just
;; when I'm in a Projecile-defined project. This uses the current directory as a
;; project root (if I'm not in a "real" project).
(setq projectile-require-project-root nil)

;; Use projectile everywhere
(projectile-global-mode)

;; Enable [[https://github.com/hrs/engine-mode][engine-mode]] and define a few useful engines.
(use-package engine-mode)

(defengine stackage
  "https://www.stackage.org/lts-9.0/hoogle?q=%s"
  :keybinding "s")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "o")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine youtube
  "https://www.youtube.com/results?search_query=%s")

(engine-mode t)

;; Make sure backups go into a separate dir, don't clobber symlinks, and use versioned backups
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Autocomplete cycling with TAB
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(setq company-require-match 'never)

;; Multiple-cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Better than zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

;; Look for executables in =/usr/local/bin=.
(defun az/append-to-path (path)
  "Add a path both to the $PATH variable and to Emacs' exec-path."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))
(az/append-to-path "/usr/local/bin")

;; Use paredit
(use-package paredit)

;; Kill current-buffer
  (defun az/kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'az/kill-current-buffer)

;; ace-window
(use-package ace-window
  :bind ("M-p" . ace-window))

;; avy
(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line))
(avy-setup-default)

;; PDF reading
(use-package pdf-tools)

;; Yasnippet
(use-package yasnippet)
(use-package yasnippet-snippets)

;; Editor-config
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Helpful
(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; Nix-mode
(use-package nix-mode)

;; git-timemachine
(use-package git-timemachine)

;; Stop annoying tags-related pop-ups
(setq tags-add-tables nil)

;; Misc keybindigns
(global-set-key (kbd "C-c g") 'align-regexp)
(global-set-key (kbd "C-c C-w RET") 'whitespace-cleanup)
(global-set-key (kbd "C-c b") 'comment-box)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-c TAB") 'indent-buffer)
(global-set-key (kbd "M-q") 'indent-region)
(global-set-key (kbd "C-x p") 'paredit-splice-sexp)
(global-set-key (kbd "C-c r") 'replace-string)
