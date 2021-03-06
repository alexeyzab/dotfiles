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
;; (use-package doom-themes)

;; Global settings (defaults)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-one t)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)
(setq nord-uniform-mode-lines t)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;; Solaire-mode.
(use-package solaire-mode
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
  (solaire-mode-swap-bg))

;; This disables the visual bell.
(setq ring-bell-function 'ignore)

;; The standard =text-scale-= functions just resize the text in the current buffer;
;; I'd generally like to resize the text in /every/ buffer, and I usually want to
;; change the size of the modeline, too (this is especially helpful when
;; presenting). These functions and bindings let me resize everything all together!

;; Note that this overrides the default font-related keybindings from
;; =sensible-defaults=.
(setq az/default-font "Iosevka")
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
(use-package flycheck
  :defer t)
(use-package flycheck-package
  :defer t)

;; I'd rather have only a few necessary mode identifiers on my modeline. This
;; either hides or "renames" a variety of major or minor modes using the =diminish=
;; package.
(use-package diminish
  :defer t)
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
(setq linum-mode t)

;; Magit settings
;; The default behavior of =magit= is to ask before pushing. I haven't had any
;; problems with accidentally pushing, so I'd rather not confirm that every time.
(setq magit-push-always-verify nil)

;; Enable spellchecking when writing commit messages:
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

;; Try out packages without installing them
(use-package try
  :defer t)

;; Show keybingings for commands
(use-package which-key
  :config
  (which-key-mode))

;; Swiper setup
(use-package counsel
  :defer t)

(use-package swiper
  :defer t
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

;; Auto-complete setup with company
(use-package company
  ;; :defer t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
  (add-to-list 'company-backends 'company-ghc)))

;; Expand region binding
(use-package expand-region
  :defer t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Simpleclip
(use-package simpleclip
  :defer t
  :config
  (simpleclip-mode 1))

;; Magit for Git
(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

;; Buffer Selection
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Switch to ZSH
(setq explicit-shell-file-name "/run/current-system/sw/bin/zsh")

;; Shell-pop
(use-package shell-pop
  :defer t
  :bind
  ("C-t" . shell-pop-universal-key))

;; Eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t))

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

(global-set-key (kbd "C-x M-w") 'copy-to-x-clipboard)
(global-set-key (kbd "C-x C-y") 'paste-from-x-clipboard)

;; undo-tree
(use-package undo-tree
  :defer t
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

;; node stuff
(setq exec-path (append exec-path '("~/.config/nvm/versions/node/v8.10.0/bin/")))

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
(use-package projectile
  :defer t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Running C-u s-p f will invalidate the cache prior to prompting you for a file to jump to.
  (setq projectile-enable-caching t))
(use-package counsel-projectile)
(counsel-projectile-mode)

;; Use ripgrep for projectile.
(use-package ripgrep
  :defer t)
(use-package projectile-ripgrep
  :defer t)

;; I'd like to /always/ be able to recursively fuzzy-search for files, not just
;; when I'm in a Projecile-defined project. This uses the current directory as a
;; project root (if I'm not in a "real" project).
(setq projectile-require-project-root nil)

;; Use projectile everywhere
(projectile-global-mode)

;; Enable [[https://github.com/hrs/engine-mode][engine-mode]] and define a few useful engines.
(use-package engine-mode
  :defer t)

(defengine stackage
  "https://www.stackage.org/lts-11.5/hoogle?q=%s"
  :keybinding "s")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "o")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
  :keybinding "d")

(defengine youtube
  "https://www.youtube.com/results?search_query=%s"
  :keybinding "y")

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
(use-package multiple-cursors
  :defer t)
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
(az/append-to-path "~/.local/bin")

;; Use paredit
(use-package paredit
  :defer t)

;; Kill current-buffer
  (defun az/kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'az/kill-current-buffer)

;; ace-window
(use-package ace-window
  :defer t
  :bind ("M-p" . ace-window))

;; avy
(use-package avy
  :defer t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line))
(avy-setup-default)

;; PDF reading
(use-package pdf-tools
  :defer t)

;; Yasnippet
(use-package yasnippet
  :defer t)
(use-package yasnippet-snippets
  :defer t)

;; Editor-config
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

;; git-timemachine
(use-package git-timemachine
  :defer t)

;; Stop annoying tags-related pop-ups
(setq tags-add-tables nil)

;; Render markdown
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Turn off electric-indent-mode
;; (electric-indent-mode -1)

;; Rainbow delimeters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Misc keybindigns
(global-set-key (kbd "C-c g") 'align-regexp)
(global-set-key (kbd "C-c C-w RET") 'whitespace-cleanup)
(global-set-key (kbd "C-c b") 'comment-box)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-c TAB") 'indent-buffer)
(global-set-key (kbd "M-q") 'indent-region)
(global-set-key (kbd "C-x p") 'paredit-splice-sexp)
(global-set-key (kbd "C-c r") 'replace-string)

;; Have to unset these otherwise it doesn't work
(global-unset-key "\C-?")
(global-unset-key "\M-h")
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "DEL") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Terminal emacs color fix
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

;; Reverse words
(defun reverse-words (beg end)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
    (reverse
     (split-string
      (delete-and-extract-region beg end) "\\b"))))

;; Weather from wttr.in
(use-package wttrin
  :defer t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("New York"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US")))

;; zoom
(use-package zoom
  :defer t
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (custom-set-variables
   '(zoom-mode t)))

;; restclient
(use-package restclient
  :defer t)

;; magithub
(use-package magithub
  :defer t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/code"))

;; ws-butler
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; browse-kill-ring
(use-package browse-kill-ring
  :config
  (global-set-key "\M-y" 'browse-kill-ring))

;; docker.el
(use-package docker
  :defer t
  :bind ("C-c d" . docker))

;; twittering-mode
(use-package twittering-mode
  :defer t
  :config
  (setq twittering-icon-mode t))

;; sql-mode
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; eshell completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; actionable urls
;; (use-package goto-addr
;;   :hook ((compilation-mode . goto-address-mode)
;;          (prog-mode . goto-address-prog-mode)
;;          (eshell-mode . goto-address-mode)
;;          (shell-mode . goto-address-mode))
;;   :bind (:map goto-address-highlight-keymap
;;               ("<RET>" . goto-address-at-point)
;;               ("M-<RET>" . newline))
;;   :commands (goto-address-prog-mode
;;              goto-address-mode))

;; Eglot
(use-package eglot)

(global-flycheck-mode)

;; Yaml
(use-package yaml-mode
  :defer t)

;; backward-kill-word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
;;; 00general-config.el ends here
