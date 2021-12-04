(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(setq user-full-name "Alexey Zabelin"
      user-mail-address "hello@alexeyzabelin.com"
      calendar-latitude 37.77
      calendar-longitude -122.43
      calendar-location-name "San Francisco, CA")

(require 'netrc)

(setq netrc-file "~/.netrc")

(defun netrc-username (machine)
  "Helper function to extract a username from my netrc."
  (car (netrc-credentials machine)))

(defun netrc-password (machine)
  "Helper function to extract a password from my netrc."
  (cadr (netrc-credentials machine)))

(defun az/rename-file (new-name)
  (interactive "FNew name: ")
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

(defun az/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun az/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun az/visit-last-migration ()
  "Open the most recent Rails migration. Relies on projectile."
  (interactive)
  (let ((migrations
         (directory-files
          (expand-file-name "db/migrate" (projectile-project-root)) t)))
    (find-file (car (last migrations)))))

(defun az/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun az/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun az/region-or-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(defun az/append-to-path (path)
  "Add a path both to the $PATH variable and to Emacs' exec-path."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(set-window-scroll-bars (minibuffer-window) nil nil)

(setq frame-title-format '((:eval (projectile-project-name))))

(global-prettify-symbols-mode t)

(use-package nord-theme
  :config
  (load-theme 'nord t)

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun az/apply-theme ()
  (interactive)
  (load-theme 'nord t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (az/apply-theme))))
  (az/apply-theme))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(setq az/default-font "Iosevka")
(setq az/default-font-size 16)
(setq az/current-font-size az/default-font-size)

(setq az/font-change-increment 1.1)

(defun az/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
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

(global-hl-line-mode)

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

(use-package dumb-jump
  :config
  (define-key global-map (kbd "M-.") 'dumb-jump-go)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-selector 'ivy))

(use-package flycheck)

(use-package magit
  :bind
  ("C-x g" . magit-status)

  :config
  (use-package transient)
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50))

(use-package ghub)
(use-package forge)

(use-package projectile
  :bind
  ("C-c v" . 'projectile-ag)
  ("C-c p" . 'projectile-command-map)

  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil))

(use-package undo-tree)
(global-undo-tree-mode)

(setq-default tab-width 2)

(use-package subword
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package haskell-mode)

(add-hook 'haskell-mode-hook
          (lambda ()
            (haskell-doc-mode)
            (turn-on-haskell-indent)))

(az/append-to-path "~/.cabal/bin")

;; rustic
(use-package rustic
  :config
  (setq rustic-rls-pkg 'eglot)
  (autoload 'rustic-mode "rustic-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

;; rustfmt
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; toml-mode
(use-package toml-mode)

;; racer
(use-package racer
  :config
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "/Users/alexeyzab/code/rust/src")) ;; Rust source code PATH
;;   :bind
;;   ("M-." . racer-find-definition))
(use-package company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

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
  ("C-c C-c C-S-k" . cargo-process-clippy))

;; completion
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package rust-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; minor cargo-mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; flycheck
(use-package flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rainbow-delimeters for rust-mode
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)

(setq js-indent-level 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

(use-package prettier-js)

(use-package paredit)

(use-package rainbow-delimiters)

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package flycheck-package)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(use-package web-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (rainbow-mode)
            (rspec-mode)
            (prettier-js-mode)
            (setq web-mode-markup-indent-offset 2)))

(az/add-auto-mode
 'web-mode
 "\\.html$"
 "\\.ts$"
 "\\.tsx$")

(use-package yaml-mode)

(use-package multi-term)
(global-set-key (kbd "C-c t") 'multi-term)

(setq multi-term-program-switches "--login")

(defun az/term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)
            (define-key term-raw-map (kbd "C-y") 'az/term-paste)
            (define-key term-raw-map (kbd "<mouse-2>") 'az/term-paste)
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (setq yas-dont-activate t)))

(use-package org)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-directory "~/Dropbox/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun az/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file))

(defun az/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'az/mark-done-and-archive)

(setq org-log-done 'time)

(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file "~/documents/notes/blog-ideas.org")
         "* %?\n")

        ("e" "Email" entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n\n%a\n\n")

        ("f" "Finished book"
         table-line (file "~/documents/notes/books-read.org")
         "| %^{Title} | %^{Author} | %u |")

        ("r" "Reading"
         checkitem
         (file (org-file-path "to-read.org")))

        ("s" "Subscribe to an RSS feed"
         plain
         (file "~/Dropbox/org/feeds.org")
         "%^{Feed URL} \"~%^{Feed name}\"")

        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun az/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (az/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'az/open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(require 'ox-md)
(require 'ox-beamer)

(use-package gnuplot)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)

(use-package graphviz-dot-mode)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

(use-package instapaper)
(require 'instapaper)

(setq instapaper-username (netrc-username "instapaper.com")
      instapaper-password (netrc-password "instapaper.com"))

(defvar az/jekyll-posts-directory "~/documents/blog/_posts/")
(defvar az/jekyll-post-extension ".md")

(defun az/replace-whitespace-with-hyphens (s)
  (replace-regexp-in-string " " "-" s))

(defun az/replace-nonalphanumeric-with-whitespace (s)
  (replace-regexp-in-string "[^A-Za-z0-9 ]" " " s))

(defun az/replace-unusual-characters (title)
  "Remove quotes, downcase everything, and replace characters
that aren't alphanumeric with hyphens."
  (az/replace-whitespace-with-hyphens
   (s-trim
    (downcase
     (az/replace-nonalphanumeric-with-whitespace
      (az/remove-quotes title))))))

(defun az/slug-for (title)
  "Given a blog post title, return a convenient URL slug.
   Downcase letters and remove special characters."
  (let ((slug (az/replace-unusual-characters title)))
    (while (string-match "--" slug)
      (setq slug (replace-regexp-in-string "--" "-" slug)))
    slug))

(defun az/timestamped-slug-for (title)
 "Turn a string into a slug with a timestamp and title."
  (concat (format-time-string "%Y-%m-%d")
          "-"
          (az/slug-for title)))

(defun az/jekyll-yaml-template (title)
  "Return the YAML header information appropriate for a blog
   post. Include the title, the current date, the post layout,
   and an empty list of tags."
  (concat
   "---\n"
   "title: " title "\n"
   "date: " (format-time-string "%Y-%m-%d") "\n"
   "layout: post\n"
   "# pdf_file: " (az/slug-for title) ".pdf\n"
   "tags: []\n"
   "---\n\n"))

(defun az/new-blog-post (title)
  "Create a new blog post in Jekyll."
  (interactive "sPost title: ")
  (let ((post (concat az/jekyll-posts-directory
                      (az/timestamped-slug-for title)
                      az/jekyll-post-extension)))
    (if (file-exists-p post)
        (find-file post)
      (find-file post)
      (insert (az/jekyll-yaml-template title)))))

(defun az/existing-blog-tags ()
  "Return a list of all the tags currently used in my blog."
  (split-string (shell-command-to-string "cd ~/documents/blog && rake tags")))

(defun az/insert-blog-tag ()
  "Select one of the current tags and insert it at point."
  (interactive)
  (insert
   (ivy-completing-read "Insert tag at point: " (az/existing-blog-tags))))

(setq az/checklist-script "~/bin/daily-checklist")

(defun az/today-checklist-filename ()
  "The filename of today's checklist."
  (concat "/home/az/documents/checklists/daily-checklist-" (format-time-string "%Y-%m-%d") ".org"))

(defun az/today ()
  "Take a look at today's checklist."
  (interactive)
  (let ((filename (az/today-checklist-filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (progn
        (shell-command (concat az/checklist-script " > " filename))
        (find-file filename)))))

(defun az/dashboard ()
  (interactive)
  (delete-other-windows)
  (az/today)
  (split-window-right)
  (az/open-index-file))

(global-set-key (kbd "C-c d") 'az/dashboard)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/.mail")

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "personal"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "hello@harryrschwartz.com")
                  (mu4e-trash-folder . "/personal/archive")
                  (mu4e-refile-folder . "/personal/archive")
                  (mu4e-sent-folder . "/personal/sent")
                  (mu4e-drafts-folder . "/personal/drafts")))))

(setq mu4e-get-mail-command "killall --quiet mbsync; mbsync inboxes")

(define-key mu4e-headers-mode-map (kbd "o") 'mu4e-update-mail-and-index)

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-update-interval 300)

(defun az/visit-inbox ()
  (interactive)
  (delete-other-windows)
  (mu4e~headers-jump-to-maildir "/personal/inbox"))

(global-set-key (kbd "C-c m") 'az/visit-inbox)

(setq mu4e-maildir-shortcuts '(("/personal/inbox" . ?i)
                               ("/personal/sent" . ?s)))

(setq mu4e-confirm-quit nil)

(setq mu4e-compose-context-policy 'pick-first)

(setq mail-user-agent 'mu4e-user-agent)

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

(setq message-kill-buffer-on-exit t)

(setq mu4e-view-show-addresses t)

(setq mu4e-attachment-dir "~/downloads")

(define-key mu4e-view-mode-map (kbd "C-c C-o") 'mu4e~view-browse-url-from-binding)

(add-to-list 'mu4e-view-actions '("html in browser" . mu4e-action-view-in-browser) t)

(defun az/encrypt-responses ()
  (let ((msg mu4e-compose-parent-message))
    (when msg
      (when (member 'encrypted (mu4e-message-field msg :flags))
        (mml-secure-message-encrypt-pgpmime)))))

(add-hook 'mu4e-compose-mode-hook 'az/encrypt-responses)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
(setq sendmail-program "msmtp")

(require 'org-mu4e)

(setq org-mu4e-link-query-in-headers-mode nil)

(use-package bbdb)
(require 'bbdb-mu4e)

(setq mu4e-compose-complete-addresses nil)

(use-package elfeed)

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/feeds.org"))
  :bind (:map elfeed-search-mode-map
              ("A" . az/elfeed-show-all)
              ("E" . az/elfeed-show-emacs)
              ("C" . az/elfeed-show-economics)
              ("F" . az/elfeed-show-fitness)
              ("q" . az/elfeed-save-db-and-bury)))

(defun az/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun az/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun az/elfeed-show-economics ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-economics"))
(defun az/elfeed-show-fitness ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-fitness"))

;;makes sure elfeed reads index from disk before launching
(defun az/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun az/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'gfm-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode))

(defun az/dictionary-prompt ()
  (read-string
   (format "Word (%s): " (or (az/region-or-word) ""))
   nil
   nil
   (az/region-or-word)))

(defun az/dictionary-define-word ()
  (interactive)
  (let* ((word (az/dictionary-prompt))
         (buffer-name (concat "Definition: " word)))
    (with-output-to-temp-buffer buffer-name
      (shell-command (format "sdcv -n %s" word) buffer-name))))

(define-key global-map (kbd "C-x w") 'az/dictionary-define-word)

(use-package synosaurus)
(setq-default synosaurus-backend 'synosaurus-backend-wordnet)
(add-hook 'after-init-hook #'synosaurus-mode)
(define-key global-map "\C-xs" 'synosaurus-lookup)

(use-package markdown-mode
  :commands gfm-mode

  :mode (("\\.md$" . gfm-mode))

  :config
  (setq markdown-command "pandoc --standalone --mathjax --from=markdown")
  (custom-set-faces
   '(markdown-code-face ((t nil)))))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package deft
  :bind ("C-c n" . deft)
  :commands (deft)
  :config

  (setq deft-directory "~/Dropbox/notes"
        deft-recursive t
        deft-use-filename-as-title t))

(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("pdf" . "/Applications/Adobe\ Acrobat\ Reader\ DC.app/Contents/MacOS/AdobeReader")
          ("mkv" . "vlc")
          ("mp3" . "vlc")
          ("mp4" . "vlc")
          ("avi" . "vlc"))))

(setq-default dired-listing-switches "-lhvA")

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

(defun dired-xdg-open ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(define-key dired-mode-map (kbd "C-c C-o") 'dired-xdg-open)

(defun az/visit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/configuration.org"))

(global-set-key (kbd "C-c e") 'az/visit-emacs-config)

(defun az/visit-work-notes ()
  (interactive)
  (find-file "~/Dropbox/org/work/notes.org"))

(global-set-key (kbd "C-c w") 'az/visit-work-notes)

(global-set-key (kbd "C-x k") 'az/kill-current-buffer)

(use-package helpful)

(global-set-key (kbd "C-? f") #'helpful-callable)
(global-set-key (kbd "C-? v") #'helpful-variable)
(global-set-key (kbd "C-? k") #'helpful-key)

(az/append-to-path "/usr/local/bin")
(az/append-to-path "~/.local/bin")
(az/append-to-path "~/.fzf/bin")

(save-place-mode t)

(setq-default indent-tabs-mode nil)

(use-package which-key
  :config (which-key-mode))

(use-package yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(setq yas/indent-line nil)

(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'counsel-grep-or-swiper)
  ("C-c C-r" . 'ivy-resume)
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("C-c j" . 'counsel-git-grep)
  ("C-c k" . 'counsel-rg)
  ("C-x l" . 'counsel-locate))

  (setq enable-recursive-minibuffers t)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  :config
  (use-package flx)
  (use-package smex)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

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

(use-package wgrep)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(setq wgrep-auto-save-buffer t)

(projectile-global-mode)

(use-package engine-mode)
(require 'engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine youtube
  "https://www.youtube.com/results?search_query=%s")

(engine-mode t)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'other-window)

(define-key input-decode-map "\e[1;2A" [S-up])

;; Misc keybindigns
(global-set-key (kbd "C-c b") 'comment-box)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Have to unset these otherwise it doesn't work
(global-unset-key "\C-?")
(global-unset-key "\M-h")
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "DEL") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; proper modifier key
(setq mac-option-modifier 'meta)

;; browse-kill-ring
(use-package browse-kill-ring
	      :config
	      (global-set-key "\M-y" 'browse-kill-ring))

;; avy
(use-package avy
	      :bind
	      ("C-;" . avy-goto-char)
	      ("C-'" . avy-goto-char-2)
	      ("M-g f" . avy-goto-line)
	      ("M-s" . avy-goto-word-1))

;; ace-window
(use-package ace-window
	      :bind ("M-p" . ace-window))

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Eyebrowse
(use-package eyebrowse
 :config
 (eyebrowse-mode t))

(defun az/newline-and-indent-same-level ()
"Insert a newline, then indent to the same column as the current line."
(interactive)
(let ((col (save-excursion
             (back-to-indentation)
             (current-column))))
  (newline)
  (indent-to-column col)))

(global-set-key (kbd "RET") 'az/newline-and-indent-same-level)

;; Better than zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

;; for pdf-tools
(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig/")

(use-package prescient)
(use-package ivy-prescient)
(use-package company-prescient)

(defun insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 6.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let (($charset "1234567890" )
        ($baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 6 ))
      (insert (elt $charset (random $baseCount))))))
