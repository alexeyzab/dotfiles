(package-initialize)
;; I like to see an outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

;; Set transient mode everywhere
(transient-mark-mode t)

;; I like seeing a little downward-pointing arrow instead of the usual ellipsis
;; (=...=) that org displays when there's stuff under a header.
(setq org-ellipsis "⤵")

;; Use syntax highlighting in source blocks while editing.
(setq org-src-fontify-natively t)

;; Make TAB act as if it were issued in a buffer of the language's major mode.
(setq org-src-tab-acts-natively t)

;; When editing a code snippet, use the current window rather than popping open a
;; new one (which shows the same information).
(setq org-src-window-setup 'current-window)

;; Quickly insert a block of elisp:
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; Enable spell-checking in Org-mode.
(add-hook 'org-mode-hook 'flyspell-mode)

;; Store my org files in =~/Dropbox/org=, maintain a separate inbox Dropbox, define the location
;; of an index file (my main todo list), and archive finished tasks in
;; =~/Dropbox/org/archive.org=.
(setq org-directory "~/Dropbox/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

;; MobileOrg setup.
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/index.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Hide :PROPERTIES:.

(defun lawlist-org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
                    (if (eq state 'children)
                        (save-excursion (outline-next-heading) (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$" end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$")
              (let ((b (match-end 0)))
                (if (re-search-forward
                     "^[ \t]*:END:"
                     (save-excursion (outline-next-heading) (point)) t)
                    (outline-flag-region b (point-at-eol) t)
                  (user-error ":END: line missing at position %s" b))))))))))

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; I store all my todos in =~/Dropbox/org/index.org=, so I'd like to derive my agenda from
;; there.
(setq org-agenda-files (list org-index-file))

;; Hitting =C-c C-x C-s= will mark a todo as done and move it to an appropriate
;; place in the archive.
(defun az/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'az/mark-done-and-archive)

;; Record the time that a todo was archived.
(setq org-log-done 'time)


;; Define a few common tasks as capture templates. Specifically, I frequently:

;; - Record ideas for future blog posts in =~/Dropbox/org/blog-ideas.org=,
;; - Maintain a todo list in =~/Dropbox/org/index.org=.
(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* %?\n")

        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\nCREATED: %u\n")))


;; Bind a few handy keys.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)


(defun az/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

;; Hit =C-c i= to quickly open up my todo list.
(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (az/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

;; Org-capture for projectile
(use-package org-projectile
  :bind ("C-c n p" . org-projectile-project-todo-completing-read)
  :config
  (progn
    (setq org-projectile-projects-file
          "~/Dropbox/org/todo-projectile.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

;; Open project-specific todo list
(setq org-project-todo (org-file-path "todo-projectile.org"))

(defun open-project-todo ()
  (interactive)
  (find-file org-project-todo)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c o") 'open-project-todo)

;; Hit =M-n= to quickly open up a capture template for a new todo.
(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

;; Allow export to markdown and beamer (for presentations).
(require 'ox-md)
(require 'ox-beamer)

;; Allow =babel= to evaluate Emacs lisp, Ruby, dot, or Gnuplot code.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

;; Don't ask before evaluating code blocks.
(setq org-confirm-babel-evaluate nil)

;; Associate the "dot" language with the =graphviz-dot= major mode.
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Translate regular ol' straight quotes to typographically-correct curly quotes
;; when exporting.
(setq org-export-with-smart-quotes t)


;; Don't include a footer with my contact and publishing information at the bottom
;; of every exported HTML document.
(setq org-html-postamble nil)

;; Exporting to HTML and opening the results triggers =/usr/bin/sensible-browser=,
;; which checks the =$BROWSER= environment variable to choose the right browser.
;; I'd like to always use Chrome, so:
(setenv "BROWSER" "chrome")

;; I want to produce PDFs with syntax highlighting in the code. The best way to do
;; that seems to be with the =minted= package, but that package shells out to
;; =pygments= to do the actual work. =pdflatex= usually disallows shell commands;
;; this enables that.
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Include the =minted= package in all of my LaTeX exports.
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; Automatically parse the file after loading it.
(setq TeX-parse-self t)

;; Always use =pdflatex= when compiling LaTeX documents. I don't really have any
;; use for DVIs.
(setq TeX-PDF-mode t)

;; Open compiled PDFs in =evince= instead of in the editor.
(add-hook 'org-mode-hook
          '(lambda ()
             (delete '("\\.pdf\\'" . default) org-file-apps)
             (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))
